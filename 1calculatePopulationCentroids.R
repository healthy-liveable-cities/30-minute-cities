library(sf)
library(lwgeom)
library(dplyr)
library(RSQLite)
library(tidyr)

###############################################################################
### study regions                                                           ###
###############################################################################
study_regions <- st_read("data-input/ntnl_li_australia_study_regions_epsg7845.sqlite") %>%
  filter(!is.na(li_inds_region_dwelling_study_region)) %>%
  dplyr::select(locale,name,region,state) %>%
  st_set_crs(7845) %>% # import preserves the projection, but not the epsg code
  st_snap_to_grid(0.1)

# compound study regions are the individual polygons created after applying a 
# 40km buffer to the study regions (i.e the max distance reachable within 30
# minutes of a car traveling at 80km/h). Any sources or destinations outside
# of this are not considered.
compound_study_regions <- study_regions %>%
  st_geometry() %>%
  st_union() %>%
  st_buffer((40000)) %>% # 40km buffer
  st_cast(to="POLYGON") %>%
  st_sf() %>%
  mutate(COMPOUND_ID=row_number()) %>%
  dplyr::select(COMPOUND_ID,geometry)
st_write(compound_study_regions, "data-output/compoundStudyRegions.sqlite",
         delete_layer=TRUE)
 

# what study regions are in each of the compound study regions
study_regions_intersect <- st_intersects(study_regions,compound_study_regions,
                                         prepared=TRUE,sparse=TRUE) %>%
  lapply(first) %>%
  unlist()

# assign compound study region id to each region
study_regions <- study_regions %>%
  mutate(compound_id=study_regions_intersect) %>%
  filter(!is.na(compound_id)) %>%
  dplyr::select(locale,name,region,state,compound_id)
st_write(study_regions, "data-output/studyRegions.sqlite", delete_layer=TRUE)


###############################################################################
### centroids                                                               ###
###############################################################################

# only want meshblocks with people, dwellings, and an IRSD score
populated_meshblocks <- read.csv(gzfile("data-input/ABS/populated_meshblocks.csv.gz")) %>%
  pull(mb_code_2016)

# write.csv(population,gzfile("data-input/ABS/2016 census mesh block counts.csv.gz"),row.names = F)
# write.csv(populated_meshblocks,gzfile("data-input/ABS/populated_meshblocks.csv.gz"),row.names = F)

# find all populated meshblocks
MB <- st_read("data-input/ABS/ASGS_2016_Volume_1_GDA2020/main_MB_2016_AUST_FULL.shp") %>%
  # dplyr::select(MB_CODE_20,SA1_MAINCO,SA1_7DIGIT,GCCSA_NAME) %>%
  filter(!st_is_empty(.)) %>% # some meshblocks don't have a location
  mutate(MB_CODE_20=as.numeric(as.character(MB_CODE_20)), # must be numbers
         SA1_MAINCO=as.numeric(as.character(SA1_MAINCO)),
         SA1_7DIGIT=as.numeric(as.character(SA1_7DIGIT))) %>%
  filter(MB_CODE_20 %in% populated_meshblocks) %>%
  st_transform(7845) %>%
  st_snap_to_grid(0.1)

# need population numbers for meshblocks
population <- read.csv(gzfile("data-input/ABS/2016 census mesh block counts.csv.gz")) %>%
  dplyr::select(MB_CODE_2016,Person,Dwelling) %>%
  filter(!is.na(Person) & Person > 0 & Dwelling > 0) %>%
  mutate(MB_CODE_2016=as.numeric(as.character(MB_CODE_2016)))
MB_population <- MB %>%
  inner_join(population, by=c("MB_CODE_20"="MB_CODE_2016"))

# Calculate the centroid coordinates for each populated meshblock
MB_centroid <- cbind(MB_population,
                     st_coordinates(st_centroid(MB_population))) %>%
  st_set_geometry(NULL)

# calculate the population-weighted centroids for SA1
SA1centroids <- MB_centroid %>%
  group_by(SA1_MAINCO) %>%
  # needed this when we kept in meshblocks with no people
  # mutate(Person=Person+0.000001) %>%
  summarise(SA1_7DIGIT=first(SA1_7DIGIT),
            GCCSA_NAME=first(GCCSA_NAME),
            Population=sum(Person),
            Dwelling=sum(Dwelling),
            X=weighted.mean(X,Person,na.rm=TRUE),
            Y=weighted.mean(Y,Person,na.rm=TRUE)
  ) %>%
  ungroup() %>%
  filter(Population >= 10 & Dwelling >= 5) %>%
  st_as_sf(coords = c("X", "Y"), crs = 7845) %>%
  st_snap_to_grid(0.1)

# SA1 centroids that intersect the compound study regions
SA1centroids_intersect <- st_intersects(SA1centroids,compound_study_regions,
                                        prepared=TRUE,sparse=TRUE) %>%
  lapply(first) %>%
  unlist()
SA1centroids_filtered <- SA1centroids %>%
  mutate(COMPOUND_ID=SA1centroids_intersect) %>%
  filter(!is.na(COMPOUND_ID)) %>%
  dplyr::select(SA1_MAINCO,SA1_7DIGIT,COMPOUND_ID,geometry)

# calculate the centroids of the DZN regions
DZNcentroids <- st_read("data-input/ABS/ASGS 2016 DZN.gpkg") %>%
  dplyr::select(DZN_CODE_2016,geom) %>%
  filter(!st_is_empty(.)) %>%
  st_transform(7845) %>%
  st_snap_to_grid(0.1) %>%
  st_centroid() %>%
  st_snap_to_grid(0.1)

# DZN centroids that intersect the compound study regions
DZNcentroids_intersect <- st_intersects(DZNcentroids,compound_study_regions,
                                        prepared=TRUE,sparse=TRUE) %>%
  lapply(first) %>%
  unlist()
DZNcentroids_filtered <- DZNcentroids %>%
  mutate(COMPOUND_ID=DZNcentroids_intersect) %>%
  filter(!is.na(COMPOUND_ID)) %>%
  dplyr::select(DZN_CODE_2016,COMPOUND_ID,geom)


# write all of these to a geopackage
st_write(study_regions, "data-output/30minuteCities.gpkg",delete_layer=TRUE,
         layer_options=c("OVERWRITE=yes"),layer="study_regions")
st_write(compound_study_regions, "data-output/30minuteCities.gpkg",
         layer_options=c("OVERWRITE=yes"),layer="compound_study_regions")
st_write(SA1centroids_filtered, "data-output/30minuteCities.gpkg",
         layer_options=c("OVERWRITE=yes"),layer="SA1_centroids")
st_write(DZNcentroids_filtered, "data-output/30minuteCities.gpkg",
         layer_options=c("OVERWRITE=yes"),layer="DZN_centroids")
