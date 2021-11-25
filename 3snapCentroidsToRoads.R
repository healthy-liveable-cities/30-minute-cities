library(sf)
library(lwgeom)
library(dplyr)
# library(RSQLite)
# library(tidyr)

###############################################################################
### snapping                                                                ###
###############################################################################


# read in the regions from the last section
study_regions <- st_read("data-output/30minuteCities.gpkg",
                         layer="study_regions")
compound_study_regions <- st_read("data-output/30minuteCities.gpkg",
                                  layer="compound_study_regions")
SA1_centroids <- st_read("data-output/30minuteCities.gpkg",
                         layer="SA1_centroids")
DZN_centroids <- st_read("data-output/30minuteCities.gpkg",
                         layer="DZN_centroids")

compound_ids <- unique(compound_study_regions$COMPOUND_ID)

for (i in compound_ids) {
  roads_endpoints <- st_read(
    paste0("data-output/osm_data/30minute_cities_",i,".sqlite")) %>%
    # st_transform(7845) %>%
    # st_snap_to_grid(0.1) %>%
    st_endpoint() %>%
    st_coordinates() %>%
    as.data.frame() %>%
    distinct() %>%
    st_as_sf(coords=c("X","Y"),crs=4326) %>%
    mutate(id=row_number())
  
  region_sa1 <- SA1_centroids %>%
    filter(COMPOUND_ID==i) %>%
    st_transform(4326)
  nearest_road <- st_nearest_feature(region_sa1,roads_endpoints)
  region_sa1 <- region_sa1 %>%
    st_drop_geometry() %>%
    mutate(id=nearest_road) %>%
    left_join(roads_endpoints, by=c("id"="id")) %>%
    st_sf() %>%
    mutate(fid=row_number()) %>%
    st_transform(4326)
  region_sa1 <- cbind(region_sa1,st_coordinates(region_sa1)) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(Y,X,fid,SA1_MAINCO,SA1_7DIGIT,COMPOUND_ID)
  write.table(region_sa1, file=paste0("data-output/OD_coordinates/sa1_2016_network_snapped_pwc_region",i,".csv"),row.names=FALSE,append=FALSE,sep=",")
  
  region_dzn <- DZN_centroids %>%
    filter(COMPOUND_ID==i) %>%
    st_transform(4326)
  nearest_road2 <- st_nearest_feature(region_dzn,roads_endpoints)
  region_dzn <- region_dzn %>%
    st_drop_geometry() %>%
    mutate(id=nearest_road2) %>%
    left_join(roads_endpoints, by=c("id"="id")) %>%
    st_sf() %>%
    mutate(fid=row_number()) %>%
    st_transform(4326)
  region_dzn <- cbind(region_dzn,st_coordinates(region_dzn)) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(Y,X,fid,DZN_CODE_2016,COMPOUND_ID)
  write.table(region_dzn, file=paste0("data-output/OD_coordinates/dzn_2016_network_snapped_centroids_region",i,".csv"),row.names=FALSE,append=FALSE,sep=",")
}
