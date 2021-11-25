# load libraries and functions -------------------------------------------------
library(sf)
# library(lwgeom)
library(dplyr)
library(RSQLite)
library(tidyr)
library(transport)
library(reshape2) # for acast

# walk = 1
# bike = 2
# car  = 3
# pt   = 4

jobTypes <- c(
  "Agriculture, Forestry and Fishing",
  "Mining",
  "Manufacturing",
  "Electricity, Gas, Water and Waste Services",
  "Construction",
  "Wholesale Trade",
  "Retail Trade",
  "Accommodation and Food Services",
  "Transport, Postal and Warehousing",
  "Information Media and Telecommunications",
  "Financial and Insurance Services",
  "Rental, Hiring and Real Estate Services",
  "Professional, Scientific and Technical Services Administrative and Support Services",
  "Public Administration and Safety",
  "Education and Training",
  "Health Care and Social Assistance",
  "Arts and Recreation Services",
  "Other Services"
)

# for a given job type, calculate an optimised flow model between the origins
# (SA1 regions) and destinations (DZN zones).
calculateFlow <- function(jobType,costMatrix)  {
  # jobType <- jobTypes[1]

  originIds <- data.frame(SA1_MAINCO=as.numeric(dimnames(costMatrix)[[1]])) %>%
    mutate(index=row_number()) %>%
    left_join(origins %>%
                filter(ANZSIC==jobType) %>%
                dplyr::select(SA1_MAINCO,count=Total),
              by=c("SA1_MAINCO"="SA1_MAINCO")) %>%
    replace_na(list(count=0))
  
  destinationIds <- data.frame(DZN=as.numeric(dimnames(costMatrix)[[2]])) %>%
    mutate(index=row_number()) %>%
    left_join(destinations_proportional %>%
                filter(ANZSIC==jobType) %>%
                dplyr::select(DZN,count=Total),
              by=c("DZN"="DZN")) %>%
    replace_na(list(count=0)) %>%
    group_by(DZN,index) %>%
    # there are some double DZN ids ...
    summarise(count=sum(count,na.rm=T)) %>%
    # ALWAYS UNGROUP
    ungroup()
  
  # make sure the sum of the origins is the same as the sum of the destinations
  destinations_proportion<-sum(originIds$count,na.rm=T)/sum(destinationIds$count,na.rm=T)
  destinationIds <- destinationIds %>%
    mutate(count=count*destinations_proportion)
  
  # remove origins and destinations with 0 count (people,jobs)
  costMatrixLocal <- costMatrix[which(originIds$count>0),which(destinationIds$count>0)]
  
  originIds <- originIds %>%
    filter(count>0) %>%
    mutate(index=row_number())
  destinationIds <- destinationIds %>%
    filter(count>0) %>%
    mutate(index=row_number())
  
  
  transportCalc <- transport(originIds$count,destinationIds$count,
                             costm=costMatrixLocal,
                             method="networkflow") %>%
    inner_join(originIds,by=c("from"="index")) %>%
    inner_join(destinationIds,by=c("to"="index")) %>%
    dplyr::select(origin=SA1_MAINCO,destination=DZN,count=mass) %>%
    arrange(origin,destination,count)
  return(transportCalc)
}

calculateFlowMode <- function(compound_id,mode) {
  # compound_id=4 # Canberra
  # mode=1 
  con <- dbConnect(SQLite(), dbname=paste0("./data-output/OTP_cleaned/region",formatC(compound_id,width=2,flag="0"),".db"))
  cost <- dbGetQuery(con, paste0("SELECT origin, destination, time_secs FROM od_modes_0745 WHERE mode = ",mode)) %>%
    mutate(origin=as.numeric(origin))
  costMatrix <- cost %>%
    acast(origin~destination, value.var="time_secs", fill=36000)
  dbDisconnect(con)
  
  flowRaw <- NULL
  for(i in jobTypes) {
    currentJobFlow <- calculateFlow(i,costMatrix) 
    flowRaw <- bind_rows(flowRaw,currentJobFlow)
  }
  flowRaw2 <- flowRaw %>%
    group_by(origin,destination) %>%
    summarise(count=sum(count,na.rm=T)) %>%
    ungroup() %>%
    mutate(compound_id=compound_id,
           mode=mode) %>%
    dplyr::select(compound_id,mode,origin,destination,count) %>%
    left_join(cost, by=c("origin","destination")) %>%
    mutate(time_secs=replace_na(time_secs,36000))
  return(flowRaw2)
}



# loading data ------------------------------------------------------------

# Add all SA1 regions to single table
SA1_files <- list.files(path = "data-output/OD_coordinates", pattern="sa1_2016_network_snapped_pwc_region*")
SA1_table <- NULL # X, Y, fid, SA1_MAINCO, SA1_7DIGIT, COMPOUND_ID
for (i in SA1_files) {
  SA1_temp <- read.csv(paste0("data-output/OD_coordinates/",i))
  SA1_table <- bind_rows(SA1_table,SA1_temp)
}

# Add all DZNs to single table
DZN_files <- list.files(path = "data-output/OD_coordinates", pattern="dzn_2016_network_snapped_centroids_region*")
DZN_table <- NULL # X, Y, fid, DZN_CODE_2016, COMPOUND_ID
for (i in DZN_files) {
  DZN_temp <- read.csv(paste0("data-output/OD_coordinates/",i))
  DZN_table <- bind_rows(DZN_table,DZN_temp)
}

# just the SA1 regions that are used in this project
SA1_geom <- st_read("data-input/ABS/SA1_2016_AUST.shp") %>%
  dplyr::select(SA1_MAINCO=SA1_MAIN16)  %>%
  mutate(SA1_MAINCO=as.numeric(as.character(SA1_MAINCO))) %>%
  filter(SA1_MAINCO %in% SA1_table$SA1_MAINCO)

# need to match the 7digit code with the full code
SA1_mapping <- SA1_table %>%
  dplyr::select(SA1_MAINCO,SA1_7DIGIT) %>%
  mutate(SA1_MAINCO=as.numeric(as.character(SA1_MAINCO)),
         SA1_7DIGIT=as.numeric(as.character(SA1_7DIGIT)))

# read in census data, add in the full SA1 code, keep only records within
# compound study region
origins <- read.csv("data-input/census/SA1_to_work.csv",as.is=T,fileEncoding="UTF-8-BOM")
colnames(origins) <- c("SA1","ANZSIC","PT","CAR","BIKE","WALK","Total","etc")
origins <- origins %>%
  mutate(SA1=as.character(SA1)) %>%
  dplyr::select(SA1,ANZSIC,PT,CAR,BIKE,WALK,Total) %>%
  mutate(SA1=ifelse(SA1=="",NA,SA1)) %>%
  mutate(SA1=as.numeric(SA1)) %>%
  fill(SA1) %>%
  filter(SA1 %in% SA1_table$SA1_7DIGIT & ANZSIC %in% jobTypes) %>%
  left_join(SA1_table, by=c("SA1"="SA1_7DIGIT")) %>%
  dplyr::select(COMPOUND_ID,SA1_MAINCO,ANZSIC,PT,CAR,BIKE,WALK,Total) %>%
  # the modes don't necessarily add up to the total...
  mutate(Total=PT+CAR+BIKE+WALK)

destinations <- read.csv("data-input/census/DZN_from_work.csv")
colnames(destinations) <- c("DZN","ANZSIC","PT","CAR","BIKE","WALK","Total","etc")
destinations <- destinations %>%
  mutate(DZN=as.character(DZN)) %>%
  dplyr::select(DZN,ANZSIC,PT,CAR,BIKE,WALK,Total) %>%
  mutate(DZN=ifelse(DZN=="",NA,DZN)) %>%
  mutate(DZN=as.numeric(DZN)) %>%
  fill(DZN) %>%
  filter(DZN %in% DZN_table$DZN_CODE_2016 & ANZSIC %in% jobTypes) %>%
  left_join(DZN_table, by=c("DZN"="DZN_CODE_2016")) %>%
  dplyr::select(COMPOUND_ID,DZN,ANZSIC,PT,CAR,BIKE,WALK,Total) %>%
  # the modes don't necessarily add up to the total...
  mutate(Total=PT+CAR+BIKE+WALK)

total_origin <- origins %>%
  group_by(COMPOUND_ID,ANZSIC) %>%
  summarise(origin_count=sum(Total,na.rm=T))

total_destination <- destinations %>%
  group_by(COMPOUND_ID,ANZSIC) %>%
  summarise(destination_count=sum(Total,na.rm=T))

od_diff <- inner_join(total_origin,total_destination,by=c("COMPOUND_ID","ANZSIC")) %>%
  mutate(proportion=origin_count/destination_count) %>%
  dplyr::select(COMPOUND_ID,ANZSIC,proportion)

destinations_proportional <- destinations %>%
  inner_join(od_diff,by=c("COMPOUND_ID","ANZSIC")) %>%
  mutate(Total=Total*proportion) %>%
  dplyr::select(COMPOUND_ID,DZN,ANZSIC,Total)



# calculate flows ---------------------------------------------------------
flowAllComplete <- NULL

print(paste0(" 0 out of 13 regions completed at ",Sys.time()))
for (compound_id in 1:13) {
  for (mode in 1:4) {
    flowAllComplete <- bind_rows(
      flowAllComplete,
      suppressMessages(calculateFlowMode(compound_id,mode)))
  }
  print(paste0(formatC(compound_id,width=2)," out of 13 regions completed at ",Sys.time()))
  flush.console()
}
# [1] " 0 out of 13 regions completed at 2020-09-24 18:48:01"
# [1] " 1 out of 13 regions completed at 2020-09-24 18:49:14"
# [1] " 2 out of 13 regions completed at 2020-09-24 18:55:33"
# [1] " 3 out of 13 regions completed at 2020-09-24 18:55:37"
# [1] " 4 out of 13 regions completed at 2020-09-24 18:55:42"
# [1] " 5 out of 13 regions completed at 2020-09-24 18:55:45"
# [1] " 6 out of 13 regions completed at 2020-09-24 18:55:48"
# [1] " 7 out of 13 regions completed at 2020-09-24 18:55:51"
# [1] " 8 out of 13 regions completed at 2020-09-24 18:55:54"
# [1] " 9 out of 13 regions completed at 2020-09-24 18:55:57"
# [1] "10 out of 13 regions completed at 2020-09-24 19:02:06"
# [1] "11 out of 13 regions completed at 2020-09-24 19:02:27"
# [1] "12 out of 13 regions completed at 2020-09-24 19:02:30"
# [1] "13 out of 13 regions completed at 2020-09-24 19:03:04"

write.csv(flowAllComplete,gzfile("./data-output/flowAll.csv.gz"), row.names=F)


