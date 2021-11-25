# functions and city labels -----------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

aggregateData <-function(df,binwidth) {
  df <- df %>%
    mutate(time=findInterval(time,seq(0,180,binwidth))) %>%
    mutate(time=time*binwidth-(binwidth/2)) %>%
    group_by(city,mode,time) %>%
    summarise(count=sum(count,na.rm=F))
  # add Australia
  df <- bind_rows(
    df %>%
      group_by(mode,time) %>%
      summarise(count=sum(count,na.rm=F)) %>%
      mutate(city="Australia") %>%
      dplyr::select(city,mode,time,count),
    df) %>%
    mutate(city=factor(city,levels=city_labels,labels=city_labels))
  return(df)
}

plotData <- function(df,binwidth) {
  g <- ggplot(df,
              aes(x=time,y=count)) +
    geom_vline(xintercept=30,color="grey70",size=0.3) +
    geom_col(width=binwidth,position=position_nudge(x=0),fill="gray5") +
    labs(x = "Time traveled to work (minutes)", y=NULL) +
    facet_grid(cols=vars(mode),rows=vars(city), scales = "free") +
    scale_x_continuous(limits=c(0,180),breaks=seq(0,180,60)) +
    scale_y_continuous(
      name = waiver(),
      breaks = waiver(),
      minor_breaks = NULL,
      n.breaks = 3,
      labels = waiver()) +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      axis.text.y = element_blank(),
      strip.text = element_text(size = 10),
      strip.text.y = element_text(angle=0, hjust=0),
      axis.ticks = element_line(size=0.2),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_line(size=0.2),
      panel.grid.minor = element_blank())
  return(g)
}

countData <-function(df) {
  df <- df %>%
    filter(time<=30) %>%
    group_by(city,mode) %>%
    summarise(count=sum(count,na.rm=T))
  # add Australia
  df <- bind_rows(
    df %>%
      group_by(mode) %>%
      summarise(count=sum(count,na.rm=F)) %>%
      mutate(city="Australia") %>%
      dplyr::select(city,mode,count),
    df) %>%
    mutate(city=factor(city,levels=city_labels,labels=city_labels))
  return(df)
}

funCar <- function(d) {
  # capping a max speed of 100km/h
  ifelse((0.697*d + 20.165)>100,
         return(d/100),
         return(d / (0.697*d + 20.165)) )
}
funCar <- function(d) {
  # capping a max speed of 100km/h
  if((0.697*d + 20.165)>100) return(d/80)
  return(d / (0.697*d + 20.165))
}
funPT <- function(d) {
  # capping a max speed of 80km/h
  if((0.509*d + 8.782)>80) return(d/80)
  return(d / (0.509*d + 8.782))
}
funWalk <- function(d) {d / (4.539)}
funCycle <- function(d) {d / (13.099)}

city_levels=c("Australia","Sydney","Melbourne","Brisbane","Perth","Adelaide","Canberra",
              "Hobart","Darwin","Gold_Coast","Newcastle","Sunshine_Coast",
              "Wollongong","Geelong","Townsville","Cairns","Toowoomba",
              "Ballarat","Bendigo","Mackay","Launceston","Albury_Wodonga")

city_levels2=c("Australia","Sydney","Melbourne","Brisbane","Perth","Adelaide","Canberra",
               "Hobart","Darwin","Gold Coast - Tweed Heads","Newcastle - Maitland","Sunshine Coast",
               "Wollongong","Geelong","Townsville","Cairns","Toowoomba",
               "Ballarat","Bendigo","Mackay","Launceston","Albury - Wodonga")

city_labels=c("Australia","Sydney","Melbourne","Brisbane","Perth","Adelaide","Canberra",
              "Hobart","Darwin","Gold Coast","Newcastle","Sunshine Coast",
              "Wollongong","Geelong","Townsville","Cairns","Toowoomba",
              "Ballarat","Bendigo","Mackay","Launceston","Albury Wodonga")

city_labels_reordered=c("Australia","Sydney","Melbourne","Brisbane","Perth","Adelaide",
                        "Gold Coast","Canberra","Newcastle","Wollongong","Sunshine Coast",
                        "Geelong","Hobart","Townsville","Darwin","Cairns","Toowoomba",
                        "Ballarat","Bendigo","Albury Wodonga","Launceston","Mackay")
# loading census distances ------------------------------------------------
ranges_df <- data.frame(range=c(
  "0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-4","4-5","5-6","6-7","7-8",
  "8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18",
  "18-19","19-20","20-21","21-22","22-23","23-24","24-25","25-26","26-27",
  "27-28","28-29","29-30","30-32","32-34","34-36","36-38","38-40","40-42",
  "42-44","44-46","46-48","48-50","50-52","52-54","54-56","56-58","58-60",
  "60-62","62-64","64-66","66-68","68-70","70-72","72-74","74-76","76-78",
  "78-80","80-85","85-90","90-95","95-100","100-110","110-120","120-130",
  "130-140","140-150","150-200","200-250","250-300","300-350","350-400",
  "400-600","600-800","800-1000","1000-3000","3000-inf"
),range_value=c(
  0.25,0.75,1.25,1.75,2.25,2.75,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,
  13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5,21.5,22.5,23.5,24.5,25.5,26.5,27.5,
  28.5,29.5,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,
  75,77,79,82.5,87.5,92.5,97.5,105,115,125,135,145,175,225,275,325,375,500,
  700,900,2000,4000    
)
)

census_distances <- read.csv("data-input/census/distanceToWorkByModeByCity.csv") %>%
  mutate(city=as.character(city)) %>%
  mutate(city=ifelse(city=="",NA,city)) %>%
  fill(city) %>%
  mutate(city=factor(city,levels=city_levels,labels=city_labels))

colnames(census_distances) <- 
  c("city","mode","nil","0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-4",
    "4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14",
    "14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23",
    "23-24","24-25","25-26","26-27","27-28","28-29","29-30","30-32","32-34",
    "34-36","36-38","38-40","40-42","42-44","44-46","46-48","48-50","50-52",
    "52-54","54-56","56-58","58-60","60-62","62-64","64-66","66-68","68-70",
    "70-72","72-74","74-76","76-78","78-80","80-85","85-90","90-95","95-100",
    "100-110","110-120","120-130","130-140","140-150","150-200","200-250",
    "250-300","300-350","350-400","400-600","600-800","800-1000",
    "1000-3000","3000-inf","na","x")
census_distances <- census_distances %>%
  dplyr::select(-"nil",-"na",-"x",-"300-350",-"350-400",-"400-600",-"600-800",
                -"800-1000",-"1000-3000",-"3000-inf") %>%
  pivot_longer(cols="0-0.5":"250-300",names_to="range",values_to="count") %>%
  left_join(ranges_df, by="range")

pop_total <- census_distances %>%
  group_by(city) %>%
  summarise(total=sum(count,na.rm=T))
# add Australia
pop_total <- bind_rows(
  data.frame(city="Australia",total=sum(pop_total$total,na.rm=T)),
  pop_total) %>%
  mutate(city=factor(city,levels=city_labels,labels=city_labels))
write.csv(pop_total, "data-output/pop_total.csv", row.names=F)

df=census_distances
range_min=3.5
range_max=29.5
source_spacing=1
target_spacing=0.5
expandRange <- function(df,range_min,range_max,source_spacing,target_spacing=1) {
  offset_value <- (source_spacing/2) - (target_spacing/2)
  df %>%
    filter(range_value>=range_min & range_value<=range_max) %>%
    inner_join(crossing(data.frame(range_value=seq(range_min,range_max,source_spacing)),
                        data.frame(offset=seq(offset_value*-1,offset_value,target_spacing))),
               by="range_value") %>%
    mutate(range_value=range_value+offset) %>%
    mutate(count=count/(source_spacing/target_spacing)) %>%
    dplyr::select(-offset)
}

test <- expandRange(census_distances,3.5,29.5,1,0.5)

census_distances_expanded <- bind_rows(
  expandRange(census_distances,0.25,2.75,0.5,0.25),
  expandRange(census_distances,3.5,29.5,1,0.25),
  expandRange(census_distances,31,79,2,0.25),
  expandRange(census_distances,82.5,97.5,5,0.25),
  expandRange(census_distances,105,145,10,0.25),
  # anything above 300km isn't considered
  expandRange(census_distances,175,275,50,0.25)%>%filter(range_value<=300)
) %>%
  arrange(city,mode,range_value)

census_times <- census_distances_expanded %>%
  dplyr::select(-range) %>%
  pivot_wider(names_from=mode,
              values_from=count) %>%
  distinct() %>%
  mutate(walk_time=funWalk(range_value),
         bike_time=funCycle(range_value),
         pt_time=sapply(range_value,funPT),
         car_time=sapply(range_value,funCar)) %>%
  mutate(walk_time=walk_time*60,
         bike_time=bike_time*60,
         pt_time=pt_time*60,
         car_time=car_time*60)

write.csv(census_times, "data-output/census_times.csv", row.names=F)



# calculate s -------------------------------------------------------------


# calculate SA1 study regions ---------------------------------------------
sa1_ids <- read.csv(gzfile("./data-output/flowAll.csv"),as.is=T) %>%
  pull(origin) %>% unique()

# First filtering to the SA1 regions used by the optimised flow model
SA1 <- st_read("data-input/ABS/SA1_2016_AUST.shp") %>%
  st_set_crs(4283) %>%
  dplyr::select(SA1_MAINCO=SA1_MAIN16,SA1_7DIGIT=SA1_7DIG16,
                SA2_MAINCO=SA2_MAIN16) %>%
  mutate(SA1_MAINCO=as.numeric(as.character(SA1_MAINCO)),
         SA2_MAINCO=as.numeric(as.character(SA2_MAINCO)),
         SA1_7DIGIT=as.numeric(as.character(SA1_7DIGIT))) %>%
  filter(SA1_MAINCO %in% sa1_ids)

# It's much faster to just calculate intersections based on centroids
SA1_centroids <- SA1 %>%
  st_transform(7845) %>%
  st_centroid(.)

SA1_study_regions <- st_join(study_regions, SA1_centroids, prepared=TRUE,
                             sparse=TRUE, left=TRUE) %>%
  st_drop_geometry()
write.csv(SA1_study_regions, "data-output/SA1_study_regions.csv", append=F, row.names=F)

# calculate interventions -------------------------------------------------
census_times <- read.csv("data-output/census_times.csv", as.is=T) %>%
  mutate(city=factor(city,levels=city_labels,labels=city_labels))
pop_total <- read.csv("data-output/pop_total.csv", as.is=T) %>%
  mutate(city=factor(city,levels=city_labels,labels=city_labels))
cities_SA1 <- read.csv("data-output/SA1_study_regions.csv") %>%
  dplyr::select(city=name,SA1_MAINCO) %>%
  mutate(city=factor(city,levels=city_levels2,labels=city_labels))

no_shift <- census_times %>%
  rename(walk_count=WALK,bike_count=BIKE,pt_count=PT,car_count=CAR) %>%
  pivot_longer(cols=bike_count:car_time,
               names_to=c("mode","measure"),
               names_sep="_") %>%
  pivot_wider(id_cols=c("city","range_value","mode"),
              names_from="measure",
              values_from="value") %>%
  filter(time<180) %>%
  mutate(mode=factor(mode,
                     levels=c("walk","bike","pt","car"),
                     labels=c("Walking","Cycling","Public transport","Driving")))

mode_shift <- census_times %>%
  mutate(count=WALK+BIKE+CAR+PT) %>%
  dplyr::select(-WALK,-BIKE,-CAR,-PT) %>%
  rename(walk=walk_time,bike=bike_time,pt=pt_time,car=car_time) %>%
  pivot_longer(cols=walk:car,
               names_to="mode",
               values_to="time") %>%
  filter(time<180) %>%
  mutate(mode=factor(mode,
                     levels=c("walk","bike","pt","car"),
                     labels=c("Walking","Cycling","Public transport","Driving")))

job_shift <- read.csv(gzfile("./data-output/flowAll.csv"),as.is=T) %>%
  inner_join(cities_SA1, by=c("origin"="SA1_MAINCO")) %>%
  mutate(time=time_secs/60) %>%
  dplyr::select(city,mode,origin,destination,count,time) %>%
  mutate(mode=factor(mode,
                     levels=c(1,2,4,3), # walk = 1, bike = 2, car = 3, pt = 4
                     labels=c("Walking","Cycling","Public transport","Driving")))
job_shift_proportion <- job_shift %>%
  group_by(city,mode) %>%
  summarise(jobshift_total=sum(count,na.rm=T)) %>%
  ungroup() %>%
  inner_join(pop_total,by="city") %>%
  mutate(proportion=total/jobshift_total) %>%
  dplyr::select(city,mode,proportion)
job_shift <- job_shift %>%
  inner_join(job_shift_proportion, by=c("city","mode")) %>%
  filter(time<180) %>%
  mutate(count=count*proportion) %>%
  dplyr::select(city,mode,origin,destination,count,time)


# number of commuters within 30 minutes ----------------------------------------
no_shift_count <- countData(no_shift) %>% rename(noshift=count)
mode_shift_count <- countData(mode_shift) %>% rename(modeshift=count)
job_shift_count <- countData(job_shift) %>% rename(jobshift=count)

summaryTable <- pop_total %>% 
  full_join(no_shift_count, by="city") %>%
  full_join(mode_shift_count, by=c("city","mode")) %>%
  full_join(job_shift_count, by=c("city","mode")) %>%
  mutate(noshift_percent=round(noshift/total*100,2),
         modeshift_percent=round(modeshift/total*100,2),
         jobshift_percent=round(jobshift/total*100,2)) %>%
  # mutate(noshift_percent=ifelse(noshift_percent>100,100,noshift_percent),
  #        modeshift_percent=ifelse(modeshift_percent>100,100,modeshift_percent),
  #        jobshift_percent=ifelse(jobshift_percent>100,100,jobshift_percent)) %>%
  rename(population=total)
write.csv(summaryTable, "results/summaryTable.csv", row.names=F)

summaryTableOutput <- summaryTable %>%
  arrange(-population) %>%
  dplyr::select(city,mode,baseline=noshift_percent,modeshift=modeshift_percent,jobshift=jobshift_percent) %>%
  pivot_wider(names_from=mode,
              values_from=c(baseline,modeshift,jobshift)) %>%
  dplyr::select(city,baseline_Walking,modeshift_Walking,jobshift_Walking,
                baseline_Cycling,modeshift_Cycling,jobshift_Cycling,
                "baseline_Public transport","modeshift_Public transport","jobshift_Public transport",
                baseline_Driving,modeshift_Driving,jobshift_Driving
                )
write.csv(summaryTableOutput, "results/summaryTableOutput.csv", row.names=F)

# plot interventions ------------------------------------------------------
bandwidth=10
no_shift_aggregated <- aggregateData(no_shift,bandwidth)
mode_shift_aggregated <- aggregateData(mode_shift,bandwidth)
job_shift_aggregated <- aggregateData(job_shift,bandwidth)


plotData(no_shift_aggregated,bandwidth)
ggsave(paste0("results/noshift_",bandwidth,".pdf"), width=6, height=9, units = "in")
plotData(mode_shift_aggregated,bandwidth)
ggsave(paste0("results/modeshift_",bandwidth,".pdf"), width=6, height=9, units = "in")
plotData(job_shift_aggregated,bandwidth)
ggsave(paste0("results/jobshift_",bandwidth,".pdf"), width=6, height=9, units = "in")




# plot Australia ---------------------------------------------------------------
bw=10
australia <- bind_rows(
  aggregateData(no_shift,bw) %>% filter(city=="Australia") %>% mutate(intervention="noshift"),
  aggregateData(mode_shift,bw) %>% filter(city=="Australia") %>% mutate(intervention="modeshift"),
  aggregateData(job_shift,bw) %>% filter(city=="Australia") %>% mutate(intervention="jobshift")
) %>%
  mutate(intervention=factor(intervention,
                     levels=c("noshift","modeshift","jobshift"),
                     labels=c("Baseline (a)","Mode shift (b)","Job-worker shift (c)"))) %>%
  group_by(intervention,mode,time) %>%
  summarise(count=sum(count,na.rm=F)) %>%
  ungroup()


  
library(scales)

australia_tmp <- bind_rows(
  australia,
  australia%>%dplyr::select(intervention,mode)%>%distinct()%>%mutate(time=4.99,count=0),
  australia%>%filter(time==175)%>%mutate(time=184.99),
  australia%>%dplyr::select(intervention,mode)%>%distinct()%>%mutate(time=185,count=0),
) %>% mutate(time=time-5)

ggplot(australia_tmp, aes(x=time,y=count,color=intervention)) +
  facet_grid(cols=vars(mode), scales = "free") +
  geom_hline(yintercept=1500000,color="white",alpha=0,size=0) +
  geom_step() +
  geom_vline(xintercept=30,color="black",size=0.3) +
  labs(x = "Time traveled to work (minutes)", y="Commuters") +
  scale_x_continuous(limits=c(-0.01,180),breaks=c(0,30,60,120,180)) +
  # scale_y_continuous(limits=c(0,4000000),breaks=seq(0,4000000,1000000),labels = comma) +
  scale_y_continuous(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = NULL,
    n.breaks = 4,
    labels = comma) +
  theme(
    legend.title=element_blank(),
    legend.position="bottom",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-5,0,5,0),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 8),
    # axis.text.y = element_blank(),
    strip.text = element_text(size = 10),
    # strip.text.y = element_text(angle=0, hjust=0),
    axis.ticks = element_line(size=0.2),
    # axis.ticks.y = element_blank(),
    panel.grid.major = element_line(size=0.2),
    panel.grid.minor = element_blank())
ggsave("results/interventions_time_distribution2.pdf",width=8,height=4)

ggplot(australia, aes(x=time,y=count)) +
  facet_grid(rows=vars(intervention),cols=vars(mode), scales = "free") +
  geom_hline(yintercept=1500000,color="white",alpha=0,size=0) +
  geom_col(width=bw,fill="gray50") +
  geom_vline(xintercept=30,color="black",size=0.3) +
  labs(x = "Time traveled to work (minutes)", y="Commuters") +
  scale_x_continuous(limits=c(0,180),breaks=c(0,30,60,120,180)) +
  # scale_y_continuous(limits=c(0,4000000),breaks=seq(0,4000000,1000000),labels = comma) +
  scale_y_continuous(
    name = waiver(),
    breaks = waiver(),
    minor_breaks = NULL,
    n.breaks = 4,
    labels = comma) +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 8),
    # axis.text.y = element_blank(),
    strip.text = element_text(size = 10),
    # strip.text.y = element_text(angle=0, hjust=0),
    axis.ticks = element_line(size=0.2),
    # axis.ticks.y = element_blank(),
    panel.grid.major = element_line(size=0.2),
    panel.grid.minor = element_blank())
ggsave("results/interventions_time_distribution.pdf",width=8,height=5)

# plot intervention percentages -------------------------------------------
capital_cities=c("Sydney","Melbourne","Brisbane","Perth","Adelaide","Canberra",
                 "Hobart","Darwin")
regional_cities=c("Gold Coast","Newcastle","Sunshine Coast","Wollongong",
                  "Geelong","Townsville","Cairns","Toowoomba","Ballarat",
                  "Bendigo","Mackay","Launceston","Albury Wodonga")

large_cities=c("Sydney","Melbourne","Brisbane","Perth","Adelaide")
small_cities=c("Gold Coast","Canberra","Newcastle","Wollongong","Sunshine Coast",
               "Geelong","Hobart","Townsville","Darwin","Cairns","Toowoomba",
               "Ballarat","Bendigo","Albury Wodonga","Launceston","Mackay")


# source: https://en.wikipedia.org/wiki/List_of_cities_in_Australia_by_population
# | rank | City                   | State/territory                               | ext 2020  | pop 2011  | growth | nat pop|
# | ---- | ---------------------- | --------------------------------------------- | --------- | --------- | ------ | ------ |
# | 1    | Melbourne              | Victoria                                      | 4,969,305 | 3,999,982 | 24.08% | 19.86% |
# | 2    | Sydney                 | New South Wales                               | 4,966,806 | 4,231,954 | 19.10% | 20.93% |
# | 3    | Brisbane               | Queensland                                    | 2,475,680 | 2,065,996 | 19.20% | 9.85%  |
# | 4    | Perth                  | Western Australia                             | 2,083,645 | 1,728,867 | 19.12% | 8.24%  |
# | 5    | Adelaide               | South Australia                               | 1,357,504 | 1,262,940 | 6.56%  | 5.38%  |
# | 6    | Gold Coast–Tweed Heads | Queensland/New South Wales                    | 709,495   | 557,822   | 21.75% | 2.72%  |
# | 7    | Newcastle–Maitland     | New South Wales                               | 498,095   | 398,770   | 22.05% | 1.95%  |
# | 8    | Canberra–Queanbeyan    | Australian Capital  Territory/New South Wales | 464,995   | 391,645   | 16.83% | 1.83%  |
# | 9    | Sunshine Coast         | Queensland                                    | 348,343   | 270,770   | 23.14% | 1.33%  |
# | 10   | Central Coast          | New South Wales                               | 337,284   | 297,713   | 12.68% | 1.31%  |
# | 11   | Wollongong             | New South Wales                               | 309,345   | 268,944   | 12.57% | 1.21%  |
# | 12   | Geelong                | Victoria                                      | 282,412   | 173,454   | 54.67% | 1.07%  |
# | 13   | Hobart                 | Tasmania                                      | 219,071   | 211,656   | 9.90%  | 0.93%  |
# | 14   | Townsville             | Queensland                                    | 183,322   | 162,292   | 11.42% | 0.72%  |
# | 15   | Cairns                 | Queensland                                    | 155,340   | 133,911   | 14.05% | 0.61%  |
# | 16   | Toowoomba              | Queensland                                    | 139,526   | 105,984   | 29.13% | 0.55%  |
# | 17   | Darwin                 | Northern Territory                            | 133,268   | 120,586   | 23.20% | 0.59%  |
# | 18   | Ballarat               | Victoria                                      | 109,533   | 91,801    | 14.89% | 0.42%  |
# | 19   | Bendigo                | Victoria                                      | 102,499   | 86,079    | 15.15% | 0.40%  |
# | 20   | Albury–Wodonga         | New South Wales/Victoria                      | 96,075    | 82,083    | 14.03% | 0.37%  |
# | 21   | Launceston             | Tasmania                                      | 89,178    | 82,220    | 6.21%  | 0.35%  |
# | 22   | Mackay                 | Queensland                                    | 80,926    | 77,293    | 3.69%  | 0.32%  |

city_type <- bind_rows(
  data.frame(city="Australia",type=""),
  data.frame(city=capital_cities,type="Capital Cities"),
  data.frame(city=regional_cities,type="Large Regional Cities"),
) %>%
  mutate(type=factor(type,
                     levels=c("","Capital Cities","Large Regional Cities"),
                     labels=c("","Capital Cities","Large Regional Cities")))

city_type2 <- bind_rows(
  data.frame(city="Australia",type=""),
  data.frame(city=large_cities,type="Large Cities"),
  data.frame(city=small_cities,type="Small Cities"),
) %>%
  mutate(type=factor(type,
                     levels=c("","Large Cities","Small Cities"),
                     labels=c("","Large Cities","Small Cities")))

city_labels2=c("Australia (21 cities)","Sydney, New South Wales",
               "Melbourne, Victoria","Brisbane, Queensland","Perth, Western Australia",
               "Adelaide, South Australia","Canberra, Australian Capital Territory",
               "Hobart, Tasmania","Darwin, Northern Territory","Gold Coast, Queensland",
               "Newcastle, New South Wales","Sunshine Coast, Queensland",
               "Wollongong, New South Wales","Geelong, Victoria","Townsville, Queensland",
               "Cairns, Queensland","Toowoomba, Queensland","Ballarat, Victoria",
               "Bendigo, Victoria","Mackay, Queensland","Launceston, Tasmania",
               "Albury Wodonga")

summaryTableLong <- summaryTable %>%
  dplyr::select(city,mode,noshift_percent,modeshift_percent,jobshift_percent) %>%
  pivot_longer(cols=noshift_percent:jobshift_percent,
               names_to="measure",
               values_to="percent") %>%
  mutate(percent=percent/100) %>%
  mutate(measure=factor(measure,
                     levels=c("jobshift_percent","modeshift_percent","noshift_percent"),
                     labels=c("Job Shift","Mode Shift","No Shift"))) %>%
  inner_join(city_type,by="city") %>%
  mutate(city=factor(city,levels=rev(city_labels),labels=rev(city_labels))) %>%
  arrange(city) %>%
  mutate(city2=as.character(city))

ggplot(summaryTableLong, aes(x=percent,y=city)) +
  facet_grid(rows=vars(type),cols=vars(mode), scales = "free", space = "free") +
  geom_col(position="dodge", aes(fill=measure,y=city), width=0.8) +
  scale_fill_manual(values=c("#66c2a5","#fc8d62","#8da0cb"),
                    guide=guide_legend(reverse=TRUE),
                    labels=c("Job-worker shift","Mode shift","Baseline"),
                    breaks=c("Job Shift","Mode Shift","No Shift")) +
  labs(y=NULL, x="Percent within 30 minutes of work")+ 
  scale_x_continuous(expand=c(0,0), limits=c(0, 1), labels=scales::percent) +
  # scale_y_discrete(limits = rev(levels(summaryTableLong$city2))) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,0,5,0),
        text = element_text(family = "Helvetica"),
        panel.spacing.x = unit(1.5, "lines"),
        panel.grid.major = element_line(size=0.3),
        panel.grid.minor = element_blank())
        # plot.margin=unit(c(0.1,0.5,0,0.1), "cm")) #top, right, bottom, left
ggsave("results/interventions.pdf",width=8,height=6)

# attempt at rotation
summaryTableLongRotate <- summaryTable %>%
  dplyr::select(city,mode,noshift_percent,modeshift_percent,jobshift_percent) %>%
  pivot_longer(cols=noshift_percent:jobshift_percent,
               names_to="measure",
               values_to="percent") %>%
  mutate(percent=percent/100) %>%
  mutate(measure=factor(measure,
                        levels=c("noshift_percent","modeshift_percent","jobshift_percent"),
                        labels=c("No Shift","Mode Shift","Job Shift"))) %>%
  inner_join(city_type2,by="city") %>%
  mutate(city=factor(city,levels=city_labels_reordered,labels=city_labels_reordered)) %>%
  arrange(city) %>%
  mutate(city2=as.character(city))
ggplot(summaryTableLongRotate, aes(x=city,y=percent)) +
  facet_grid(rows=vars(mode),cols=vars(type), scales = "free", space = "free") +
  geom_col(position="dodge", aes(fill=measure,x=city), width=0.8) +
  scale_fill_manual(values=c("#66c2a5","#fc8d62","#8da0cb"),
                    # guide=guide_legend(reverse=TRUE),
                    labels=c("Baseline","Mode shift","Job-worker shift"),
                    breaks=c("No Shift","Mode Shift","Job Shift")) +
  labs(x=NULL, y="Percent within 30 minutes of work") + 
  scale_y_continuous(expand=c(0,0), limits=c(0, 1), labels=scales::percent) +
  # scale_y_discrete(limits = rev(levels(summaryTableLong$city2))) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,0,5,0),
        text = element_text(family = "Helvetica"),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        panel.spacing.y = unit(0.6, "lines"),
        panel.grid.major = element_line(size=0.3),
        panel.grid.minor = element_blank())
# plot.margin=unit(c(0.1,0.5,0,0.1), "cm")) #top, right, bottom, left
ggsave("results/interventionsRotated.pdf",width=8,height=6)

# ggplot(no_shift,aes(x=time,y=noshift))+geom_point()+geom_smooth(span = 0.2)+facet_grid(cols=vars(mode),rows=vars(city), scales = "free")
# ggsave("test.pdf",width=8,height=16)



# save intervention percentages table -------------------------------------
summaryTableFinal <- summaryTableLong %>%
  dplyr::select(city,mode,measure,percent) %>%
  mutate(city=factor(city,levels=city_labels2,labels=city_labels2)) %>%
  mutate(measure=factor(measure,
                        levels=c("No Shift","Mode Shift","Job Shift"),
                        labels=c("No Shift","Mode Shift","Job Shift"))) %>%
  mutate(percent=percent*100) %>%
  arrange(city,mode,measure) %>%
  pivot_wider(names_from=c("mode","measure"),
              values_from=percent)
write.csv(summaryTableFinal, "results/summaryTableFinal.csv", row.names=F)


