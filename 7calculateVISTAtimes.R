library("dplyr")
library("haven") # for spss reading
library("ggplot2")

# vista data with valid coordinates for origin and destination
data <- read_sav("vista/RMIT VISTA 2012-16 Pack/T_VISTA12_16_v1.sav") %>%
  filter(ORIGLONG -2 & ORIGLAT != -2 & DESTLONG != -1 & DESTLAT != -1)

# LINKMODE numbers for the different transport types
# PT	 7,8,10
# CAR	 1,2,3
# BIKE 5
# WALK 4

# assign mode type, calculate speed, and distance traveled in 30 minutes
dataCleaned <- data %>%
  filter(LINKMODE %in% c(1,2,3,4,5,7,8,10)) %>%
  filter(DESTPLACE1==3) %>% # going to work
  mutate(type = ifelse(LINKMODE %in% c(7,8,10), "PT",NA),
         type = ifelse(LINKMODE %in% c(1,2,3), "CAR",type),
         type = ifelse(LINKMODE %in% c(5), "BIKE",type),
         type = ifelse(LINKMODE %in% c(4), "WALK",type)) %>%
  filter(CUMDIST>0 & TRIPTIME>0) %>%
  dplyr::select(type, CUMDIST, TRIPTIME) %>%
  mutate(speed=(CUMDIST/(TRIPTIME/60)), # km/h
         dist_30_min=speed*0.5) %>% # distance in 30 minutes
  mutate(type=factor(type,levels=c("WALK","BIKE","PT","CAR"),
                     labels=c("Walking","Cycling","Public transport","Driving"))) %>%
  arrange(type)

mean(dataCleaned%>%filter(type=="Driving")%>%pull(TRIPTIME))

quantile(dataCleaned%>%filter(type=="Driving")%>%pull(TRIPTIME),
         c(.025, .5, .975)) 

binwidth=1
dataCleanedAggregated <- dataCleaned %>%
  mutate(distance=findInterval(CUMDIST,seq(0,200,binwidth))) %>%
  mutate(distance=distance*binwidth-(binwidth/2)) %>%
  group_by(type,distance) %>%
  summarise(count=n()) %>%
  ungroup()

dataCleanedAggregatedMax <- dataCleanedAggregated %>%
  group_by(type) %>%
  summarise(max_count=max(count)) %>%
  ungroup()
# summarise by mode type
dataSummarised <- dataCleaned %>%
  group_by(type) %>%
  summarise(mean_time=mean(TRIPTIME),mean_dist=mean(CUMDIST),
            mean_speed=mean(speed),mean_dist_30_min=mean(dist_30_min)) %>%
  ungroup() %>%
  mutate(mean_dist_30_min2=round(mean_dist_30_min,2)) %>%
  mutate(mean_dist_30_min2=ifelse(type=="Public transport",5.89,mean_dist_30_min2)) %>%
  mutate(mean_dist_30_min2=ifelse(type=="Driving",15.48,mean_dist_30_min2)) %>%
  inner_join(dataCleanedAggregatedMax)
write.csv(dataSummarised,"data-output/vistaTimes.csv",append=FALSE,
          row.names=FALSE)



# Plot histograms for each transport mode
ggplot() +
  geom_col(data=dataCleanedAggregated, aes(x=distance,y=count), width=binwidth,fill="gray50") +
  geom_vline(data=dataSummarised,aes(xintercept=mean_dist_30_min2)) +
  geom_text(data=dataSummarised,
            aes(x=mean_dist_30_min2,y=0.1*max_count,label=mean_dist_30_min2),
            hjust=-0.2,vjust=0) +
  facet_grid(vars(type), scales = "free") +
  labs(x = "Distance traveled to work (km)", y="Trips to work") +
  scale_x_continuous(expand=c(0,0), limits=c(0, 60)) +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    # axis.text.y = element_blank(),
    strip.text = element_text(size = 10),
    # strip.text.y = element_text(angle=0, hjust=0),
    axis.ticks = element_line(size=0.2),
    # axis.ticks.y = element_blank(),
    panel.grid.major = element_line(size=0.2),
    panel.grid.minor = element_blank())
ggsave("results/vistaTimes.pdf", width=8, height=5, units = "in")

ggplot() +
  geom_col(data=dataCleanedAggregated, aes(x=distance,y=count), width=binwidth,fill="gray50") +
  geom_vline(data=dataSummarised,aes(xintercept=mean_dist_30_min2)) +
  geom_text(data=dataSummarised,
            aes(x=mean_dist_30_min2,y=850,label=mean_dist_30_min2),
            hjust=-0.2,vjust=0) +
  facet_grid(vars(type), scales = "free") +
  labs(x = "Distance traveled to work (km)", y="Trips to work") +
  scale_x_continuous(expand=c(0,0), limits=c(0, 60)) +
  scale_y_continuous(expand=c(0,0), limits=c(0, 1070)) +
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    # axis.text.y = element_blank(),
    strip.text = element_text(size = 10),
    # strip.text.y = element_text(angle=0, hjust=0),
    axis.ticks = element_line(size=0.2),
    # axis.ticks.y = element_blank(),
    panel.grid.major = element_line(size=0.2),
    panel.grid.minor = element_blank())
ggsave("results/vistaTimes2.pdf", width=8, height=5.5, units = "in")



# Calculate linear model for cars and PT ----------------------------------
dataCleaned2 <- dataCleaned %>%
  rename(distance=CUMDIST,mode=type) %>%
  filter(mode %in% c("Driving","Public transport")) %>%
  filter(distance>=1 & distance <=100 & speed >= 5 & speed <= 100)

dataCleaned3 <- dataCleaned2 %>%
  group_by(mode) %>%
  summarise(mean_speed=mean(speed),sd_speed=sd(speed),mean_dist=mean(distance),
            st_dist=sd(distance))
ggplot(dataCleaned2,aes(x=distance,y=speed)) +
  labs(x = "Distance (km)", y="Speed (km/h)") +
  geom_point(alpha=0.05) +
  # geom_smooth(method=poly(x, 3, raw=TRUE), se=F) +
  stat_smooth(method="lm", se=TRUE, fill=NA) +
  # stat_smooth(method="lm", se=TRUE, fill=NA,
  #             formula=y ~ poly(x, 2, raw=TRUE),colour="blue") +
  # geom_tile(width=0.5,height=5,linetype="blank") + # x, y
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,50)) +
  facet_grid(rows=vars(mode), scales = "free")

make_lm <-function(name,df) {
  lm_df <- lm(speed ~ distance, data=df)
  results <- data.frame(row.names = NULL,
                        mode=name,
                        m=lm_df$coefficients[2],
                        c=lm_df$coefficients[1],
                        r2=summary(lm_df)$r.squared)
  return(results)
}
ggsave("data-output/Speed_DistributionVISTA.pdf", width=6, height=4, units = "in")

lm_vista <- bind_rows(
  make_lm("Driving",dataCleaned2%>%filter(mode=="Driving")),
  make_lm("Public transport",dataCleaned2%>%filter(mode=="Public transport")))

write.csv(lm_vista, "data-output/lm_vista.csv", append=F, row.names=F)





funCar <- function(d) {d / (0.697*d + 20.165)}
funCarOld <- function(d) {d / (30.4)}
funPT <- function(d) {d / (0.509*d + 8.782)}
funPTOld <- function(d) {d / (18.790)}

funWalk <- function(d) {d / (4.539)}
funCycle <- function(d) {d / (13.099)}



ggplot(data.frame(x = c(0,100)), aes(x = x)) +
  geom_path(stat="function",fun = funPTOld, aes(color="#fdbf6f"),size=1, lineend="round") +
  geom_path(stat="function",fun = funCarOld, aes(color="#fb9a99"),size=1, lineend="round") +
  geom_path(stat="function",fun = funWalk, aes(color="#1f78b4"),size=1, lineend="round") +
  geom_path(stat="function",fun = funCycle, aes(color="#33a02c"),size=1, lineend="round") +
  geom_path(stat="function",fun = funPT, aes(color="#ff7f00"),size=1, lineend="round") +
  geom_path(stat="function",fun = funCar, aes(color="#e31a1c"),size=1, lineend="round") +
  scale_colour_identity("Mode", guide="legend",
                        labels = c("Walking","Cycling","Public transport","Driving"),
                        breaks = c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c")) +
  labs(x = "distance (km)", y = "time (hours)") +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,25)) +
  scale_y_continuous(limits=c(0,1.8), breaks=seq(0,2,0.5))
ggsave("data-output/Speed_Distribution_Curves_VISTA.pdf", width=6, height=4, units = "in")



# calculate proportion of walking in PT -----------------------------------

dataPT <- read_sav("vista/RMIT VISTA 2012-16 Pack/T_VISTA12_16_v1.sav") %>%
  filter(ORIGLONG -2 & ORIGLAT != -2 & DESTLONG != -1 & DESTLAT != -1) %>%
  filter(DESTPLACE1==3) %>% # going to work
  filter(LINKMODE %in% c(7,8,10)) %>%
  filter(CUMDIST>0 & TRIPTIME>0) %>%
  dplyr::select(TRIPID,CUMDIST,TRIPTIME,Mode1:Mode9,Time1:Time9,Dist1:Dist9) %>%
  rename_with( ~ gsub("([a-z])([0-9])","\\1_\\2",.x))
                

# LINKMODE numbers for the different transport types
# PT	 7,8,10
# CAR	 1,2,3
# BIKE 5
# WALK 4

dataPT_mode <- dataPT %>%
  dplyr::select(TRIPID,Mode_1:Mode_9) %>%
  pivot_longer(Mode_1:Mode_9,names_to="stop",values_to="mode") %>%
  filter(mode %in% c(1,2,3,4,5,7,8,10)) %>%
  mutate(stop=as.numeric(gsub("Mode_", "",stop)))

dataPT_dist <- dataPT %>%
  dplyr::select(TRIPID,Dist_1:Dist_9) %>%
  mutate_at(vars(starts_with("Dist")),as.numeric) %>%
  pivot_longer(Dist_1:Dist_9,names_to="stop",values_to="distance") %>%
  filter(distance>0) %>%
  mutate(stop=as.numeric(gsub("Dist_", "",stop)))

dataPTlong <- dataPT_mode %>%
  inner_join(dataPT_dist, by=c("TRIPID","stop")) %>%
  mutate(type = ifelse(mode %in% c(7,8,10), "PT",NA),
         type = ifelse(mode %in% c(1,2,3), "CAR",type),
         type = ifelse(mode %in% c(5), "BIKE",type),
         type = ifelse(mode %in% c(4), "WALK",type)) %>%
  group_by(TRIPID,type) %>%
  summarise(distance=sum(distance,na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from="type",values_from="distance") %>%
  mutate(across(CAR:BIKE,~replace_na(.x,0)))

dataPT_final <- dataPT %>%
  dplyr::select(TRIPID,CUMDIST,TRIPTIME) %>%
  inner_join(dataPTlong,by="TRIPID") %>%
  mutate(speed=(CUMDIST/(TRIPTIME/60)), # km/h
         dist_30_min=speed*0.5) %>% # distance in 30 minutes
  mutate(proportion_PT = PT/CUMDIST,
         proportion_WALK = WALK/CUMDIST)

dataPT_summary <- dataPT_final %>%
  summarise(across(CUMDIST:proportion_WALK,mean))
hist(dataPT_final$WALK)
hist(dataPT_final$proportion_WALK)
mean(dataPT_final$WALK)


write.csv(dataPT_summary,"data-output/dataPT_summary.csv",
          row.names=FALSE)
