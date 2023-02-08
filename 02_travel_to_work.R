options(java.parameters = '-Xmx150G')

library(ggplot2)
library(sf)
library(readr)
library(tidyverse)
library(r5r)


msoa_centroids<- st_read("input/msoa_centroids.gpkg")
car <- read_csv("input/car.csv")
foot <- read_csv("input/foot.csv")
bicycle <- read_csv("input/bycicle.csv")

car <- car %>%
  tidyr::pivot_longer(cols = !msoa, names_to = "destination", values_to = "commuters") %>%
  filter(commuters!=0) %>%
  mutate(
    mode="CAR"
  )

foot <- foot %>%
  tidyr::pivot_longer(cols = !msoa, names_to = "destination", values_to = "commuters") %>%
  filter(commuters!=0) %>%
  mutate(
    mode="WALK"
  )

bicycle <- bicycle %>%
  tidyr::pivot_longer(cols = !msoa, names_to = "destination", values_to = "commuters") %>%
  filter(commuters!=0) %>%
  mutate(
    mode="BICYCLE"
  ) 

travel_to_work<-rbind(car,foot,bicycle)
remove(car,foot,bicycle)

####join geom

origins<-travel_to_work %>%
  inner_join(msoa_centroids[c("MSOA11CD","LONG","LAT")], by=c("msoa"="MSOA11CD")) %>%
  dplyr::rename(
                  id=msoa,
                  lon=LONG,
                  lat=LAT
                )

destinations<-travel_to_work %>%
  inner_join(msoa_centroids[c("MSOA11CD","LONG","LAT")], by=c("destination"="MSOA11CD")) %>%
  dplyr::rename(
    id=destination,
    lon=LONG,
    lat=LAT
  )

####run routing with geometry path

path <- "../routing_london/"
r5r_core <- setup_r5(data_path = path, verbose = FALSE)

modes<-unique(travel_to_work$mode)


for (m in modes){
  
  o<-origins[origins$mode==m,]
  d<-destinations[destinations$mode==m,]
  
  ttm<-detailed_itineraries(r5r_core,
                            origins = o,
                            destinations = d,
                            mode=m,
                            drop_geometry = FALSE,
                            verbose=FALSE,
                            progress=TRUE)
  
  write_rds(ttm,paste0("output/route",m,".rds"))
}

grid<-st_read("input/SpatialData_Packaged.gpkg",layer = "Grid_500m_Join") %>%
  select(id)

bicycle<-st_join(st_transform(ttm,27700),grid)
bicycle<-st_drop_geometry(bicycle[c("fromId","toId","distance","id","mode")])

write_rds(bicycle,paste0("output/bicycle_join.rds"))

walk<-readRDS("output/routeWALK.rds")
walk<-st_transform(walk,27700)
walk<-st_join(walk,grid)
walk<-st_drop_geometry(walk[c("fromId","toId","distance","id","mode")])

write_rds(walk,paste0("output/walk_join.rds"))


car<-readRDS("output/routeCAR.rds")
car<-st_transform(car,27700)


n <- nrow(car)
data.subset<-list()
j=0
for(i in seq(1,n,50000)){
  j=j+1
  start <- i
  end <- i + 49999
  join <- st_join(car[start:end,],grid)
  data.subset[[j]]<-st_drop_geometry(join[c("fromId","toId","distance","id","mode")])
  print(j)
}

saveRDS(data.subset,"output/datasubset.rds")
data.subset<-do.call("rbind",data.subset)

car<-data.subset
walk<-readRDS("output/walk_join.rds")
bicycle<-readRDS("output/bicycle_join.rds")
travel_data<-rbind(car,walk,bicycle)
remove(car,walk,bicycle)

grid_data<-st_read("output/data_processed.gpkg")

travel_data_geo<-travel_data %>%
  inner_join(st_drop_geometry(grid_data[c("id","Noise_Road")]),by=c("id"="id"))

travel_data_agg<-travel_data_geo %>%
  group_by(fromId,toId,distance,mode,Noise_Road) %>%
  dplyr::summarise(
    n=n()
  )

travel_data_tot<-travel_data_geo %>%
  group_by(fromId,toId,distance,mode) %>%
  dplyr::summarise(
    tot=n()
  )

travel<-travel_data_agg %>%
  inner_join(travel_data_tot,by=c("fromId"="fromId","toId"="toId","distance"="distance","mode"="mode")) %>%
  inner_join(travel_to_work,by=c("fromId"="msoa","toId"="destination","mode"="mode")) 


travel<-travel %>%
  mutate(
    cellM=(distance/tot)*n
  )

avg_travel_noise<-travel %>%
  group_by(mode,Noise_Road) %>%
  dplyr::summarise(
    meanM=mean(cellM*commuters)
  )

tot_meters<-avg_travel_noise %>%
  group_by(mode)%>%
  dplyr::summarise(
    tot=sum(meanM)
  )

avg_travel_noise<-avg_travel_noise %>%
  inner_join(tot_meters,by=c("mode"="mode")) %>%
  mutate(
    meanMp=meanM/tot,
    Noise_Road=factor(Noise_Road,levels = c("0","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"),labels = c("<55","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"))
  )

png("./img/commuting_mode.png", units="in", width=3, height=5, res=300)

ggplot(avg_travel_noise,aes(x=Noise_Road,y=meanMp*100))+
  geom_bar(stat = "identity",width = 0.7, fill="#1c5184")+
  coord_flip() +
  facet_wrap(~mode,ncol=1)+
  theme_minimal() +
  labs(x="Road Noise Class", y="Distance travelled (%)")+
  theme(text = element_text(family="Times New Roman"))

dev.off()
