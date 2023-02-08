library(sf)
library(tidyverse)

layers<-st_layers("SpatialData_Packaged.gpkg")
layers<-layers[["name"]][c(1,3,5:12)]

data<-st_read("SpatialData_Packaged.gpkg",layer = "Grid_500m_Join") %>%
  select(id)



for (l in layers){
  print(paste("Working on",l))
  if(l=="Building_Area (sqm)"){
    d<-st_read("SpatialData_Packaged.gpkg",layer = l)
    ##building use grouping
    cat<-unique(d$UKBld_use)
    retail<-cat[c(4,5,9,17,20,22,24)]
    public<-cat[c(2,7,11,12,15,18,21)]
    other<-cat[c(3,8,10,16,19,23,25,26,13)]
    ##data wrangling
    d<-d %>%
      mutate(
        UKBld_use= case_when(
          UKBld_use=="RESIDENTIAL ONLY" ~ "Residential",
          UKBld_use %in% retail ~ "Retail",
          UKBld_use=="TRANSPORT" ~ "Transport",
          UKBld_use %in% public ~ "Public",
          UKBld_use=="OFFICE ONLY" | UKBld_use=="STORAGE/WAREHOUSING WITH LINKED OFFICE BLOCK" ~ "Office",
          UKBld_use %in% other ~ "Other"
        )
      ) %>%
      st_drop_geometry() %>%
      group_by(id,UKBld_use) %>%
      dplyr::summarise(
        build_area=sum(UKBld_Hex_,na.rm = T)
      ) %>%
      pivot_wider(id_cols = id, values_from = build_area, names_from = UKBld_use) %>%
      dplyr::rowwise()%>%
      dplyr::mutate(
        bld_area=sum(c(Other,Retail,Residential,Office,Public,Transport),na.rm = T)
      )
    
    d[is.na(d)] <- 0
    data<-inner_join(data,d,by=c("id"="id"))
    
  }else{
  d<-st_read("SpatialData_Packaged.gpkg",layer = l) %>%
    select(-c("left","top","right","bottom"))%>%
    st_drop_geometry()
  names(d)[2]<-l
  data<-inner_join(data,d,by=c("id"="id"))
  }
}

data[is.na(data)] <- 0
data<-data %>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    Railway=sum(c(TL_Railway_SpatialJoinSum,TQ_Railway_SpatialJoinSum,TQ_Railway_Tunnel_SpatialJoinSum),na.rm = T),
    Road=sum(c(TL_RoadLink_SpatialJoinSum,TQ_RoadLink_SpatialJoinSum),na.rm = T)
  ) %>%
  select(!starts_with("TL")) %>%
  select(!starts_with("TQ"))


st_write(data,"data_processed.gpkg")




  