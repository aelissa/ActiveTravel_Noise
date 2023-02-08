library(tidyr)
library(dplyr)
library(ggplot2)
library(sas7bdat)
library(mlogit)
library(nnet)
library(reshape2)

gisruk<-read.csv("input/GISRUK_Data.csv")
str(gisruk)

#Kendall correlation (Builidng Area and Road Noise)
cor1<-cor.test(gisruk$bld_area,gisruk$RoadNoise_Ord, method="kendall")
cor1

plot(gisruk$RoadNoise_Ord,gisruk$bld_area)


#Conversion of scale for multinominal regression (which doesn't read 0)
gisruk$RoadNoise_OrdPlus1<- factor(gisruk$RoadNoise_OrdPlus1)
gisruk$RoadNoise_OrdPlus1<- relevel(gisruk$RoadNoise_OrdPlus1, ref='6')


#Multinominal Logistic Regress (DV: Road Noise; IV: Building Area across use)
output1 <- multinom(RoadNoise_OrdPlus1 ~ Other + Retail + Residential + Office + Public + Transport, data=gisruk)
output1
logLik(output1)
summary(output1)

##two-tail Z-test for significance
z1 <- summary(output1)$coefficients/summary(output1)$standard.errors
z1
p1 <- (1 - pnorm(abs(z1),0,1))*2
p1


#Multinominal Logistic Regress (DV: Road Noise; IV: Building Use)
output3 <- multinom(RoadNoise_OrdPlus1 ~ bld_use, data=gisruk)
output3
logLik(output3)
summary(output3)

z3 <- summary(output3)$coefficients/summary(output3)$standard.errors
z3
p3 <- (1 - pnorm(abs(z3),0,1))*2
p3


#Multinominal Logistic Regress (DV: Road Noise; IV: LOSA Population per sqkm)
output5 <- multinom(RoadNoise_Ord ~ LOSA_PeoplePerSqkm, data=gisruk)
output5
logLik(output5)
summary(output5)

z5 <- summary(output5)$coefficients/summary(output5)$standard.errors
z5
p5 <- (1 - pnorm(abs(z5),0,1))*2
p5

####Visualisation

library(ggpubr)
library(grid)
#### Noise Level vs Average building area per grid

df_tot<-df %>%
  group_by(Noise_Road) %>%
  dplyr::summarise(
    tot=n()
  )

df_agg1<-df %>%
  group_by(Noise_Road) %>%
  dplyr::summarise(
    bld_area=sum(bld_area,na.rm = T)
  ) %>%
  inner_join(df_tot,by=c("Noise_Road"="Noise_Road")) %>%
  mutate(
    y=bld_area/(tot*216555),
    Noise_Road=factor(Noise_Road,levels = c("0","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"),labels = c("<55","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"))
  )
    

p1<-ggplot(df_agg1,aes(x=Noise_Road,y=y*100))+
  geom_bar(stat = "identity",width = 0.7, fill="#1c5184")+
  coord_flip() +
  labs(x="Road Noise Class",title = "Average Building Coverage (%)")+
  theme_minimal() +
  theme(text = element_text(family="Times New Roman"),axis.title.x = element_blank(),plot.title = element_text(hjust = 0.5))
  
  
#### Noise Level vs average number of building use

df_agg2<-df %>%
  group_by(Noise_Road) %>%
  dplyr::summarise(
    bld_use=sum(bld_use,na.rm = T)
  ) %>%
  inner_join(df_tot,by=c("Noise_Road"="Noise_Road")) %>%
  mutate(
    y=bld_use/tot,
    Noise_Road=factor(Noise_Road,levels = c("0","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"),labels = c("<55","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"))
    )

p2<-ggplot(df_agg2,aes(x=Noise_Road,y=y))+
  geom_bar(stat = "identity",width = 0.7, fill="#1c5184")+
  coord_flip() +
  labs(x="Road Noise Class",title = "Average Building Use (count)")+
  theme_minimal() +
  theme(text = element_text(family="Times New Roman"),axis.title.x = element_blank(),plot.title = element_text(hjust = 0.5))


### Noise Level per people density

df_agg3<-df %>%
  group_by(Noise_Road) %>%
  dplyr::summarise(
    LOSA_PeoplePerSqkm=sum(LOSA_PeoplePerSqkm,na.rm = T)
  ) %>%
  inner_join(df_tot,by=c("Noise_Road"="Noise_Road")) %>%
  mutate(
    y=LOSA_PeoplePerSqkm/tot,
    Noise_Road=factor(Noise_Road,levels = c("0","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"))
    )

p3<-ggplot(df_agg3,aes(x=Noise_Road,y=y))+
  geom_bar(stat = "identity",width = 0.7, fill="#1c5184")+
  coord_flip() +
  labs(x="Road Noise Class",title = "Average Population Density (People per squared Km)")+
  theme_minimal() +
  theme(text = element_text(family="Times New Roman"),axis.title.x = element_blank(),plot.title = element_text(hjust = 0.5))


p<-ggarrange(p1+rremove("ylab"),p2+rremove("ylab"),ncol=1)
annotate_figure(p, left = textGrob("Road Noise Class", rot = 90, vjust = 1, gp = gpar(cex = 1.2,fontfamily="Times New Roman")))

png("./img/uff_noise.png", units="in", width=4, height=5, res=300)
annotate_figure(p, left = textGrob("Road Noise Class", rot = 90, vjust = 1, gp = gpar(cex = 1.2,fontfamily="Times New Roman")))
dev.off()
    
#### (Noise Level vs building density) Grouped according to number of building use

df_tot<-df %>%
  tidyr::pivot_longer(cols = c("Other","Retail","Residential","Office","Public","Transport"),names_to = "Use",values_to = "useArea")%>%
  filter(useArea!=0)%>%
  group_by(Noise_Road,Use) %>%
  dplyr::summarise(
    tot=n()
  )

df_agg4<-df %>%
  tidyr::pivot_longer(cols = c("Other","Retail","Residential","Office","Public","Transport"),names_to = "Use",values_to = "useArea")%>%
  group_by(Noise_Road,Use) %>%
  dplyr::summarise(
    area=sum(useArea,na.rm = T)
  ) %>%
  inner_join(df_tot,by=c("Noise_Road"="Noise_Road","Use"="Use")) %>%
  mutate(
    y=area/(tot*216555),
    Noise_Road=factor(Noise_Road,levels = c("0","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"),labels = c("<55","55.0-59.9","60.0-64.9","65.0-69.9","70.0-74.9",">=75.0"))
  )

df_max<-df_agg4 %>%
  group_by(Use) %>%
  dplyr::summarise(
    max=max(y)
  )

df_agg4<-df_agg4 %>%
  inner_join(df_max,by=c("Use"="Use")) %>%
  mutate(
    ynorm=y/max
  )
  
png("./img/use_noise.png", units="in", width=8, height=5, res=300)

ggplot(df_agg4,aes(x=Noise_Road,y=round(ynorm,1)))+
  geom_bar(stat = "identity",width = 0.7, fill="#1c5184")+
  coord_flip()+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),labels = c(0,0.25,0.5,0.75,1))+
  facet_wrap(~Use)+
  labs(y="Building Area (div by max)",x="Road Noise Class")+
  theme_minimal() +
  theme(text = element_text(family="Times New Roman"))

dev.off()


