gisruk<-read.csv("C:/Users/ktong/Desktop/GIS/_Exports/GISRUK_Data.csv")
str(gisruk)

#Kendall correlation (Builidng Area and Road Noise)
cor1<-cor.test(gisruk$bld_area,gisruk$RoadNoise_Ord, method="kendall")
cor1

#Kendall correlation (No.of building use and Road Noise)
cor2<-cor.test(gisruk$Land_Use,gisruk$RoadNoise_Ord, method="kendall")
cor2

#Kendall correlation between specific building use and road noise
#Kendall correlation (Other and Road Noise)
cor3<-cor.test(gisruk$Other,gisruk$RoadNoise_Ord, method="kendall")
cor3

#Kendall correlation (Retail and Road Noise)
cor4<-cor.test(gisruk$Retail,gisruk$RoadNoise_Ord, method="kendall")
cor4

#Kendall correlation (Residential and Road Noise)
cor5<-cor.test(gisruk$Residential,gisruk$RoadNoise_Ord, method="kendall")
cor5

#Kendall correlation (Office and Road Noise)
cor6<-cor.test(gisruk$Office,gisruk$RoadNoise_Ord, method="kendall")
cor6

#Kendall correlation (Public and Road Noise)
cor7<-cor.test(gisruk$Public,gisruk$RoadNoise_Ord, method="kendall")
cor7

#Kendall correlation (Transport and Road Noise)
cor8<-cor.test(gisruk$Transport,gisruk$RoadNoise_Ord, method="kendall")
cor8
