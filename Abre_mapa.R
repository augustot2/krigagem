
require("rgdal") 
require("maptools")
require("ggplot2")
require("ggmap")
require("plyr")
require("RColorBrewer") 
require("classInt") 
require("scales")
library(rgeos)       # loads polygon clipping library
library(maptools)     # loads sp library too
library(rgdal)
library(sp)




cria_faixa<- function (x){
  # x <- seq(0,4000,50)
  faixas <- c(seq(0,250,50),500,1000,2000,Inf)
  ref <-data.frame(matrix(ncol = 2,nrow = length(faixas)))
  names(ref) <-  c("brks","brks2")
  for(i in 1:(length(faixas)-1)){
    if(i + 1 <10  ){
      ref[i,2] <- paste(0,0,i,"-","[",faixas[i],"-",faixas[i+1],"]",sep="") 
    }
    else{
      ref[i,2] <- paste(0,i,"-","[",faixas[i],"-",faixas[i+1],"]",sep="")
    }
    ref[i,1]<-paste(faixas[i],"-",faixas[i+1])  
  }
  
  faixa_nova <- data.frame(matrix(ncol = 1,nrow = length(x)))
  names(faixa_nova) <-  c("brks")
  for(j in 1:length(x)){
    for(i in 1:(length(faixas)-1)){
      if (faixas[i] <= x[j] && x[j] <= faixas[i+1]) faixa_nova[j,1] <- paste(faixas[i],"-",faixas[i+1])  
    }
  } 
  require(dplyr)
  z<- left_join(faixa_nova,ref,by="brks",match=all,type="full")
  z[1:length(x),2]
  
}

###mapa###
setwd("I:\\LAFEE/Projeto_krigagem_SP/")
file_map <-"I:\\LAFEE/Projeto_krigagem_SP/mapa/mun_lin.shp"
x<- ogrListLayers(file_map)
sp=readOGR(file_map, layer=x) #will load the shapefile to your dataset.
sp <-  spTransform(sp, CRS("+proj=longlat +datum=WGS84"))
plot(sp)



##################################
### configura mapa para ggplot ###
##################################

sp@data$id = rownames(sp@data)
sp.points  = fortify(sp, region="id")
sp.df      = join(sp.points, sp@data, by="id")

##################
# mapa da baixada#
##################
ggplot()+ 
 geom_polygon(data=sp.df, aes(long,lat,group=group ),fill="white",colour="black")+
 geom_path(color = "black")+
 ggtitle("Baixada Santista")+
 theme_bw() +
 coord_fixed()
#####################
# no google maps   #
###################
library(ggmap)
sp@bbox[,1]<-sp@bbox[,1] - 2
sp@bbox[,2]<-sp@bbox[,2] + 2
#recupera mapas no quadrante da baixada
map <- get_map(c(sp@bbox),maptype="satellite")
ggmap(map)+
geom_polygon(aes(x = long, y = lat, group=group), alpha= 0,colour="white", fill="white", size = 0.5, data = sp.df)  


 