
#options(jupyter.plot_mimetypes = 'image/jpg')
options(jupyter.plot_mimetypes = 'image/png')

require("rgdal") 
require("ggplot2")
require("ggmap")
require("plyr")
require("sp")


setwd(dir = "./mapa/");
file_map <- "mun_lin.shp";
x<- ogrListLayers(file_map);
sp=readOGR(file_map, layer=x)  
sp <-  spTransform(sp, CRS("+proj=longlat +datum=WGS84"));
plot(sp);
title("Mapa de São Paulo")



sp@data$id = rownames(sp@data)
sp.points  = fortify(sp, region="id")
sp.df      = join(sp.points, sp@data, by="id")

 ggplot()+ 
 geom_polygon(data=sp.df, aes(long,lat,group = group  ),fill="white",colour="black")+
 ggtitle("Mapa de São Paulo")+
 theme_bw() +
 coord_fixed()



sp@bbox[,1]<-sp@bbox[,1] - 2
sp@bbox[,2]<-sp@bbox[,2] + 2



map <- get_map(c(sp@bbox),maptype="satellite")
ggmap(map)+
geom_polygon(aes(x = long, y = lat, group=group), alpha= 0,colour="white", fill="white", size = 0.5, data = sp.df)  


require("geoR")
require("dplyr")


serie_temp <- read.csv2("../dados/serie_temporal_mensal.csv")
head(serie_temp)
meses <- c("janeiro","fevereiro","marco","abril","maio","junho",
           "julho","agosto","setembro","outubro","novembro","dezembro")

ano <- 2014
mes <- 4
titulo <- paste(mes,"de",ano,sep=" ")




serie <-serie_temp[serie_temp$ano == ano & serie_temp$mes == mes , ]
variaveis <- c("latitude","longitude","mm.sum")
serie <- serie[,variaveis]
head(serie)

geodata <- as.geodata(serie,coords.col = c(1,2),data.col = 3)

xrange <- c(min(geodata[[1]][,1])-0.1,max(geodata[[1]][,1])+0.1)
yrange <- c(min(geodata[[1]][,2])-0.1,max(geodata[[1]][,2])+0.1)

grid <- expand.grid(seq(xrange[1],xrange[2],by=.05),seq(yrange[1],yrange[2],by=.05))

variogram <- variog(geodata, trend="2nd", uvec=seq(0, .5, by=0.05))
plot(variogram)

initial.values <- expand.grid(seq(min(variogram$v), max(variogram$v), by=10),
                              seq(min(variogram$u), max(variogram$u),by=0.005))

model <- "cubic"
fit <- variofit(variogram, cov.model= model , ini.cov.pars=initial.values,
                    fix.nugget=FALSE)
    plot(variogram)
    title(paste(meses[mes],"de",ano,"-", "modelo", model ))
    lines(fit)
 

grid <- expand.grid(seq(xrange[1],xrange[2],by=.05),seq(yrange[1],yrange[2],by=.05))



variogram <- variog(geodata, trend="2nd", uvec=seq(0, .5, by=0.05))
plot(variogram)

initial.values <- expand.grid(seq(min(variogram$v), max(variogram$v), by=10),
                              seq(min(variogram$u), max(variogram$u),by=0.005))


models<-c("matern", "exponential", "gaussian",
          "spherical", "circular", "cubic", "wave")
for(model in models){
  fit <- variofit(variogram, cov.model= model , ini.cov.pars=initial.values,
                  fix.nugget=FALSE)
  plot(variogram)
  title(paste(meses[mes],"de",ano,"-", "modelo", model ))
  lines(fit)
  
}

model <- "cubic"
krigagem <- krige.control(type.krige = "ok", cov.model = model,
                          cov.pars = fit$cov.pars,nugget = min(variogram$v))
krigagem2 <- krige.conv(geodata, locations = grid, krige = krigagem)

contour(krigagem2,
        filled = TRUE,coords.data = geodata$coords,zlim = c(min(krigagem2$predict),max(krigagem2$predict)))
contour(krigagem2,
        filled = TRUE ,zlim = c(min(krigagem2$predict),max(krigagem2$predict)))


malha <- data.frame(grid[,1],grid[,2],krigagem2$predict)
write.csv2(malha,paste("malha","_",model,".csv",sep = ""),row.names = FALSE)
dir()

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
  require("dplyr")
  z<- dplyr::left_join(faixa_nova,ref,by="brks",match=all,type="full")
  z[1:length(x),2]
  
}

X = c(50,150,750,1000,1500)
data.frame(X,cria_faixa(X))

model = "cubic"
paste("malha","_",model,".csv",sep = "")
arquivos <-paste("malha","_",model,".csv",sep = "")
gg<- read.csv2(file = arquivos )
names(gg)<- c("y","x","z")
gg$brks <- cria_faixa(gg$z)
modelo <- gsub(x=arquivos,pattern = ".csv","")



z <-c(seq(0,250,50),500,1000,2000,Inf)
 
x <- seq(min(geodata[[1]][,1])-0.5,min(geodata[[1]][,1])-0.4,length(z))
y <- seq(min(geodata[[1]][,2])-0.5,min(geodata[[1]][,2])-0.4,length(z))

test <- data.frame(y=y ,x= x ,z= z, brks = cria_faixa(z))
gg<-rbind(gg,test)


    color  <-c("#a50026","#d73027",  "#f46d43",  "#fdae61",  "#fee08b",
               "#d9ef8b",  "#a6d96a",  "#66bd63",  "#1a9850",  "#006837")
    color <- color[order(1:10,decreasing = TRUE)]

 ggplot() %+% gg+aes(x,y)+
      geom_polygon(data=sp.df, aes(long,lat,group=group,fill=""),fill="white",colour="black") +
      geom_tile(aes(fill=brks,alpha=0.6 ))+
      scale_fill_manual("mm",values= color)+
      ggtitle(paste("SP-",modelo,ano,meses[mes],sep=" ")) 

 ggmap(map)  %+% gg+aes(x,y)+
      geom_polygon(data=sp.df, aes(long,lat,group=group ),fill = "white" , colour="white",alpha=0,size= 0.2) +
      geom_tile(aes(fill=brks,alpha=0.6 ))+
      scale_fill_manual("mm",values= color)+
      ggtitle(paste("SP_ -",modelo,ano,meses[mes],sep=" ")) +
      coord_equal()




