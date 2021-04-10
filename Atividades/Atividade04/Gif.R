library(leaflet)
library(animation)
library(png)
library(htmlwidgets)
library(webshot)
library(ggmap)
library(magick)
library(magrittr)
library(sf)
library(geobr)
library(tidyverse)
library(zoo)
#install_phantomjs(version = "2.1.1",
#                  baseURL = "https://github.com/wch/webshot/releases/download/v0.3.1/",
#                  force = FALSE)
Folder<-"C:\\Users\\Marcel\\Dropbox\\00-Ciencias-de-Dados\\M02_Mineracao\\Atividades\\ATV04_M02_Mineracao"
setwd(Folder)

covid<-read.csv("covid2.csv")
covidM<-aggregate(covid$new_confirmed,by=list(City=covid$city,
                                              Code_ibge=covid$city_ibge_code,
                                              Date=covid$date),FUN="sum")
covidM<-covidM %>% 
  arrange(Date)%>%
  arrange(City)%>%
  group_by(City)%>%
  mutate(mean_15=rollmean(x, 15, align="right",fill=0))



shp<-read_municipality(code_muni = "RJ", year= 2018)
shp2<-left_join(shp,covidM, by = c("code_muni"="Code_ibge"))

cores <- colorQuantile(heat.colors(5),NULL, reverse = T)

a<-unique(shp2$Date)
a<-as.Date(a,"%Y-%m-%d")

lag<-length(unique(shp2$Date))

for (i in (lag-120):lag){
  aux<- shp2[shp2$Date==a[i],]
  if (length(aux$City==92)){
    map<-leaflet(aux)%>% 
    addProviderTiles(provider = providers$CartoDB.Positron)%>%
    addPolygons(weight=0.5,
                color="black",
                label=~paste0(City,": ",round(mean_15,2)),
                fillColor = ~cores(mean_15),
                smoothFactor=0.5,
                opacity=0.8,
                fillOpacity=0.5,
                highlightOptions=highlightOptions(color="white",weight=5,bringToFront=TRUE))%>%
    leaflet::addLegend(pal=cores,values=~mean_15,opacity=0.5,
                       title = paste(format(a[i],"%d %b %Y"),
                                     "<br><br>Média Móvel<br>15 dias"))
    
    saveWidget(map, 'temp.html', selfcontained = FALSE)
    webshot('temp.html', file=sprintf('MapaGif\\Rplot%02d.png', i),
            cliprect = 'viewport')
  }
}

list.files(path=paste0(Folder,"\\MapaGif"), pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2, optimize = TRUE) %>% # animates, can opt for number of loops
  image_write("images\\mapa.gif") # write to current dir
