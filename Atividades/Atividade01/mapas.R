library(leaflet)

setwd("C:\\Users\\Marcel\\Dropbox\\00 Ciencias de Dados\\M02_Mineracao\\Atividades\\ATV01_M02_Mineracao")

data1<-read.csv("kc_housing_1.csv",sep=",")
data2<-read.csv("kc_housing_2.csv",sep=",")

house<-merge(data1,data2,by=c("id","date"))
rm(data1,data2)
names(house)[20]<-"lng"

x<-house
x2<-unique(vendas$id)
x1<-dplyr::distinct(vendas,id)

#y<-x[1:100,c(4,3,1,2)]
x<-cbind(x[1:1000,],House=paste("house",c(1:1000)))
#selecionar as 100 casas mais caras

printMoney <- function(x){
  format(x, digits=10, nsmall=2, decimal.mark=",", big.mark=".")
}

leaflet(x) %>% 
  addTiles() %>% 
  addMarkers(popup = paste0(x$House,"; Preço: US$",printMoney(x$price)),
             label= ~House)


# EXERCÍCIO
# Plotarunidades de saúde por localidades

install.packages("cepR")
library(cepR)
T<-"dd4dc1e8e8438841211f1bdc0677dd9b"
busca_cep("20231045",token = "dd4dc1e8e8438841211f1bdc0677dd9b")
busca_cidades(estado = "Es", token = x )       

#token minha chave
#https://www.cepaberto.com/
#https://cran.r-project.org/web/packages/cepR/cepR.pdf

library(stringr)

# Dados do Cnes
x<-read.csv("C:\\Users\\Marcel\\Downloads\\BASE_DE_DADOS_CNES_202102\\tbEstabelecimento202102.csv",sep=";")
names(x)

x$CO_CEP
grep("^20",x$CO_CEP) 
CEP<-x$CO_CEP[str_length(x$CO_CEP)==8]#ceps com 8 digitos

CEPRJ<-CEP[grep("^20",CEP)]

a<-unique(as.character(CEPRJ)) #CEPs unicos | ver a possibilidade de unicos lat|long

z<-busca_multi(lista_ceps = a[1:20],token = T)


leaflet(z) %>% 
  addTiles() %>% 
  addMarkers(popup = paste0(z$bairro,"; tet",z$logradouro),
             label= ~cep)

