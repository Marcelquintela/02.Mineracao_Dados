rm(list=ls())
setwd("C:\\Users\\Marcel\\Dropbox\\00-Ciencias-de-Dados\\M02_Mineracao\\Atividades\\ATV03_M02_Mineracao")

Nutri<-read.csv("malnutrition.csv")

Nutri<-cbind(Nutri,Income=factor(Nutri[,3], levels = c(0:3),
                                 labels = c("Baixa","Média Baixa","Média Alta","Alta")))

Nutri<-cbind(Nutri,x=0,y=0)

for (a in 0:3){
  pos<-sample(seq(0.5,5,0.1),length(Nutri[Nutri[,3]==a,10]),replace = F)
  neg<-sample(seq(-5,-0.5,0.1),length(Nutri[Nutri[,3]==a,10]),replace = F)
  if (a==0){
    Nutri[Nutri[,3]==a,10]<-pos
    Nutri[Nutri[,3]==a,11]<-neg
  } else if (a==1){
    Nutri[Nutri[,3]==a,10]<-neg
    Nutri[Nutri[,3]==a,11]<-neg
  }else if (a==2){
    Nutri[Nutri[,3]==a,10]<-neg
    Nutri[Nutri[,3]==a,11]<-pos
  }else {
    Nutri[Nutri[,3]==a,10]<-pos
    Nutri[Nutri[,3]==a,11]<-pos
  }
}

p <- ggplot(Nutri,aes(x, y, label = Income ,color=Country)) +
  geom_point() +
  theme_void() + 
  theme(legend.position="none",
        axis.line=element_blank())+
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  scale_x_continuous(expand = c(0.1,0.1)) + 
  scale_y_continuous(expand = c(0.1,0.1)) +
  annotate(geom="text", x=4, y=5.5, label="Renda Alta",color="red") +
  annotate(geom="text", x=4, y=-5.5, label="Renda Baixa",color="red") +
  annotate(geom="text", x=-4, y=-5.5, label="Renda Média Baixa",color="red") +
  annotate(geom="text", x=-4, y=5.5, label="Renda Média Alta",color="red")

ggplotly(p, tooltip = c("label","colour"))
