# instalação de pacotes 
.packages = c("ggplot2","gridExtra","reshape2","ggcorrplot","factoextra")# Lista de bibliotecas necessárias
# Instalar (caso ainda não tenha sido instalado)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst],dependencies = TRUE)
# Carregando bibliotecas
lapply(.packages, require, character.only=TRUE)

white <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",sep=";")

graf <- ggplot(white,aes(quality)) + geom_histogram(bins=7, col="white",fill="blue")
gra <- graf + xlab("Qualidade")+ylab("Frequência")
graf

