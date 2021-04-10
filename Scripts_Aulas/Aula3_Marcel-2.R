list_pkgs <- c("stringr","wordcloud","tm","ggplot2","wordcloud2")
new.packages <- list_pkgs[!(list_pkgs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE, INSTALL_opts = '--no-lock')

library(tm)
library(ggplot2)
library(wordcloud)
library(wordcloud2)

dados <- read.csv("WomensCloths.csv")

b<- Corpus(VectorSource(dados$Title))
inspect(b)
b <- tm_map(b, removePunctuation)
b <- tm_map(b, stripWhitespace)
b <- tm_map(b,content_transformer(tolower))
b <- tm_map(b, removeNumbers)

Tirar<-function(x) iconv(x, "UTF-8", to = "ASCII//TRANSLIT") #remove todos os Acentos

for (i in seq(b)){
  b[[i]]<-Tirar(b[[i]])
}

myStopwords <- c(stopwords('en'),readLines("Stopwords_English.txt"))
c <- tm_map(b, removeWords, myStopwords)


data.Dtm = DocumentTermMatrix(c,control=list(wordLengths=c(3,50),bounds=list(global = c(4,Inf))))

#inspecionar as palavras mais frequentes
findFreqTerms(data.Dtm, lowfreq=250)

#Encontra associações com
findAssocs(data.Dtm, 'great',0.05)
findAssocs(data.Dtm, 'great',corlimit=0.05)

plot(data.Dtm, terms=findFreqTerms(data.Dtm,lowfreq=300),corThreshold=0.05,
     attrs=list(node=list(shape="ellipse",fixedsize=FALSE,fontsize="25"),edge=list(color="black")))

#Frequencias
mat = as.matrix(data.Dtm)
freq = sort(colSums(mat), decreasing=TRUE)
Freq = data.frame(word=names(freq), freq=freq)
(Freq <- data.frame(Freq,Freq_r=prop.table(Freq$freq)))

#Gráfico de frequencias
p <- ggplot(subset(Freq,freq>350),aes(word,freq))
p <- p + ggtitle("Frequência de maiores frequências de palavras nos títulos")
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45,hjust=1))
plot(p)

#Nuvem de Palavras
wordcloud(words=Freq$word,freq=Freq$freq,scale=c(3,0.3), min.freq=20, max.words=1000, random.order=F,
          random.color=1,rot.per=.2,use.r.layout=T, colors=brewer.pal(5, "Dark2"))
wordcloud2(Freq, shape="circle",minSize = 50 )
