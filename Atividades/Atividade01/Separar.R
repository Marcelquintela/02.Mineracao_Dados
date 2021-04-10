# entativa de quebra do data frame em 2 sendo um de vendas e outro de características
x2<-unique(vendas[,1:2])
x1<-dplyr::distinct(vendas,id)
vend<-vendas[vendas$id==x2,]

vendas[vendas$id %in% x2, ]


dados <- read.csv("C:/Users/Marcel/Dropbox/00 Ciencias de Dados/M02_Mineracao/bestsellers with categories.csv")
attach(dados)
autortab <- table(Author)   # frequência absoluta para cada autor
ind <- which.max(autortab)   # índice do máximo
ind

x<-data.frame(table(vendas$id))

which(x>=2)

vendas[vendas$id=="795000620",]
vendas[,1]


a <- c(rep("A", 3), rep("B", 3), rep("C",2))
b <- c(1,1,2,4,1,1,2,2)
df <-data.frame(a,b)
df[duplicated(df), ]

unique(df[ ,])
