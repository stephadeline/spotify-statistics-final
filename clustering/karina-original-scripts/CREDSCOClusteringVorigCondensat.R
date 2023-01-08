#Retrieve the data saved AFTER the preprocessing practice...... this means data already cleaned

setwd("D:/karina/docencia/areferenciesPPT/0DadesPractiques/CREDSCO")
dd <- read.csv("credscoClean.csv", sep=";");
names(dd)
dim(dd)
summary(dd)

attach(dd)

#set a list of numerical variables
names(dd)

#hierarchical clustering

#euclidean distance si totes son numeriques
dcon<-data.frame (Antiguedad.Trabajo,Plazo,Edad,Gastos,Ingresos,Patrimonio,Cargas.patrimoniales,Importe.solicitado,Precio.del.bien.financiado,Estalvi, RatiFin)

d  <- dist(dcon[1:10,])

#move to Gower mixed distance to deal 
#simoultaneously with numerical and qualitative data

library(cluster)

#dissimilarity matrix
#do not include in actives the identifier variables nor the potential response variable

actives<-c(2:16)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot

plot(h1)

k<-4

c2 <- cutree(h1,k)

#class sizes 
table(c2)

#comparing with other partitions
#table(c1,c2)

# LETS SEE THE PARTITION VISUALLY

c1<-c2
plot(Edad,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3","class4"),pch=1,col=c(1:k))



plot(RatiFin,Estalvi)
plot(RatiFin,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

plot(Antiguedad.Trabajo,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

pairs(dcon[,1:7], col=c1)

#plot(FI[,1],FI[,2],col=c1,main="Clustering of credit data in 3 classes")
#legend("topleft",c("c1","c2","c3"),pch=1,col=c(1:3))

# LETS SEE THE QUALITY OF THE HIERARCHICAL PARTITION

cdg <- aggregate(as.data.frame(dcon),list(c1),mean)
cdg

plot(cdg[,1], cdg[,7])

#Tss <- h1$totss
#Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))

#Ib4 <- 100*Bss/Tss
#Ib4



names(dd)
#ratiFin
boxplot(dd[,16]~c2, horizontal=TRUE)

#plazo
boxplot(dd[,4]~c2, horizontal=TRUE)

#gastos
boxplot(dd[,9]~c2, horizontal=TRUE)

pairs(dcon[,1:7], col=c2)

plot(RatiFin,Estalvi,col=c2,main="Clustering of credit data in 3 classes")
legend("topright",levels(c2),pch=1,col=c(1:4), cex=0.6)

cdg <- aggregate(as.data.frame(dcon),list(c2),mean)
cdg

plot(Edad, Gastos, col= c2)
points(cdg[,4],cdg[,5],pch=16,col="yellow")
text(cdg[,4],cdg[,5], labels=cdg[,1], pos=2, font=2, cex=1.2, col="yellow")

potencials<-c(3,4,6,7,10,11)
pairs(dcon[,potencials],col=c2)

tt<-table(Tipo.trabajo,c2)

barplot(table(Tipo.trabajo, c2), beside=TRUE,col=c(1:length(levels(Tipo.trabajo))) )
legend("topright",levels(Tipo.trabajo),pch=1,cex=0.5, col=c(1:length(levels(Tipo.trabajo))))


barplot(table(Tipo.trabajo, c2), beside=FALSE,col=c(1:length(levels(Tipo.trabajo))) )
legend("topright",levels(Tipo.trabajo),pch=1,cex=0.5, col=c(1:length(levels(Tipo.trabajo))))

barplot(table(Tipo.trabajo, c2), beside=FALSE,col=c(1:length(levels(Tipo.trabajo))) )

#Profiling plots