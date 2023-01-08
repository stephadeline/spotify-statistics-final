#Retrieve the data saved AFTER the profiling practice...... this means data already cleaned

# Uncomment if the code will look for 'cluster' package
# install.packages("cluster")

library("tidyverse")

# setwd("D:/karina/docencia/areferenciesPPT/DadesPractiques/CREDSCO")
setwd("C:/Users/Joseph/Documents/Codes/2022/mvtec-2022/mvtec-statsprogramming/statsprog-09")
# dd <- read.csv("data/credscoClean.csv", sep=";");
raw <- read.csv("data/spotify.csv", sep=",");

set.seed(1)
sample <- sample_n(raw, 1000)
View(sample)

dd <- sample %>% select(popularity, duration_ms, danceability,
         energy, key, loudness,
         speechiness, acousticness, # mode, # excluded as considered binary
         instrumentalness, liveness, valence,
         tempo, time_signature)
names(dd)
dim(dd)
summary(dd)

attach(dd)

#set a list of numerical variables
names(dd)

# dcon <- data.frame (Antiguedad.Trabajo,Plazo,Edad,Gastos,Ingresos,Patrimonio,Cargas.patrimoniales,Importe.solicitado,Precio.del.bien.financiado,Estalvi, RatiFin)
# dcon <- data.frame(popularity, duration_ms, danceability,
#                    energy, key, loudness,
#                    speechiness, acousticness, # mode, # excluded as considered binary
#                    instrumentalness, liveness, valence,
#                    tempo, time_signature)
dcon <- dd
dim(dcon)

#
# CLUSTERING
#



# KMEANS RUN, BUT HOW MANY CLASSES?

k1 <- kmeans(dcon,5)
names(dcon)
print(k1)

attributes(k1)

k1$size

k1$withinss

k1$centers

# LETS COMPUTE THE DECOMPOSITION OF INERTIA

Bss <- sum(rowSums(k1$centers^2)*k1$size)
Bss
Wss <- sum(k1$withinss)
Wss
Tss <- k1$totss
Tss

Bss+Wss

Ib1 <- 100*Bss/(Bss+Wss)
Ib1

# LETS REPEAT THE KMEANS RUN WITH K=5

k2 <- kmeans(dcon,5)
k2$size

Bss <- sum(rowSums(k2$centers^2)*k2$size)
Bss
Wss <- sum(k2$withinss)
Wss

Ib2 <- 100*Bss/(Bss+Wss)
Ib2
Ib1

k2$centers
k1$centers

plot(k1$centers[,3],k1$centers[,2])

table(k1$cluster, k2$cluster)

# WHY WE HAVE OBTAINED DIFFERENT RESULTS?, AND WHICH RUN IS BETTER?

# NOW TRY K=8

k3 <- kmeans(dcon,8)
k3$size

Bss <- sum(rowSums(k3$centers^2)*k3$size)
Wss <- sum(k3$withinss)

Ib3 <- 100*Bss/(Bss+Wss)
Ib3


# HIERARCHICAL CLUSTERING

d  <- dist(dcon[1:50,])
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

# d  <- dist(dcon) # Getting an error of 'Error: cannot allocate vector of size 48.4 Gb'
d  <- dist(dcon) # Cut down data to top 1000
h1 <- hclust(d,method="ward")  # NOTICE THE COST
# The "ward" method has been renamed to "ward.D"; note new "ward.D2"
plot(h1)


# BUT WE ONLY NEED WHERE THERE ARE THE LEAPS OF THE HEIGHT

# WHERE ARE THER THE LEAPS? WHERE WILL YOU CUT THE DENDREOGRAM?, HOW MANY CLASSES WILL YOU OBTAIN?

nc = 3

c1 <- cutree(h1,nc)

c1[1:20]

nc = 5

c5 <- cutree(h1,nc)

c5[1:20]


table(c1)
table(c5)
table(c1,c5)


cdg <- aggregate(as.data.frame(dcon),list(c1),mean)
cdg

plot(cdg[,1], cdg[,7])

# LETS SEE THE PARTITION VISUALLY


# plot(Edad,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
plot(popularity ,duration_ms ,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3))


# plot(RatiFin,Estalvi)
# plot(RatiFin,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
plot(danceability, popularity)
plot(danceability, popularity,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

# plot(Antiguedad.Trabajo,Estalvi,col=c1,main="Clustering of credit data in 3 classes")
# legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)
# plot(Patrimonio, Ingresos,col=c1,main="Clustering of credit data in 3 classes")
# legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)
# plot(Patrimonio, Antiguedad.Trabajo,col=c1,main="Clustering of credit data in 3 classes")
plot(key, popularity, col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)
plot(loudness, speechiness,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)
plot(loudness, acousticness,col=c1,main="Clustering of credit data in 3 classes")
legend("topright",c("class1","class2","class3"),pch=1,col=c(1:3), cex=0.6)

pairs(dcon[,1:7], col=c1)

#plot(FI[,1],FI[,2],col=c1,main="Clustering of credit data in 3 classes")
#legend("topleft",c("c1","c2","c3"),pch=1,col=c(1:3))

# LETS SEE THE QUALITY OF THE HIERARCHICAL PARTITION



Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))

Ib4 <- 100*Bss/Tss
Ib4


#move to Gower mixed distance to deal 
#simoultaneously with numerical and qualitative data

library(cluster)

#dissimilarity matrix

#-- IMPORTANT PART HERE
# actives<-c(2:16)
actives<-c(6, 7, 9:12, 14:20) # Selecting according to the spotify dataset
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE) 

distMatrix<-dissimMatrix^2
#--

#-- INSTRUCTION THAT RUN THE HEIRARCHICAL CLUSTERING
h1 <- hclust(distMatrix,method="ward.D")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot
#--

plot(h1)

#-- ASKING TO CUT THE TREE
c2 <- cutree(h1,4)
#--

#class sizes 
table(c2)

#comparing with other partitions
table(c1,c2)


names(dd)
#time_signature
boxplot(dd[,13]~c2, horizontal=TRUE)

#energy
boxplot(dd[,4]~c2, horizontal=TRUE)

#instrumentalness
boxplot(dd[,9]~c2, horizontal=TRUE)

pairs(dcon[,1:7], col=c2)

plot(popularity,danceability,col=c2,main="Clustering of credit data in 3 classes")
legend("topright",levels(c2),pch=1,col=c(1:4), cex=0.6)

cdg <- aggregate(as.data.frame(dcon),list(c2),mean)
cdg

# plot(Edad, Gastos, col= c2)
plot(popularity, duration_ms, col= c2)
points(cdg[,4],cdg[,5],pch=16,col="orange")
text(cdg[,4],cdg[,5], labels=cdg[,1], pos=2, font=2, cex=0.7, col="orange")

potencials<-c(3,4,6,7,10,11)
pairs(dcon[,potencials],col=c2)

#Profiling plots
