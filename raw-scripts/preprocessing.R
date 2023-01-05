#  READING CREDSCO.CSV. NOTE: Change the path of the file for the proper one in your computer

#Note: Take care to use "/" fo the directory file. "\" provides errors
#Note: Update the working directory to your own path

setwd("D:/karina/docencia/01areferenciesPPT/0DadesPractiques/CREDSCO")

dd <- read.table("credsco.csv",header=T);

#is R reading data correctly?


dd <- read.table("credsco.csv",header=T, sep=";");

#Has dd the correct number of rows and columns?
dim(dd)
n<-dim(dd)[1]
n
K<-dim(dd)[2]
K

#is dd the expected type of object?
class(dd)

#check column contents
names(dd)
View(dd)

#open access by name to columns
attach(dd)

#are all columns of expected types?

summary(Dictamen)
boxplot(Dictamen)
class(dd[,1])
class(Dictamen)
sapply(dd, class)

# if numerical variables are taken as FACTORS, include proper "dec" parameter
#dd <- read.table("credsco.csv",header=T, sep=";", dec=".");

#open access by name to columns
attach(dd)

# DECLARE CATEGORICAL 
#identify which categorical are not recognized by the system yet
sapply(dd, class)

#declare factors
attach(dd)

names(dd)
class(dd[,6])



Dictamen    <- as.factor(Dictamen)
Vivienda     <- factor(Vivienda)
Estado.civil <- factor(Estado.civil)
Registros   <- factor(Registros)
Tipo.trabajo <-factor(Tipo.trabajo)

# R knows that Dictament is not numeric now
mean(Dictamen)

class(Dictamen)
table(Dictamen)

actives<-c(2:14)
dd2<-dd
dd<-dd[,actives]

#look at modalities
K<-dim(dd)[2]
dfactor<-c(2:K)
levelsVars<-sapply(dd[,dfactor],levels)
levelsVars[5]

#care with "attach" effects
levels(Dictamen)
levels(Vivienda)
levels(Estado.civil)
levels(Registros)
levels(Tipo.trabajo)

#consistency issues derived from "attach" function
class(Dictamen)
class(dd[,1])
summary(Dictamen)
summary(dd[,1])
barplot(table(Dictamen))
pie(table(Dictamen))

#internal coertion. NOT ALLWAYS
barplot(table(dd[,1]))

#INTERPRETABILITY, EXPLAINABILITY
#labelling modalities. Check metadata. 
#WARNING: Sequential assignment with levels

levels(Dictamen) <- c(NA, "positiu","negatiu")
table(Dictamen)

#care with missing data!!!!
table(Dictamen, useNA="ifany")

#What about ORDINAL VARIABLES?
barplot(table(Tipo.trabajo))

Tipo.trabajo<-factor(Tipo.trabajo, levels=c( "1", "2", "3", "4", "0"), labels=c("fixe","temporal","autonom","altres sit", "WorkingTypeUnknown"))
pie(table(Tipo.trabajo))
barplot(table(Tipo.trabajo))

#zoom the barplot to see all levels in the X axis

#ordering modalities! For ordinal variables 
Tipo.trabajo <- factor(Tipo.trabajo, ordered=TRUE,  levels= c("WorkingTypeUnknown","temporal","fixe","autonom","altres sit"))
frecs<-table(Tipo.trabajo)
barplot(frecs)

#recodificacions. Find short acronims of modalities for efficient data visualitation
print(frecs)
newvalues<-c("WTUnk","Fix","Temp","Auto","Other")
Tipo.trabajo <- newvalues[ match(Tipo.trabajo,
                                 c("WorkingTypeUnknown","fixe",                                             
                                   "temporal","autonom","altres sit"
                                 )
) 
]

table(Tipo.trabajo)
frecs<-table(Tipo.trabajo)
barplot(frecs, las=3, cex.names=0.7, main=paste("Barplot of", "Tipo.trabajo"))

#labelling of other factors in the dataset, be sure your assignment corresponds with the order of modalities
#if you doubt, use the match instruction shown above

levels(Vivienda) <- c("VivUnkown", "lloguer","escriptura","contr_privat","ignora_cont","pares","altres viv")
levels(Estado.civil) <- c("ECUnknown", "solter","casat","vidu","separat","divorciat")
levels(Registros) <- c("reg_no","reg_si")

#propagate preprocessing to dataframe
class(Dictamen)
class(dd[,1])
dd[,1]<-Dictamen
class(dd[,1])

dd[,3]<-Vivienda
dd[,6]<-Estado.civil
dd[,7]<-Registros
dd[,8]<-Tipo.trabajo

class(dd[,1])

#Export pre-processed data to persistent files, independent from statistical package
write.table(dd, file = "credscoCategoriques.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

