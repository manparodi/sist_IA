library(rpart)
library(caret)
library(nnet)
#Parte A
base=read.table("vowel.train",sep=",",header=TRUE)
head(base)
base$row.names=NULL
names(base)[names(base)=="y"]="VocalCorrespondiente"
base$VocalCorrespondiente=factor(base$VocalCorrespondiente,
                                 levels=c(1,2,3,4,5,6,7,8,9,10,11),
                                 labels=c("i","I","E","A","a:","Y","O","C:","U","u:","3:"))
head(base)
dim(base)
xyplot(x.1~x.2,base,groups=base$VocalCorrespondiente,
       auto.key=list(columns=3),main="X.1 - X.2",pch=19)
miVocal=base[17,]
miVocal

#------------------------------------------------------------------------------#

#Parte B
set.seed(61517);particion=createDataPartition(y=base$VocalCorrespondiente,
                                               p=0.75,list=FALSE)
entreno=base[particion,]
testeo=base[-particion,]
head(entreno);summary(entreno)
head(testeo);summary(testeo)
table(base$VocalCorrespondiente)
table(entreno$VocalCorrespondiente)
table(testeo$VocalCorrespondiente)
#Quedaron 36 registros por vocal en entrenamiento y 12 en testeo

#------------------------------------------------------------------------------#

#Parte C
library("e1071")
svm=svm(VocalCorrespondiente~.,entreno,kernel="sigmoid")
svm
#El svm type es de tipo clasificacion y hay 362 vectores
pred=predict(svm,testeo)
confusionMatrix(pred,testeo$VocalCorrespondiente)
#El accuracy fue de 0.5227
predict(svm,miVocal)
miVocal
#No coincide, la vocal es Y y predice que sera "a:"

#------------------------------------------------------------------------------#

#Parte D
svm2=svm(VocalCorrespondiente~.,entreno, cost= 50,gamma=0.1,kernel="radial")
svm2
#hay 265 vectores 
pred=predict(svm2,testeo)
confusionMatrix(pred,testeo$VocalCorrespondiente)
predict(svm2,miVocal)
miVocal
#Si, coincide

#------------------------------------------------------------------------------#

#Parte E
nb=naiveBayes(VocalCorrespondiente~.,entreno)
pred=predict(nb,testeo)
confusionMatrix(pred,testeo$VocalCorrespondiente)
predict(nb,miVocal)
#No coincide

################################################################################

#Ejercicio 2
base1=read.csv("Advertising.csv",sep=",",header=TRUE)
head(base1)
base1$X=NULL
str(base1)
dim(base1)
plot(base1$TV,base1$sales,main= "Grafico de Dispersion", pch=19)
cor(base1$TV,base1$sales,method="pearson")
#Estan altamente correlacionadas

#------------------------------------------------------------------------------#

#Parte B
set.seed(61517);particion=createDataPartition(y=base1$sales,p=0.75,list=FALSE)
entreno=base1[particion,]
testeo=base1[-particion,]
head(entreno);summary(entreno)
head(testeo);summary(testeo)
dim(entreno);dim(testeo)
#Quedaron 151 registros en entreno y 49 en testeo

#------------------------------------------------------------------------------#

#Parte C
#Red Neuronal
set.seed(61517);red=nnet(sales~.,entreno,size=20,maxit=10000,linout=TRUE)
red
pred=predict(red, testeo) 
ECMRN=mean((pred-testeo$sales)^2)
ECMRN
install.packages("NeuralNetTools")
library(NeuralNetTools)
plotnet(red)
#Arbol de decision
arbol=rpart(sales~., entreno) 
pred=predict(arbol, testeo)
ECMArbol=mean((pred-testeo$sales)^2)
ECMArbol
library(rpart.plot)
rpart.plot(arbol,extra=1,type=5,cex=0.7)
#SVM
svm=svm(sales~.,entreno,kernel="sigmoid")
svm
pred=predict(svm, testeo)
ECMSvm=mean((pred-testeo$sales)^2)
ECMSvm







