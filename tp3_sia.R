#  TP3 Sistema de inteligencia artificial

library(jpeg)

imagen = readJPEG("C:\\Users\\agustin\\Downloads\\atardecer.jpg")

plot(as.raster(imagen))

gris=(imagen[,,1]+imagen[,,2]+imagen[,,3])/3
plot(as.raster(gris))

set.seed(61244)

#segmentacion 3 grupos

rojo=as.vector(imagen[,,1])
verde=as.vector(imagen[,,2])
azul=as.vector(imagen[,,3])
base=data.frame(rojo,verde,azul)
set.seed(123)
km=kmeans(base,5)

segmR=rojo
segmV=verde
segmA=azul



segmR[km$cluster==1]=1
segmV[km$cluster==1]=0
segmA[km$cluster==1]=0

segmR[km$cluster==2]=0
segmV[km$cluster==2]=1
segmA[km$cluster==2]=0

segmR[km$cluster==3]=1
segmV[km$cluster==3]=0
segmA[km$cluster==3]=1


segmentada=imagen
segmentada[,,1]=segmR
segmentada[,,2]=segmV
segmentada[,,3]=segmA
plot(as.raster(segmentada))


dim(imagen)

km$size

igualar suma de pixeles con el total de la suma del size

km$centers

plot(1,1,pch=19,cex=10,col=rgb(km$center[1,1],km$center[1,2],km$center[1,3]))
plot(1,1,pch=19,cex=10,col=rgb(km$center[2,1],km$center[2,2],km$center[2,3]))
plot(1,1,pch=19,cex=10,col=rgb(km$center[3,1],km$center[3,2],km$center[3,3]))


#segmentacion 6 grupos



rojo1=as.vector(imagen[,,1])
verde1=as.vector(imagen[,,2])
azul1=as.vector(imagen[,,3])
base=data.frame(rojo1,verde1,azul1)
set.seed(123)
km1=kmeans(base,6)


#Reconstruir la imagen segmentada
segR=rojo
segV=verde
segA=azul



segR[km$cluster==1]=1
segV[km$cluster==1]=0
segA[km$cluster==1]=0

segR[km$cluster==2]=1
segV[km$cluster==2]=1
segA[km$cluster==2]=0

segR[km$cluster==3]=1
segV[km$cluster==3]=0
segA[km$cluster==3]=1



segR[km$cluster==4]=0
segV[km$cluster==4]=1
segA[km$cluster==4]=1


segR[km$cluster==5]=0
segV[km$cluster==5]=1
segA[km$cluster==5]=0

segR[km$cluster==6]=0
segV[km$cluster==6]=0
segA[km$cluster==6]=1






segmentada1=imagen
segmentada1[,,1]=segR
segmentada1[,,2]=segV
segmentada1[,,3]=segA
plot(as.raster(segmentada1))

#segmentacion 8 grupos




rojo1=as.vector(imagen[,,1])
verde1=as.vector(imagen[,,2])
azul1=as.vector(imagen[,,3])
base=data.frame(rojo1,verde1,azul1)
set.seed(123)
km1=kmeans(base,8)


#Reconstruir la imagen segmentada
segR=rojo
segV=verde
segA=azul



segR[km$cluster==1]=1
segV[km$cluster==1]=0
segA[km$cluster==1]=0

segR[km$cluster==2]=1
segV[km$cluster==2]=1
segA[km$cluster==2]=0

segR[km$cluster==3]=1
segV[km$cluster==3]=0
segA[km$cluster==3]=1



segR[km$cluster==4]=0
segV[km$cluster==4]=1
segA[km$cluster==4]=1


segR[km$cluster==5]=0
segV[km$cluster==5]=1
segA[km$cluster==5]=0

segR[km$cluster==6]=0
segV[km$cluster==6]=0
segA[km$cluster==6]=1



segR[km$cluster==7]=0
segV[km$cluster==7]=1
segA[km$cluster==7]=1

segR[km$cluster==8]=1
segV[km$cluster==8]=1
segA[km$cluster==8]=1



segmentada1=imagen
segmentada1[,,1]=segR
segmentada1[,,2]=segV
segmentada1[,,3]=segA
plot(as.raster(segmentada1))

#EJERCICIO 2

install.packages('ISLR')
library(ISLR)
library(kohonen)
data(Auto)


Auto$year=NULL
Auto$origin=NULL
Auto$name=NULL

dim(base) #en la base hay 392 autos

matrizAutos = as.matrix(Auto)

som = som(matrizAutos, grid = somgrid(1, 4, 'hexagonal'))
plot(som)

cantidad=plot(som,type="count")

#4 y 5 ni idea

som$codes

som$unit.classif

#8 ni idea

#RECONSTRUIR LA IOMAGEN CON KOHONEN
