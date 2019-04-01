#Parcial Aplicado ACM
#Kevin García - 1533173
#Alejandro Vargas - 1525953
datos=read.table (file = "clipboard", header = TRUE, row.names = 1, sep = "\t", dec = ".")
View(datos)
#quitamos los datos faltantes
datos=na.omit(datos)
#covertir las variables en factores
datos$P424=as.factor(datos$P424)
datos$P4030S1=as.factor(datos$P4030S1)
datos$P4030S1A1=as.factor(datos$P4030S1A1)
datos$P4030S5=as.factor(datos$P4030S5)
datos$P4030S3=as.factor(datos$P4030S3)
datos$P4030S4=as.factor(datos$P4030S4)
datos$P4030S4A1=as.factor(datos$P4030S4A1)
datos$P4030S2=as.factor(datos$P4030S2)
datos$REGION=as.factor(datos$REGION)
str(datos)
length(datos$P424)#numero de individuos
#####__###____#____#___#
#quitamos el 60% de los datos
#g=c(sample(1:length(datos$P424),0.95*length(datos$P424)))
#datos=datos[-g,]
#datos=datos[,-4]
#####__###____#____#___#
#descritivas
#graficar cada una de las modalidades segun sus ocurrencias
x11()
par(mfrow=c(3,3))
for (i in 1:length(datos)) {
  plot(datos[,i])
  
}

#para encontrar la incercia de la modalidad si en la pregunta del gas natural
j=c()
for (i in 1:length(datos$P4030S2)) {
  if(datos$P4030S4A1[i]==4){
    j=c(j,datos$P4030S4A1[i])
  }
  
}

sum(j)/length(datos$P424)
#Para realizar el ACM
install.packages("ade4")
install.packages("FactoMineR")
library("ade4")
mod.mca=dudi.acm(datos,scannf = FALSE, nf = 4)#modelo ACM
#grafico de valores propios
x11()
screeplot(mod.mca,main = "VARIANZA EXPLICADA")
#valores propios
summary(mod.mca)

mod.mca$cr

sum(mod.mca$eig)

#grafico de variables
x11()
s.corcircle(mod.mca$co, 1, 2, clabel = 0.7)
#grafico de individuos
x11()
s.corcircle(mod.mca$li, 1, 2, clabel = 0.7)
#correlacion graficos
x11()
par(mfrow = c(2, 2))
for (i in 1:4) barplot(mod.mca$cr[, i], names.arg = row.names(mod.mca$cr), las = 2, main = paste("Axe",i))

#representar la modalidades
x11()
s.label(mod.mca$co, clabel = 0.5)

#representacion de individuos
x11()
s.label(mod.mca$li, clabel = 0, pch = 20)

#en caso de tener mucho individuos con la misma combinacion de variables
install.packages("devtools")
library("devtools")
install_github("larmarange/JLutils")
#lo anterior es pra poder instalar
install.packages("JLutils")
library("JLutils")
x11()
s.freq(mod.mca$li)#no me deja bajar el paquete

#Con FactoMineR seria
install.packages("FactoMineR")
library("FactoMineR")
#Modelo
head(datos)
acm2 <- MCA(datos, ncp = 5, graph = FALSE )
acm2 <- MCA(datos, ncp = 5, graph = FALSE,quali.sup =7 )#con region como SUPL
acm2
#Contribuciones y concenos para las variables y modalidades
acm2$var$contrib
acm2$var$cos2
na.omit(acm2$var$cos2)

#para las contribuciones
x11()
par(mfrow = c(2,2 ))
for (i in 1:2) barplot(acm2$var$contrib[, i], names.arg = row.names(acm2$var$contrib), las = 2, main = paste("Dim",i))
for (i in 1:2) barplot(na.omit(acm2$var$cos2)[, i], names.arg = row.names(na.omit(acm2$var$cos2)), las = 2, main = paste("Dim",i))

#para los cocenos
x11()
par(mfrow = c(3,2 ))
for (i in 1:2) barplot(na.omit(acm2$var$cos2)[, i], names.arg = row.names(na.omit(acm2$var$cos2)), las = 2, main = paste("Dim",i))


#Contribuciones y cosenos para los individuos
acm2$ind$contrib
acm2$ind$cos2
x11()
par(mfrow = c(2,2 ))
for (i in 1:2) barplot(acm2$ind$contrib[, i], names.arg = row.names(acm2$ind$contrib), las = 2, main = paste("Dim",i))
for (i in 1:2) barplot(na.omit(acm2$ind$cos2)[, i], names.arg = row.names(na.omit(acm2$ind$cos2)), las = 2, main = paste("Dim",i))

u=c(1041524,10282113,1019294,1039511)
u=as.character(u)
for (i in 1:4) {
  acm2$ind$contrib[u[i],]
}
acm2$ind$cos2["1041524",]
acm2$ind$cos2["1038213",]
acm2$ind$cos2["1019294",]
acm2$ind$cos2["1039511",]


#grafica de individuos y variables
x11()
plot(acm2)

#solo individuos
x11()
plot.MCA(acm2, invisible=c("var","quali.sup"), cex=0.7)

#solo variables
x11()
plot.MCA(acm2, invisible=c("ind","quali.sup"), cex=0.7)

#para observar si existen diferencias significativas entre modalidades
x11()
plotellipses(acm2)

#valores propios
acm2$eig

####CLUSTER
##metodo k-means
install.packages("cluster")
install.packages("factoextra")
install.packages("FatoClass")
library(cluster)
library(factoextra)
library(FactoClass)
#determinacion de los cluster
#####-##
x11()
Y3D <- scatterplot3d (datos, main ="Y",type="h",color ="darkblue",box=FALSE)
Y3D$points3d(datos,pch=1)

#cluster
cluster1=FactoClass(datos,dudi.acm,nf = 3,nfcl = 3,scanFC = F,k.clust = 3)

x11()
plot(cluster1$cluster)

#representacion de individuos
x11()
clusplot(mod.mca$li,cluster2$cluster)


#otro grafico
x11()
plotFactoClass(cluster1,cframe=1,col.row=c("red","green","maroon2","orchid4","pink"))
write.table(data.frame(cluster1$cluster), file="salidas.txt")

head(datos)


#usando k-means
cluster2=kmeans(datos,centers = 3)
