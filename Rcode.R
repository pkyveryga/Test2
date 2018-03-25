
library(caret)
library(psych)
library(ggplot2)

library(sp)
library(gstat)
library(PerformanceAnalytics)
library(corrplot)
library(psych)

library(ggplot2)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#F0E442","#CC79A7","#000000","#734f80", "#2b5a74", "#004f39", "#787221", "#003959", "#6aaf00", "#663cd3")




setwd("C:/Peter/STAT/STAT502X_Modern_Multivariate/2018/ISU_Kaggle_Project/Data")

test=read.csv("test.csv")
head(test)
train=read.csv("train.csv")
head(train)


##Ploting Spatial Data


ggplot(train, aes(lat, long)) + 
  geom_point(aes(colour = price),size=0.5) + 
  scale_colour_gradient(low=cbPalette[7], high=cbPalette[4]) +
  labs(colour = "House Price, $", x="Longitude ", y="Latitude ", title = "House Price, $")


ggplot(train, aes(lat, long)) + 
  geom_point(aes(colour = zipcode ),size=0.5) + 
  scale_colour_gradient(low=cbPalette[7], high=cbPalette[4]) +
  labs(colour = "zipcode ", x="Longitude ", y="Latitude ", title = "zipcode ")

##change
##


###Variogram

library(gstat)
library(geoR)
library(ggplot2)
library(ncf)
library(ape)

price.var <- variogram(price~1, 
                       locations=~lat+long, 
                       data=train)
print(price.vgm <- fit.variogram(price.var, vgm(250, "Sph", 80, 100)))
plot(price.var, model=price.vgm)

###LISA Model


ncol(train)
train1=train[1:1000,]

train.lisa <- lisa(train$lat, train$long, train$price, neigh=10, resamp=100, quiet=TRUE)

train.lisa =plot.lisa(train.lisa, negh.mean=FALSE)


memory.limit(size=50000)

correlog(train1$long, train1$lat, train1$price, increment=20, resamp=500, quiet=TRUE) 

plot(price.clg)

