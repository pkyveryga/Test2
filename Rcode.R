
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


