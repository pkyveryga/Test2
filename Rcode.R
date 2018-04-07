##   This is a Test for using GitHub for the Stat 502 Kaggle Project


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



setwd("C:/Peter/STAT/STAT502X_Modern_Multivariate/2018/ISU_Kaggle_Project/GitHub/Try1/Test2/Test2")
####this comment added online on github - branch_test_bernd
#this comment added in RStudio - Bernd

test.data=read.csv("test.csv")
head(test.data)

train=read.csv("train.csv")
head(train)



##The last collumns are Month and Day

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


### spatial Variogram--Checking Spatial Correlation 

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

###LISA Model-- checking about spatial "hot spots in the 

##Reducing the data size because it says about lack of memory
ncol(train)
nrow(train)
train1=train[1:10000,]

train.lisa <- lisa(train$lat, train$long, train$price, neigh=10, resamp=10, quiet=TRUE)

train.lisa =plot.lisa(train.lisa, negh.mean=FALSE)

##Increasing memory size
memory.limit(size=50000)

correlog(train1$long, train1$lat, train1$price, increment=20, resamp=500, quiet=TRUE) 

plot(price.clg)


##checking  Correlation Matrices 

library(PerformanceAnalytics)

train_con=train[,3:6]

chart.Correlation(train[, c(3,18:21)], histogram=TRUE, pch=10)


############################
###Stacking Several Models 
## Here without predefined model parameters. 
library(psych)
library(caret)
library(randomForest)
library(caretEnsemble)

head(train)
##The dataset without Lat and Long and the 
train1=train[,-c(1,2,20, 21,22,23)]
head(train1)
head(train1)

##removing several columns to match the test data
train2=train1[,-c(18, 19)]
head(train2)

seed=111

control <- trainControl(method="repeatedcv", number=10, repeats=50, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c( 'pcr','pls','rpart', 'glmnet')
set.seed(seed)
stack_models <- caretList(price~., data=train2, trControl=control, methodList=algorithmList)
stacking_results <- resamples(stack_models)

summary(stacking_results)
dotplot(stacking_results)
# Check correlation between models to ensure the results are uncorrelated and can be ensembled
modelCor(stacking_results)
splom(stacking_results)

# stacking using Linear Regression-
stackControl <- trainControl(method="repeatedcv", number=10, repeats=50, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.lm <- caretStack(stack_models, method="lm", trControl=stackControl)
print(stack.lm)
summary(stack.lm)


##remove date
head(test.data)
test.data1=test.data[,-c(3)]

head(train1)

##Predictions
predictions=predict(stack.lm, newdata=test.data1) 
               
### 




               
               
               pred_df<-as.data.frame(cbind(1:11613,predictions))
               colnames(pred_df)<-c("id","price")
               
               head(pred_df)
               
               write.csv(x=pred_df,file="submission_April_2.csv",row.names = FALSE)


# Root Mean Square Log Loss Error: Check!!!

library (MLmetrics)
head(train)

##Checking with variables that do not have zeros
model1=c("bedrooms","bathrooms","sqft_living")

reg <- lm(log(price) ~ log(sqft_living+sqft_lot), data = train1) 
summary(reg)

reg <- lm(log(price) ~ sqft_living+sqft_lot+month, data = train) 


RMSLE(y_pred = exp(reg$fitted.values), y_true = log(train$price))
###10.40574
plot(reg$fitted.values, train$price)

apply(train,2,min)


#####  Next steps

## Bernd's RF --NO Lat and Long
require(randomForest)
start.time<-Sys.time()
rf<-randomForest(price~.,data=train1,
                 type="regression",ntree=100,mtry=4)
Sys.time()-start.time
1

predictions<-predict(object=rf,newdata=test.data)

pred_df<-as.data.frame(cbind(1:11613,predictions))
colnames(pred_df)<-c("id","price")

head(pred_df)

write.csv(x=pred_df,file="submission_test.csv",row.names = FALSE)