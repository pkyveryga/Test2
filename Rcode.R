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



setwd("C:/Peter/STAT/STAT502X_Modern_Multivariate/2018/ISU_Kaggle_Project/GitHub/Test2/")
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


head(train1)


#####  Next steps

## Bernd's RF --NO Lat and Long
require(randomForest)
start.time<-Sys.time()
rf<-randomForest(price~.,data=train1,
                 type="regression",ntree=100,mtry=4)
Sys.time()-start.time
1

predictions<-predict(object=rf,newdata=test.data)
head(predictions)

pred_df<-as.data.frame(cbind(1:11613,predictions))
colnames(pred_df)<-c("id","price")

head(pred_df)

write.csv(x=pred_df,file="submission_test.csv",row.names = FALSE)



##################################################################

###### April 7-Bernd's part

###generation of date features (new in this version)
##############
train.date<-as.Date(x=as.character(train$date),format="%Y%m%d")
train.year<-as.numeric(format(train.date,"%Y"))  ###use year as feature
train.day<-as.numeric(format(train.date,"%j"))   ## decimal day of year
train.day.norm<-((train.day-1)-364/2)/(364/2)  ##limit day range from -1 to 1
train.day.feat1<-train.day.norm
train.day.feat2<-train.day.norm^2
train.day.feat3<-train.day.norm^3
train.day.feat4<-train.day.norm^4

test.date<-as.Date(x=as.character(test.data$date),format="%Y%m%d")
test.year<-as.numeric(format(test.date,"%Y"))
test.day<-as.numeric(format(test.date,"%j"))
test.day.norm<-((test.day-1)-364/2)/(364/2)
test.day.feat1<-test.day.norm
test.day.feat2<-test.day.norm^2
test.day.feat3<-test.day.norm^3
test.day.feat4<-test.day.norm^4
################end of date feature generation

############################
###Stacking Several Models
## Here without predefined model parameters.
library(psych)
library(caret)
library(randomForest)
library(caretEnsemble)
head(train)
##The dataset without Lat and Long and the
train1=train[,-c(1,2,17,18,19)] ##changed, removed property, date, zipcode,lat, long
head(train1)
head(train1)
##removing several columns to match the test data
train2=train1[,-c(18, 19)]
head(train2)
train2<-cbind(train2,train.year,train.day.feat1,train.day.feat2,train.day.feat3,train.day.feat4) ####added
head(train2)
seed=111
control <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c( 'pcr','pls','rpart', 'glmnet')
set.seed(seed)
stack_models <- caretList(log(price)~., data=train2, trControl=control, methodList=algorithmList)
stacking_results <- resamples(stack_models)
summary(stacking_results)
dotplot(stacking_results)
# Check correlation between models to ensure the results are uncorrelated and can be ensembled
modelCor(stacking_results)
splom(stacking_results)
# stacking using Linear Regression- or Random Forest
stackControl <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.lm <- caretStack(stack_models, method="rf", trControl=stackControl)
print(stack.lm)
summary(stack.lm)


#Checking train2
head(train2)
##remove date
head(test.data)
test.data1=test.data[,c(-1,-2,-3,-17,-18,-19)] ##changed, removed id, property, date, zipcode,lat, long
test.data1<-cbind(test.data1,test.year,test.day.feat1,test.day.feat2,test.day.feat3,test.day.feat4) ##added
head(test.data1)
ncol(test.data1)
##Predictions
colnames(test.data1)[17:21]<-colnames(train2)[18:22] ##added

predictions=predict(stack.lm, newdata=test.data1)


##
length(train.year)
length(test.year)
###



pred_df<-as.data.frame(cbind(1:11613,predictions))
colnames(pred_df)<-c("id","price")

head(pred_df)

write.csv(x=pred_df,file="submission_April_4_Be.csv",row.names = FALSE)



#######

