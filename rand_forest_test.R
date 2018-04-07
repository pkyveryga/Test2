setwd("C:/Users/Wottawa/Documents/502_project")
train_data_pre<-read.csv("train.csv")
test_data<-read.csv("test.csv")

train_data<-as.data.frame(train_data_pre[,2:21])

head(train_data)

require(randomForest)
start.time<-Sys.time()
rf<-randomForest(price~.,data=train_data,
	type="regression",ntree=100,mtry=4)
Sys.time()-start.time
1

predictions<-predict(object=rf,newdata=test_data)

pred_df<-as.data.frame(cbind(1:11613,predictions))
colnames(pred_df)<-c("id","price")

head(pred_df)

write.csv(x=pred_df,file="submission_test.csv",row.names = FALSE)