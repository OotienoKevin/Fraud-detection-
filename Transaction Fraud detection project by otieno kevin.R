##load your data
data<-read.csv(file.choose())
print(tail(data))
mean(data$amount)
#no missing data ,no otliers ,we are dealing with a clean data
#data visualization
#check the age group with most frauds
png("frauds.png")
boxplot(data$customer_age~data$is_fraudulent,data=data)
abline(h=mean(data$customer_age,data=data),col="red")
dev.off()
data$is_fraudulent=as.factor(data$is_fraudulent)
#build a predictive model that determines if a transaction is a fraud or not
##in this case we will use random forest 
library(randomForest)
library(caTools)
#divide data into train and test
split=sample.split(data,SplitRatio = 0.8)
train_data=subset(data,split==TRUE)
test_data=subset(data,split==FALSE)
dim(train_data)
dim(test_data)
#build model using the train data then i will check the predictive capability of my model using the test data
model<-randomForest(train_data$is_fraudulent~.,data=train_data,importance=TRUE)
summary(model)
#WE have an OOB estimate error of 49.72% meaning our model is 50.28% accurate
#now lets check the predictive capability of our model
predicttest<-predict(model,newdata = test_data,type = "class")
test_data$is_fraudulent
result<-data.frame(predicttest,test_data$is_fraudulent);result #this will show the predictive capability of my model by comparing the predicted and actual values
