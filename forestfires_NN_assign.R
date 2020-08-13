install.packages("neuralnet")
library(neuralnet)
library(nnet) 
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(caret)
library(plyr)
forestfire=read.csv(file.choose())
View(forestfire)
forestfire=forestfire[,3:31]
View(forestfire)
summary(forestfire)
class(forestfire$size_category)
forestfire$size_category=as.numeric(revalue(forestfire$size_category,c("small"="0","large"="1")))
View(forestfire)
#Normalize
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}


ff_norm=as.data.frame(lapply(forestfire,FUN =normalize))
summary(forestfire$size_category)

#Train Test Partition
set.seed(123)
inTraining=createDataPartition(ff_norm$size_category,p=0.75,list=F)
ff_norm_train=ff_norm[inTraining,]
ff_norm_test=ff_norm[-inTraining,]

#Building model
model=neuralnet(size_category~.,data=ff_norm_train)
plot(model)
model_results=compute(model,ff_norm_test[1:28])
predicted_size=model_results$net.result
str(predicted_size)
View(predicted_size)
cor(predicted_size,ff_norm_test$size_category) #0.96
plot(predicted_size,ff_norm_test$size_category)
a=as.data.frame(cbind(ff_norm_test,predicted_size))
View(a)

#Model2 Not needed
model2=neuralnet(size_category~.,data=ff_norm_train,hidden = 5)
plot(model2)
model_results2=compute(model2,ff_norm_test[1:28])
predicted_size2=model_results2$net.result
str(predicted_size)
View(predicted_size)
cor(predicted_size2,ff_norm_test$size_category) #0.87
