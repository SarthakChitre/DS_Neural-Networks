install.packages("neuralnet")
library(neuralnet)
library(nnet) 
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(caret)
library(plyr)
concrete <- read.csv(file.choose())
View(concrete)
str(concrete)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
summary(concrete_norm$strength)
View(concrete_norm)

#Train test Partition
set.seed(123)
inTraining=createDataPartition(concrete_norm$strength,p=0.75,list=F)
concrete_norm_train=concrete_norm[inTraining,]
concrete_norm_test=concrete_norm[-inTraining,]


# Building model
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_norm_train)
#concrete_model <- neuralnet(formula = formula_nn,data = concrete_train)
str(concrete_model)
plot(concrete_model)

#Evaluation
set.seed(12323)
model_results <- compute(concrete_model,concrete_norm_test[1:8])
str(model_results)
predicted_strength <- model_results$net.result
cor(predicted_strength,concrete_norm_test$strength) #0.813
plot(predicted_strength,concrete_norm_test$strength)

#Model2
concrete_model2<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = c(5,2))
plot(concrete_model2)
model_results2<-compute(concrete_model2,concrete_norm_test[1:8])
predicted_strength2<-model_results2$net.result
cor(predicted_strength2,concrete_norm_test$strength) #0.94
plot(predicted_strength2,concrete_norm_test$strength)

