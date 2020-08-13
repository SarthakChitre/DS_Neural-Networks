install.packages("neuralnet")
library(neuralnet)
library(nnet) 
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(caret)
library(plyr)
startups <- read.csv(file.choose())
View(startups)
class(startups)
startups$State=as.numeric(factor(startups$State,levels=c("New York","California","Florida"),labels = c("0","1","2")))
View(startups$State)
startups=as.data.frame(startups)
attach(startups)
plot(startups$R.D.Spend,Profit)
pairs(startups)
summary(startups)

#Normalization
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
startups_norm=as.data.frame(lapply(startups,FUN = normalize))
View(startups_norm)
summary(startups_norm$Profit)                            


#Test and Train data
set.seed(123)
inTraining=createDataPartition(startups_norm$Profit,p=0.75,list=F)
startups_norm_train=startups_norm[inTraining,]
startups_norm_test=startups_norm[-inTraining,]

#Creating a neural network
nnmodel=neuralnet(Profit~R.D.Spend+Administration
                  +Marketing.Spend+State,data=startups_norm_train)

plot(nnmodel)

#Evaluation
model_results=compute(nnmodel,startups_norm_test[1:4])
predicted_profit=model_results$net.result
cor(predicted_profit,startups_norm_test$Profit) #0.972
#m=as.data.frame(predicted_profit,startups_norm_test$Profit)
#View(m)

#second model
set.seed(123)
nnmodel2=neuralnet(Profit~R.D.Spend+Administration
                  +Marketing.Spend+State,data=startups_norm_train,hidden=2)
plot(nnmodel2)

#Evaluation
model_results2=compute(nnmodel2,startups_norm_test[1:4])
predicted_profit2=model_results2$net.result
cor(predicted_profit2,startups_norm_test$Profit) #0.969

