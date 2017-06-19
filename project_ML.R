library(caret);
library(ggplot2);
library(lattice);
setwd("C:/Users/Venkata/Desktop/Personal/Coursera/Data_Science_Specialization/Machine_Learning/assignment");
data <- read.csv("pml-training.csv")
#stats of the data.
colnames(data)
dim(data)
summary(data)

adelmo_data <- data[data$user_name == 'adelmo',]

#Splitting the data into training and testing datasets.
set.seed(100)
inTrain <- createDataPartition(adelmo_data$classe, p=0.75, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

#k-fold -resampling method to handling
#folds <- createFolds(data$classe, k=10, list=TRUE, returnTrain = TRUE)
folds <- cvFolds(NROW(adelmo_data), K=k)
adelmo_data$pred_results <- rep(0,nrow(adelmo_data))
head(folds)

#selecting the best parameters using cv,k-folds.
#library("cvTools")
k = 10;
for(i in 1:k){
  train <- adelmo_data[folds$subsets[folds$which != i], ] #Set the training set
  validation <- adelmo_data[folds$subsets[folds$which == i], ] #Set the validation set
  
  newlm <- glm(classe~.,data=train[,c(-161,-2)]) #Get your new linear model (just fit on the train data)
  newpred <- predict(newlm,newdata=validation) #Get the predicitons for the validation set (from the model just fit on the train data)
  
  data[folds[folds$which == i], ]$classe <- newpred #Put the hold out prediction in the data set for later use
}


#plotting graphs
featurePlot(x = train[,c("pitch_belt","pitch_arm","classe")],y=train$classe,plot="pairs")
featurePlot(x = train[,c("yaw_belt","total_accel_belt","classe")],y=train$classe,plot="pairs")
featurePlot(x = train[,c("skewness_roll_belt","kurtosis_roll_belt","classe")],y=train$classe,plot="pairs")


modfit <- train(classe ~ roll_belt, data = training, method = "glm")





