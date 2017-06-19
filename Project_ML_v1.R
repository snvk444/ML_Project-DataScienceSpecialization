library(caret);
library(ggplot2);
library(lattice);
setwd("C:/Users/Venkata/Desktop/Personal/Coursera/Data_Science_Specialization/Machine_Learning/assignment");
data <- read.csv("pml-training.csv")
#stats of the data.
colnames(data)
dim(data)
summary(data)

#There are 6 participants and 5 classe variables. The 5 classe variables are 
#1.exactly according to the specification (Class A), 
#2.throwing the elbows to the front (Class B), 
#3.lifting the dumbbell only halfway (Class C), 
#4.lowering the dumbbell only halfway (Class D)
#5.throwing the hips to the front (Class E).
#GOAL: predict the manner in which the participants did the excercise.
#Since participants have performed correctly and incorrectly, first step above all would be to look at
#all the participants and classA-variable to see if there are any noticeable points to note.
###################Should decide##############################
#In order to understand the data more clearly, we can then look at the data from 2 perspectives.
#1. Considering a participant and identifying his stats for all classe variable. 
#2. Considering a classe variable and identifying all participants performance. 
##############################################################
#In the column new_window, there are only 2 values. 'yes' and 'no'. lets split the data and see what they refer to.
yes_window <- data[data$new_window=='yes',]
no_window <- data[data$new_window == 'no',]
#There are only 406 records for new_window=yes but there are 19216 records for new_window=no. 
#Since there are more records for new_window=no, lets use it for our analysis.

data_subset <- no_window
dim(data_subset)

#BASIC PLOTTING
library(ggplot2)
#case1:
#featurePlot(x = data_subset[,c("roll_belt","classe")],y=data_subset$classe,plot="pairs")
#the above graph has shown that there is a range for classA variable.

#case2:
#featurePlot(x = data_subset[,c("roll_belt","roll_arm","roll_dumbbell","roll_forearm","classe")],
#            y=data_subset$classe,plot="pairs")


#qplot(data_subset$roll_belt, data_subset$user_name,colour=data_subset$classe, data = data_subset)


########################################################
#Step1: Determine what columns to use. 
#sapply(X = data_subset[,c("roll_belt","roll_arm","roll_dumbbell","roll_forearm")], FUN=quantile)
colnames(data_subset)
listed_data <- as.list(data_subset)
column_class <- sapply(X= listed_data, FUN=class)
col_factor <- which(column_class=="factor")
numeric_columns <- which(column_class =="numeric")
temp_results <- sapply(X=data_subset[,numeric_columns], FUN=quantile, na.rm = TRUE)
dim(temp_results)
sub_data_subset <- numeric_columns[which(!is.na(temp_results[1,]))]
sub_data_subset
length(sub_data_subset)
useful_data_subset <- data_subset[,sub_data_subset]
colnames(useful_data_subset)
useful_data_subset$classe <- data_subset$classe
useful_data_subset$user_name <- data_subset$user_name
useful_data_subset <- useful_data_subset[,-27]
useful_data_subset <- useful_data_subset[,-26]
useful_data_subset <- useful_data_subset[,-19]

#splitting the data into training and testing data.
inTrain <- createDataPartition(y=useful_data_subset$classe, p=0.75, list=FALSE)
inTrain
training <- useful_data_subset[inTrain,]
testing <- useful_data_subset[-inTrain,]
colnames(training)

#using randomforest package to implement random forest algorithm
library(randomForest)
library(caret)
res = cross_validation <- rfcv(trainx=training[,-25], trainy=training[,25], cv.fold=5, step=0.5, recursive=FALSE)
with(res, plot(n.var,error.cv,log="x",type= "o", lwd=2))
res$n.var
#training the model
train_rf <- randomForest(training$classe~ .,data = training)
train_rf$confusion
print(train_rf)
#MeanDecreaseGini Index
round(importance(train_rf),2)
rf1 <- sort(importance(train_rf)[,'MeanDecreaseGini'],decreasing = TRUE)
col <- which(round(importance(train_rf),2)[,1] >451)
col
col <- c("roll_belt", "pitch_belt","yaw_belt","yaw_arm","roll_dumbbell","yaw_dumbbell","gyros_dumbbell_y","roll_forearm","pitch_forearm")
train_classe <- training[,col]
colnames(train_classe)
head(train_classe)
colnames(training)
train_classe$classe <- training[,'classe']
train_rf2 <- randomForest(train_classe$classe~ .,data = train_classe)

test_data <- read.csv("pml-testing.csv")
test_data$problem_id
required_columns <- test_data[,col]
colnames(required_columns)
colnames(train_classe)
## Applying the model to the other test-dataset given to us.
colnames(required_columns)
listed_data <- as.list(required_columns)
listed_data
column_class <- sapply(X= listed_data, FUN=class)
col_factor <- which(column_class=="factor")
col_factor
numeric_columns <- which(column_class =="numeric")
temp_results <- sapply(X=required_columns[,numeric_columns], FUN=quantile, na.rm = TRUE)
dim(temp_results)
sub_testdata_subset <- numeric_columns[which(!is.na(temp_results[1,]))]
sub_testdata_subset
length(sub_testdata_subset)
useful_testdata_subset <- required_columns[,sub_testdata_subset]

colnames(useful_testdata_subset)

output<-predict(train_rf2, useful_testdata_subset, type="class")

