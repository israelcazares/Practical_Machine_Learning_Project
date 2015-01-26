#loading required libraries
library(caret)
library(ggplot2)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)

#set seed to 32345 in order to get the same random samples.
set.seed(32345)

#Training File URL
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

#Test File URL
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainFilePath <- "./assets/pml-training.csv"
testFilePath <- "./assets/pml-testing.csv"

if(!file.exists("./assets")) {
	dir.create("./assets")
}
if(!file.exists("./assets/pml-training.csv")) {
	download.file(trainUrl, destfile = trainFilePath, method="curl")
}
if(!file.exists("./assets/pml-testing.csv")) {
	download.file(testUrl, destfile = testFilePath, method="curl")
}

trainData <- read.csv(trainFilePath, na.strings=c("NA","#DIV/0!", ""))
testData <- read.csv(testFilePath, na.strings=c("NA","#DIV/0!", ""))


# Delete columns with all missing values
trainData<- trainData[,colSums(is.na(trainData)) == 0]
testData <- testData[,colSums(is.na(testData)) == 0]


trainData <- trainData[,-c(1:7)]
testData <- testData[,-c(1:7)]

#Partitioning the training data set to allow cross-validation

inTrain <- createDataPartition(y=trainData$classe, p=0.6, list=FALSE)

training <- trainData[inTrain,]
testing <- trainData[-inTrain,]
dim(training); dim(testing)

head(training)
head(testing)



#prediction model

modFit <- train(classe ~ ., method="rpart", data=training)
print(modFit$finalModel)



fancyRpartPlot(modFit$finalModel)

#prediction
prediction <- predict(modFit, newdata=testing)

# Test results on our subTesting data set:
confusion1 <- confusionMatrix(prediction, testing$classe)
print(confusion1)


modFit2 <- randomForest(classe ~. , data=training, method="class")

# Predicting:
prediction2 <- predict(modFit2, testing, type = "class")

# Test results on subTesting data set:
confusion2 <- confusionMatrix(prediction2, testing$classe)
print(confusion1)

# predict outcome levels on the original Testing data set using Random Forest algorithm
predictfinal <- predict(modFit2, testData, type="class")
predictfinal