#
# Random trees to predict levels of confidence (1 to 5) and difficulty based on 
# the following features: size of source code, complexity, worker skill, 
# profession, Years of Experience, Gender, Age, and Question type.

install.packages('randomForest')
library(randomForest)
install.packages("rpart.plot")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(RColorBrewer)
library(rattle)
library(rpart.plot) 

source("C://Users//chris//OneDrive//Documentos//GitHub//randomForestWorkerConfidenceDifficulty//loadAnswers.R");

##
# Import data
dataf <- loadAnswers("answerList_data.csv");
summary(dataf$Answer.confidence)

#Remove NO AND IDK ANSWERS
#dataf <- dataf [!(dataf$Answer.option=="IDK") ,];
#dataf <- dataf [!(dataf$Answer.option=="NO") ,];
summary(dataf$Answer.option);

##
# Train model

# Treat worker profession as factor (categorical data)
dataf$Worker.profession = as.factor(dataf$Worker.profession)
dataf$Worker.gender = as.factor(dataf$Worker.gender)
dataf$Worker.country = as.factor(dataf$Worker.country)
dataf$Answer.confidence = as.factor(dataf$Answer.confidence)
dataf$Answer.difficulty = as.factor(dataf$Answer.difficulty)

summary(dataf$Answer.option)
#Shuffle randomly the rows in the dataset
set.seed(9850)
g<- runif((nrow(dataf))) #generates a random distribution
dataf.rows <- dataf[order(g),]

#FIRST TREE (loc,complexity,skill)
#separates 70% of the data (1806 rows) to train and 30% (774) to test
#class means that we want a categorization
#the . means that all the other columns are features.

#Confidence prediction - YES Answers

totalData = length(dataf$Answer.difficulty);
trainingSize = trunc(totalData * 0.7);
startTestIndex = totalData - trainingSize;
endTestIndex = totalData;

model1 = randomForest(Answer.confidence ~ 
                        Worker.profession + 
                        Worker.score + 
                        Worker.yearsOfExperience+
                        Worker.gender+
                        Worker.age+
                        Worker.country+
                        Code.complexity+ 
                        Code.LOC, 
                        data = dataf.rows[1:trainingSize,], importance=TRUE, ntree=2000, type="prob");
varImpPlot(model1);

#
model2 = randomForest(Answer.difficulty ~ 
                        Code.complexity+ 
                        Code.LOC, 
                      data = dataf.rows[1:trainingSize,], importance=TRUE, ntree=2000, type="prob");




##
# Predict YES answers
test<-dataf[startTestIndex:endTestIndex,];
Prediction <- predict(model2, test,'vote');
submit <- data.frame(AnswerID = test$Answer.ID, PredictedLevel = Prediction, Actual = test$Answer.difficulty);
write.csv(submit, file = "firstforest.csv", row.names = FALSE);
model2$predicted
model2$confusion
model2$votes
#1           2          3         4          5
#796  0.034165572 0.132720105 0.27201051 0.2877792 0.27332457
#1556 0.044668588 0.089337176 0.46253602 0.1873199 0.21613833



#### NO ANSWERS
model2 = rpart(Answer.confidence ~ Code.complexity + Code.LOC, data = dataf.rows[1:458,], method="class")
rpart.plot(model2)

## Difficulty prediction
### YES Answers
model3 = rpart(Answer.difficulty ~ Worker.profession + Worker.score, data = dataf.rows[1:1134,], method="class")
rpart.plot(model3)

### NO Answers
model4 = rpart(Answer.difficulty ~ Code.complexity + Code.LOC, data = dataf.rows[1:1134,], method="class")
rpart.plot(model4)



#Complexity does not seem to be able to partitiion confidence
#Cannot categorize low confidence levels as well, only 4 and 5

summary(dataf.rows$Answer.confidence)

summary(model1)
rpart.plot(model1)
rpart.plot(model1, type=3, extra=101, fallen.leaves = T)

summary(model2)
rpart.plot(model2)
rpart.plot(model2, type=3, extra=101, fallen.leaves = T)

##############
# 
# DIFFICULTY PREDICTIONS
# 
# ALL ANSWER OPTIONS IDK< YES< NO
# > model2$confusion
# 1 2   3  4 5 class.error
# 1 2 5 270  4 0   0.9928826
# 2 4 0 320 20 0   1.0000000
# 3 2 7 461 50 0   0.1134615
# 4 2 3 355 34 0   0.9137056
# 5 0 0 247 19 0   1.0000000
# 
# NO (difficulty)
# > model2$confusion
# 1 2   3  4 5 class.error
# 1  3 3 223  1 1   0.9870130
# 2  9 2 243  3 0   0.9922179
# 3 13 5 312  7 5   0.0877193
# 4  9 0 189 10 6   0.9532710
# 5  2 1  80  7 0   1.0000000
# 
# 
# YES (difficulty)
# > model2$confusion
# 1 2   3  4 5 class.error
# 1  4 2  53  1 0   0.9333333
# 2  4 5  63  4 0   0.9342105
# 3 16 0 116 14 1   0.2108844
# 4  4 3  94 15 1   0.8717949
# 5  3 2  49  3 0   1.0000000

### CONFIDENCE

# YES
# > model2$confusion
# 1 2  3   4  5 class.error
# 1 0 0  1  11  2   1.0000000
# 2 0 0  1  36  3   1.0000000
# 3 0 0  5 103  7   0.9565217
# 4 0 0 11 125 18   0.1883117
# 5 0 0  5  90 40   0.7037037
# 
# NO
# > model2$confusion
# 1 2 3  4   5 class.error
# 1 0 0 1  4  18   1.0000000
# 2 0 0 1  3  23   1.0000000
# 3 0 0 5 26  61   0.9456522
# 4 0 0 8 31 103   0.7816901
# 5 0 0 6 22 146   0.1609195
# 
#ALL ANSWER OPTIONS
# > model2$confusion
# 1 2   3  4 5 class.error
# 1 2 5 270  4 0   0.9928826
# 2 4 0 320 20 0   1.0000000
# 3 2 7 461 50 0   0.1134615
# 4 2 3 355 34 0   0.9137056
# 5 0 0 247 19 0   1.0000000

