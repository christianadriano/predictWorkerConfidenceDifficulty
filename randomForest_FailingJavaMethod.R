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
dataf <- dataf [!(dataf$Answer.option=="IDK") ,];
dataf <- dataf [!(dataf$Answer.option=="NO") ,];

#SELECT ALL BUGS BUT HIT04_7
dataf <- dataf [!(dataf$FailingMethod="HIT04_7") ,];

summary(dataf$Answer.option);

##
# Train model

# Treat worker profession as factor (categorical data)
dataf$Worker.profession = as.factor(dataf$Worker.profession)
dataf$Worker.gender = as.factor(dataf$Worker.gender)
dataf$Worker.country = as.factor(dataf$Worker.country)
dataf$Answer.confidence = as.factor(dataf$Answer.confidence)

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

model1 = randomForest(Answer.confidence ~ 
                        Worker.profession + 
                        Worker.score + 
                        Worker.yearsOfExperience+
                        Worker.gender+
                        Worker.age+
                        Worker.country+
                        Code.complexity+ 
                        Code.LOC, 
                      data = dataf.rows[1:458,], importance=TRUE, ntree=2000, type="prob");
varImpPlot(model1);

model1 = randomForest(Answer.confidence ~ 
                        Code.complexity+ 
                        Code.LOC, 
                      data = dataf.rows[1:458,], importance=TRUE, ntree=2000, type="prob");
varImpPlot(model1);

#### NO ANSWERS
model2 = rpart(Answer.confidence ~ Code.complexity + Code.LOC, data = dataf.rows[1:1134,], method="class")
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


##
# Predict YES answers
test<-dataf[459:654,];
Prediction <- predict(model1, test);
submit <- data.frame(AnswerID = test$Answer.ID, PredictedConfidence = Prediction, Actual = test$Answer.confidence);
write.csv(submit, file = "firstforest.csv", row.names = FALSE);
model1$predicted
model1$confusion
model1$votes
#1           2          3         4          5
#796  0.034165572 0.132720105 0.27201051 0.2877792 0.27332457
#1556 0.044668588 0.089337176 0.46253602 0.1873199 0.21613833


