#
# Decision trees to predict levels of confidence (1 to 5) based on 
# the following features: size of source code, complexity, worker skill, 
# profession, Years of Experience, Gender, Age, and Question type.


install.packages("rpart.plot")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart)
library(RColorBrewer)
library(rattle)
library(rpart.plot) 

source("C://Users//chris//OneDrive//Documentos//GitHub//workerConfidenceTrees//loadAnswers.R");

##
# Import data
dataf <- loadAnswers("answerList_data.csv");
summary(dataf$Answer.confidence)

##
# Train model

# Treat worker profession as factor (categorical data)
dataf$Worker.profession = as.factor(dataf$Worker.profession)

summary(dataf$Answer.option)
#Shuffle randomly the rows in the dataset
set.seed(9850)
g<- runif((nrow(dataf))) #generates a random distribution
dataf.rows <- dataf[order(g),]

#FIRST TREE (loc,complexity,skill)
#separates 70% of the data (1806 rows) to train and 30% (774) to test
#class means that we want a categorization
#the . means that all the other columns are features.

#Confidence prediction
model1 = rpart(Answer.confidence ~ Worker.profession + Worker.score, data = dataf.rows[1:1134,], method="class")
rpart.plot(model1)
model2 = rpart(Answer.confidence ~ Code.complexity + Code.LOC, data = dataf.rows[1:1134,], method="class")
rpart.plot(model2)

## Difficulty prediction
model3 = rpart(Answer.difficulty ~ Worker.profession + Worker.score, data = dataf.rows[1:1134,], method="class")
fancyRpartPlot(model3)
model4 = rpart(Answer.difficulty ~ Code.complexity + Code.LOC, data = dataf.rows[1:1134,], method="class")
fancyRpartPlot(model4) #rpart.plot(model4)


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
# Predict
p1 <- predict(model1, dataf.rows[1135:1640,], type="class")
table(dataf.rows[1135:1640,4],predicted=p1)




