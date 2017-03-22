### Exploring relationships among confidence or difficulty and other features
#Correlations <=0.30 (weak),  [0.30,0.50] (moderate), >0.70 (strong) 

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//workerConfidenceTrees//loadAnswers.R");
dataf <- loadAnswers("answerList_data.csv");
summary(dataf)

#--------------------------------------------------------
#Among indepedent variables

plot(dataf$Code.LOC,dataf$Code.complexity);

cor.test(dataf$Code.LOC,dataf$Code.complexity,method="kendall") #SIGNIFICANT, MODERATE
#z = 31.827, p-value < 2.2e-16
#tau = 0.5936692

cor.test(dataf$Answer.confidence, dataf$Answer.difficulty,method="kendall") #SIGNIFICANT, MODERATE
#z = -31.206, p-value < 2.2e-16
#tau = -0.4978455 


#--------------------------------------------------------
##Independent vs dependent variables
plot(dataf$Answer.difficulty,dataf$Code.complexity);
plot(dataf$Answer.difficulty,dataf$Code.LOC);

cor.test(dataf$Answer.difficulty,dataf$Code.LOC,method="kendall") #SIGNIFICANT, BUT WEAK
#z = 3.4995, p-value = 0.0004661
#tau = 0.058055

cor.test(dataf$Answer.difficulty,dataf$Code.complexity,method="kendall") #SIGNIFICANT, BUT WEAK
#z = 2.068, p-value = 0.03864
#tau = 0.03280068

#--------------------------------------------------------
#ONLY YES ANSWERS

#Remove NO AND IDK ANSWERS
dataf <- dataf [!(dataf$Answer.option=="IDK") ,];
dataf <- dataf [!(dataf$Answer.option=="NO") ,];

cor.test(dataf$Answer.difficulty,dataf$Code.LOC,method="kendall") #NOT SIGNIFICANT

cor.test(dataf$Answer.difficulty,dataf$Code.complexity,method="kendall") #SIGNIFICANT, BUT WEAK
#z = -2.0524, p-value = 0.04013
#tau = 0.06775545 

#--------------------------------------------------------
#ONLY NO ANSWERS

#Remove YES AND IDK ANSWERS
dataf <- dataf [!(dataf$Answer.option=="IDK") ,];
dataf <- dataf [!(dataf$Answer.option=="YES") ,];

cor.test(dataf$Answer.difficulty,dataf$Code.LOC,method="kendall") #SIGNIFICANT, BUT WEAK
#z = 3.6807, p-value = 0.0002326
#tau = 0.07790208 

cor.test(dataf$Answer.difficulty,dataf$Code.complexity,method="kendall") #SIGNIFICANT, BUT WEAK
#z = 3.5629, p-value = 0.0003668
#tau = 0.07558643 

#--------------------------------------------------------

