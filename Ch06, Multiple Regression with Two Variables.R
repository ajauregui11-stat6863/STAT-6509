satisfaction<-read.table("patientsdata.txt")
colnames(satisfaction)<-c("Satisfaction","Age","Severity.of.Illness","Anxiety")
attach(satisfaction)
View(satisfaction)

#make boxplots for predictors
boxplot(Age,main="Boxplot of Patient's Age")
boxplot(Severity.of.Illness,main="Boxplot of Patient's Severity of Illness Rating")
boxplot(Anxiety,main="Boxplot of Patient's Anxiety Level")

#obtain correlation and scatterplot matrix and interpret the results
cor(satisfaction[,1:4]) #find the pairs of correlation
#based off of the results from the above function, we see that 
#age, severity of illness, and anxiety are negatively associated with 
#satisfaction. We have respective correlation coefficients of 
#-.78, -.60, and -.64. So relatively good numbers that indicate 
#correlation.

#find scatterplot matrix
library(car) 
scatterplotMatrix(~Satisfaction+Age+Severity.of.Illness+Anxiety,
                  data=satisfaction,smooth=FALSE,ellipse="FALSE",
                  main="Simple ScatterPlot Matrix")

#fit the regression model for the 3 predictors and interpret the 
#regression coefficients
satisfaction.lm<-lm(formula=Satisfaction~Age+Severity.of.Illness+Anxiety)
summary(satisfaction.lm)

#create correlation matrix
library(psych)
corr.test(x=satisfaction,y=NULL,use="pairwise",method="pearson",
          adjust="holm",alpha=.05)

#prepare a boxplot for the residuals and the outliers
satisfaction.res<-resid(satisfaction.lm)
boxplot(satisfaction.res)

#plot residuals against fitted values
layout(matrix(c(1,2,3,4),2,2)) #optional 4 graphs/page
plot(satisfaction.lm)

#constancy of error variance
ncvTest(satisfaction.lm)

#test whether there is a regression relation. answer: 
#look at overall F-test if p-value is less 
#than .05, we say the relatioship is significant.

#obtain an interval estimate of the mean satisfaction when 
#Xh1=25, Xh2=45, and Xh3=2.2. Use a 90% confidence coefficient
predict(satisfaction.lm, 
        newdata=list(Age=25,Severity.of.Illness=45,Anxiety=2.2),
        interval="confidence",level=.9)

#test whether X3 can be dropped from the regression model 
#given that X1 and X2 are retained
satisfaction_fit1<-lm(Satisfaction~
                        Age+Severity.of.Illness+Anxiety)
satisfaction_fit2<-lm(Satisfaction~Age+Severity.of.Illness)
anova(satisfaction_fit1,satisfaction_fit2)  
#we obtain p-value of .06468, which means that we fail to 
#reject the null that X3=0, so we will keep it in the model.
