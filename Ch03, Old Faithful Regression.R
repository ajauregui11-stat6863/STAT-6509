library(MASS)
data("faithful")
View(faithful)
attach(faithful)

#response is waiting for eruption, predictor is eruption time
faithful_lm=lm(eruptions~waiting) 

#summarize regression
summary(faithful_lm) 

#gives confidence intervals for regression coefficients
confint(faithful_lm,level=.95)

#create ANOVA table
anova(faithful_lm)

library(ggplot2)
#boxplot for waiting for eruptions (predictor)
ggplot(data=faithful,aes(x="",y=eruptions))+
  geom_boxplot(color="purple")+
  scale_x_discrete(name="Predictor")+
  scale_y_continuous(name="Eruption time in minutes")+
  geom_jitter(width=.2,color="red")

#histogram for eruption time in minutes (response)
hist(eruptions)

#
dotchart(eruptions)

#scatter plot of x and y
plot(waiting, eruptions) 

#correlation coefficient
cor.test(waiting,eruptions) 

#residual vs fit plot
ggplot(faithful_lm, aes(x=.fitted,y=.resid))+geom_point(size=1,shape=2,color="orange")+
  geom_hline(yintercept=0,col="red",linetype="dashed")+xlab("Fitted values")+
  ylab("Residuals")+ggtitle("Residual vs Fitted Plot")

#qq plot of residuals
faithful_stdres=rstandard(faithful_lm)
qqnorm(faithful_stdres,ylab="Standardized Residuals",xlab="Normal Scores",
       main="Old Faithful Eruptions")
qqline(faithful_stdres)

#brown-forsythe test
faithful_lm$groups<-ifelse(eruptions<3,c("1"),c("2"))
faithful_lm$groups
library(car)
leveneTest(faithful_lm$residuals,faithful_lm$groups)
ncvTest(faithful_lm) #nonconstant variance test

#test of normality-shapiro wilk
shapiro.test(faithful_lm$residuals)

#lack of fit F-test, didn't run properly seek help
faithful_reduced=lm(eruptions~waiting) #fit reduced model
faithful_full=lm(eruptions~0 as.factor(waiting)) #fit full model
anova(faithful_reduced,faithful_full) #get lack-of-fit test

#box-cox transformation
boxcox(eruptions~waiting,data=faithful,lambda=seq(-5,5,length=.1))