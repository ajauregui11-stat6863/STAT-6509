setwd("~/Documents/CSUEB Biostatistics/STAT 6509 - Regression Theory & Application")
library(car)
library(psych)
library(visreg)
library(ggplot2)
obesity3<-read.csv("FK Rule (2013).csv")
View(obesity3)
colnames(obesity3)<-c("year3","state3","alcohol3",
                      "computer3","cigarettes3",
                      "pe3","soda3","tv3","fat3")
attach(obesity3)

#study obesity3 based on alcohol3, computer3, etc.
par(mfrow=c(2,2))
hist(alcohol3)
Boxplot(alcohol3)
hist(computer3)
Boxplot(computer3)
hist(soda3)
Boxplot(soda3)

#scatterplot matrix (only look at fat3 vs predictors)
scatterplotMatrix(~fat3+alcohol3+computer3+soda3+
                    tv3+pe3+cigarettes3,data=obesity3,
                  smooth=FALSE,ellipse="FALSE")

#correlation matrix
corr.test(x=obesity3[,3:9],y=NULL,use="pairwise",method="pearson",
          adjust="holm",alpha=.05)

#additive model for 6 predictors
obesity3.lm<-lm(fat3~alcohol3+computer3+soda3+pe3+tv3+cigarettes3
                ,data=obesity3)
summary(obesity3.lm)

#test for normality of residuals
obesity3.res<-residuals(obesity3.lm)
shapiro.test(obesity3.res)

#test for whether the variance of the error terms is constant
ncvTest(obesity3.lm)

#plot of residuals
residualPlots(obesity3.lm)

#step function
obesity3.null<-lm(fat3~1,data=obesity3)
summary(obesity3.null)
obesity3.full=lm(fat3~alcohol3+computer3+soda3+tv3+pe3+
                   cigarettes3,data=obesity3)
summary(obesity3.full)
step(obesity3.null,
     scope=list(lower=obesity3.null,upper=obesity3.full),
     direction="forward")

#reduced additive model
obesity3.lm2<-lm(fat3~pe3+tv3+computer3+soda3,data=obesity3)
summary(obesity3.lm2)

#test for normality of residuals for reduced additive model
obesity3.res2<-residuals(obesity3.lm2)
shapiro.test(obesity3.res2)

#test for whether the variance of the error terms is constant
ncvTest(obesity3.lm2)

#plot of residuals for reduced additive model and interactions
residualPlots(obesity3.lm2)
plot(alcohol3*computer3,obesity3.res)
abline(0,0)
plot(computer3*soda3,obesity3.res)
abline(0,0)
plot(alcohol3*soda3,obesity3.res)
abline(0,0)

#fitted model with polynomial with soda3 at order 2
obesity3.polylm<-lm(fat3~poly(alcohol3,2)+soda3+
                      factor(year2),
                    data=obesity3)
summary(obesity3.polylm)

#test for normality of residuals for polynomial model
obesity3.res<-residuals(obesity3.polylm)
shapiro.test(obesity3.res)

#test for whether the variance of the error terms is constant
ncvTest(obesity3.polylm)

#plot of residuals for polynomial model
residualPlots(obesity3.polylm)