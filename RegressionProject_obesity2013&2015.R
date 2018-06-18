obesity2<-read.csv("yrbs with new data.csv")
View(obesity2)
colnames(obesity2)<-c("year2","state2","alcohol2",
                     "computer2","soda2","fat2")
attach(obesity2)

#study obesity2 based on alcohol2, computer2, etc.
par(mfrow=c(2,2))
hist(alcohol2)
Boxplot(alcohol2)
hist(computer2)
Boxplot(computer2)
hist(soda2)
Boxplot(soda2)

#scatterplot matrix (only look at fat2 vs predictors)
scatterplotMatrix(~fat2+alcohol2+computer2+soda2,data=obesity2,
                  smooth=FALSE,ellipse="FALSE")

#correlation matrix
corr.test(x=obesity2[,3:9],y=NULL,use="pairwise",method="pearson",
          adjust="holm",alpha=.05)

#additive model for 6 predictors
obesity2.lm<-lm(fat2~alcohol2+computer2+soda2,data=obesity2)
summary(obesity2.lm)

#test for normality of residuals
obesity2.res<-residuals(obesity2.lm)
shapiro.test(obesity2.res)

#test for whether the variance of the error terms is constant
ncvTest(obesity2.lm)

#plot of residuals
residualPlots(obesity2.lm)

#step function
obesity2.null<-lm(fat2~1,data=obesity2)
summary(obesity2.null)
obesity2.full=lm(fat2~alcohol2+computer2+soda2,
                data=obesity2)
summary(obesity2.full)
step(obesity2.null,
     scope=list(lower=obesity2.null,upper=obesity2.full),
     direction="forward")

#reduced additive model
obesity2.lm2<-lm(fat2~poly(alcohol2,2)+soda2+poly(alcohol2,2)*soda2+
                   soda2*factor(year2)+poly(alcohol2,2)*factor(year)
                    +factor(year2)
                   ,data=obesity2)
summary(obesity2.lm2)

#test for normality of residuals for reduced additive model
obesity2.res2<-residuals(obesity2.lm2)
shapiro.test(obesity2.res2)

#test for whether the variance of the error terms is constant
ncvTest(obesity2.lm2)

#plot of residuals for reduced additive model and interactions
residualPlots(obesity2.lm2)
plot(alcohol2*computer2,obesity2.res)
abline(0,0)
plot(computer2*soda2,obesity2.res)
abline(0,0)
plot(alcohol2*soda2,obesity2.res)
abline(0,0)

#fitted model with polynomial with soda2 at order 2
obesity2.polylm<-lm(fat2~poly(alcohol2,2)+soda2+
                      factor(year2),
                   data=obesity2)
summary(obesity2.polylm)

#test for normality of residuals for polynomial model
obesity2.res<-residuals(obesity2.polylm)
shapiro.test(obesity2.res)

#test for whether the variance of the error terms is constant
ncvTest(obesity2.polylm)

#plot of residuals for polynomial model
residualPlots(obesity2.polylm)