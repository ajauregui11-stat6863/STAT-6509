moadata<-read.csv("MOAData.csv")
View(moadata)
attach(moadata)

#a. Compare the HD Index for the various races using an appropriate plot and
#comment on it.
boxplot(HD..Index~Race.Ethnicity,main="HD Index by Various Races",
        xlab="Various Races",ylab="HD Index",
        names=c("Black","Asian","Latino","White"))

#b. Develop an additive regression model with your 2 predictors (race, and income)
#for predicting HD Index. Obtain the regression coefficients and interpret them.
moa.lm<-lm(HD..Index~Income.Index+factor(Race.Ethnicity),data=moadata)
summary(moa.lm)

#c. Next run a full model with all possible interaction terms (again with just 
#two predictors), provide the regression coefficients along with any relevant
#interpretations.
moa_intrctn.lm<-lm(HD..Index~Income.Index+factor(Race.Ethnicity)+
                     factor(Race.Ethnicity)*Income.Index,data=moadata)
summary(moa_intrctn.lm)

#d. Create visual displays for both the additive and interaction models in parts 
#(b) and (c) with the data points and the estimated regression equations.
library(visreg)
par(mfrow=c(2,1))
visreg(moa.lm, xvar = "Income.Index", by = "Race.Ethnicity", 
       whitespace = 0.5, overlay = TRUE, 
       band = FALSE, points.par = list(cex = 1.1))
visreg(moa_intrctn.lm, xvar = "Income.Index", by = "Race.Ethnicity", 
       whitespace = 0.5, overlay = TRUE, 
       band = FALSE, points.par = list(cex = 1.1))

#e. Now include life expectancy as a predictor and run an additive model. 
#Check for model assumptions using residual plots and for normality 
#and equal variances using formal tests. Provide your thoughts along with 
#the outputs.
moa.lm2<-lm(HD..Index~Income.Index+factor(Race.Ethnicity)+
              Life.Expectancy.at.Birth..years.,data=moadata)
summary(moa.lm2)
plot(moa.lm2)
shapiro.test(moa.lm2$residuals)
ncvTest(moa.lm2)

#f. Check for any outliers present in the data. Use any appropriate plot to 
#identify TWO points with the largest influence.
qqPlot(moa.lm2, id.n = 2)
#setting the id.n=2 returns the names of the 2 observations with
#the largest studentized residuals.
outlierTest(moa.lm2)

#g. Finally use the AIC to justify the choice of model with our 3 factors. 
#Provide edited results.
moa.null=lm(HD..Index~1, data=moadata)
summary(moa.null)
moa.full = lm(HD..Index~factor(Race.Ethnicity)+Income.Index+
                Life.Expectancy.at.Birth..years., data= moadata)
summary(moa.full)
step(moa.null, scope=list(lower=moa.null, upper=moa.full),
     direction="forward")
