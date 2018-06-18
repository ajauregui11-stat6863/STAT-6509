Airfreight_breakage <-read.table("Airfreight breakage.txt",header=FALSE)
View(Airfreight_breakage)
colnames(Airfreight_breakage)<-c("Y","X")
airresults.lm=lm(Y~X,Airfreight_breakage) #perform regression
summary(airresults.lm) #obtain estimated regression equation
plot(Airfreight_breakage$X,Airfreight_breakage$Y) #plot estimated regression equation and the data
abline(lm(Airfreight_breakage$Y~Airfreight_breakage$X)) #put line through the data
residuals.air=resid(airresults.lm) #obtain residuals
