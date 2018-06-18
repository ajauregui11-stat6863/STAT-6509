airlinesdata=read.table("airlinesdata2004.txt")
colnames(airlinesdata)<-c("City1","City2","Fare","Distance","Passengers","LeadAirline","MarketShare","AvgFare","LowPr","MarketShare2","Price")
cor.test(airlinesdata$Distance,airlinedata$Fare) #find correlation coefficient
airlines_lm=lm(Fare~Distance,airlinesdata) #fit linear (regression) model for y~x
summary(airlines_lm) #regression summary
confint(airlines_lm)
library("ggplot2")
ggplot(airlinesdata,aes(x=Distance,y=Fare))+geom_point(colour="purple",size=1)+geom_smooth(method='lm',fill="firebrick",color="yellow",size=1)
#predict for distances 500 and 1000
predict(airlines_lm,newdata=list(Distance=c(500,1000)),interval="predict",level=.95)
anova(airlines_lm) #produce ANOVA table