assessed.valuations<-read.table("assessed_valuations_data.txt")
View(assessed.valuations)
colnames(assessed.valuations)<-c("selling.price","assessed.value","lot.location")
attach(assessed.valuations)

#Q:create boxplots to compare the selling price for corner and non-corner lots. 
#What do you observe?
boxplot(selling.price~lot.location,data=assessed.valuations,
        main="Compare Selling Price",
        xlab="Non-corner Lots vs Corner Lots",ylab="Selling Price")
#A: I observe that corner lots have a lower median selling price compared to 
#non-corner lots. The variability in selling price is also higher in 
#non-corner lots.

#Q: Assume that the that the response function can be approximated suitably
#using an additive model. Fit the regression model.
assessed.valuations.lm<-lm(formula=selling.price~assessed.value+lot.location)
summary(assessed.valuations.lm)

#Q: Plot the fitted regression function.
layout(matrix(c(1,2,3,4),2,2))
plot(assessed.valuations.lm)
plot(selling.price~assessed.value)
plot(selling.price~lot.location)

#Q: Now rerun the regression with an added interaction term. 
#Comment on the observations.
assessed.valuations_intrctn.lm<-lm(formula=selling.price~assessed.value+lot.location+assessed.value*lot.location)
summary(assessed.valuations_intrctn.lm)

#Q: Predict the selling price of a corner house with an assessed value of 
#$100,000 using a 95% confidence interval.
predict(assessed.valuations.lm,
        newdata=list(assessed.value=100),
        interval="predict",level = .95)