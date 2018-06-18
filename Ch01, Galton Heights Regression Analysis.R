library(HistData)
data("Galton")
attach(Galton)
View(Galton)
summary(Galton)
library(ggplot2)
ggplot(Galton,aes(x=parent,y=child))+
  geom_point(size=2,alpha=.7)+
  xlab("Height of Parent")+
  ylab("Height of son")+
  ggtitle("Galton Height Data")

#fit linear (regression) model
galton.lm=lm(parent~child,Galton)

#see the regression summary
summary(galton.lm)

#find residuals
galton.residuals=resid(galton.lm)

#check if residuals add to 0
k=sum(galton.residuals)
print(k)

#find correlation coefficient
cor.test(parent,child)