###5.14###
#original equations are 4y_1 + 7y_2 = 25, 2y_1 + 3y_2 = 12

#a) Write equations in matrix notation
A<-matrix(c(4,2,7,3),2,2)
B<-matrix(c(25,12),2,1)

#b) using matrix methods, find solutions for y_1 and y_2
solve(A,B)

###5.5###
#Assume first-order regression model is applicable. Using matrix methods, find

#a) Y'Y
Y<-matrix(c(16,5,10,15,13,22),6,1)
crossprod(Y)

#b) X'X
X<-matrix(c(1,1,1,1,1,1,4,1,2,3,3,4),6,2)
crossprod(X)

#c) X'Y
t(X) %*% Y