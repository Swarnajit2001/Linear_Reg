#Q1
#-----
library(readxl)
data1 = read_excel('Rocket_Propellant.xlsx')

model1 = lm(Shear_Strength~Age_of_Propellant, data1)
(s1 = summary(aov(model1)))

library(ggplot2)

ggplot(data1, aes(Age_of_Propellant, Shear_Strength))+
  geom_point()+
  stat_smooth(method = 'lm',
              formula = y~x,
              se= F)
#==============================================
#Q2
#----

data2 = read_excel('delivery_time.xlsx')

model2 = lm(delivery_time~., data2)
summary(model2)
#==========================================
#Q3
#----

#Unbiased estimate of sigma^2
UE.sigma2 = s1[[1]][2,3]

#value of the t-statistic
(s2 = summary(model1))
s2$coefficients[2,3]
#===============================================
#Q4
#----

#creating a sample of size 5k from chisq(3) distn
n = 5e3

M = matrix(0, n, 3)
for(i in 1 : n){
  M[i,] = rnorm(3)
}

W = rowSums(M^2)

library(ggplot2)

#histogram
ggplot()+
  geom_histogram(aes(W,y = after_stat(density)), col = 'black')

#sample mean and variance
mean(W)
var(W)

#===================================================

#Q5
#====

X = matrix(rnorm(40), 8, 5)
Px = X %*% solve(t(X)%*%X) %*% t(X)

sum(Px%*%Px - Px)

library(mvtnorm)

Y = rmvnorm(5e3, rep(0,8), diag(8))

u = Y %*% Px %*% t(Y)

#histogram
ggplot()+
  geom_histogram(aes(u,y = after_stat(density)), col = 'black')

#sample mean and variance
mean(u)
#var(u)
#====================================================

#Q6
#====
Px1 = Px
Px2 = diag(8) - Px1
library(Matrix)
df1 = rankMatrix(Px2)
df2 = rankMatrix(Px1)

f = (Y %*% Px2 %*% t(Y)/df1)/(Y %*% Px1 %*% t(Y)/df2)

ggplot()+
  geom_histogram(aes(f,y = after_stat(density)), col = 'black', bins = 1e3)
