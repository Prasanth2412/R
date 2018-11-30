# Working directory
setwd("E:\\Prasanth_R\\Sumit_R\\Credit Risk Case Study")
getwd()
gc()
rm(list=ls())

# impoting data from working directory
LGD=read.csv("R_Module_Day_5.2_Data_Case_Study_Loss_Given_Default.csv", stringsAsFactors = F)

# checking structure andd summary of the data
str(LGD)
summary(LGD)

# checking categories in the categorical data
table(LGD$Gender)
table(LGD$Married)

# converting categorical to numerical
LGD$Gender=ifelse(LGD$Gender=="M",1,0)
LGD$Married=ifelse(LGD$Married=="Married",1,0)
str(LGD)


# checking the relationship between the variables

pairs(LGD[,c(2:7)])

plot(LGD$Age,LGD$Losses.in.Thousands,pch=16,xlab = "Age",ylab = "Loss given default",main = "Loss given default vs Age",col="blue")
plot(LGD$Years.of.Experience,LGD$Losses.in.Thousands,pch=16,xlab = "Years.of.Experience",ylab = "Loss given default",main = "Loss given default vs Years.of.Experience",col="blue")
plot(LGD$Number.of.Vehicles,LGD$Losses.in.Thousands,pch=16,xlab = "Number.of.Vehicles",ylab = "Loss given default",main = "Loss given default vs Number.of.Vehicles",col="blue")

boxplot(LGD$Losses.in.Thousands~LGD$Number.of.Vehicles,xlab="Number.of.Vehicles",ylab="Loss given default",main="Loss given default vs Number.of.Vehicles",col="blue",pch=16)
boxplot(LGD$Losses.in.Thousands~LGD$Gender,xlab="Gender",ylab="Loss given default",main="Loss given default vs Age",col="blue",pch=16)
boxplot(LGD$Losses.in.Thousands~LGD$Married,xlab="Married",ylab="Loss given default",main="Loss given default vs Married",col="blue",pch=16)

# creating train & test data sets
set.seed(143)
s=sample(1:nrow(LGD),0.7*nrow(LGD))
LGD_train=LGD[s,]
LGD_test=LGD[-s,]

# Corelation
install.packages("corrplot")
library(corrplot)

cr=cor(LGD_train)
cr
corrplot(cr,type="lower",method="circle")
corrplot(cr,type="lower",method="number")


# Fitting Multiple Linear Regression on LGD 

model=lm(Losses.in.Thousands~.-Ac_No, data=LGD_train)
summary(model)

# R-squared:  0.3022

# residual diagnostics
# checking the linearity assumption
plot(model,1) 
# checking the normality assumption 
plot(model,2)
# checking the homoskedacity assumption
plot(model,3)
# checking the cook's distance
plot(model,4)
# checking the influential points
plot(model,5)


# examining univariate distributions
hist(resid(model),freq=F)
boxplot(resid(model))

# sum of residual is always zero
sum(resid(model))

# no autp corelation between residual
acf(resid(model))

# durbin-watson test
install.packages("lmtest")
library(lmtest)
dwtest(model)

# all independant variable are uncorelated with error term
cor(resid(model),LGD_train[c(2:6)])

# mean of resid model is zero
mean(resid(model))

# dealing with mult colinearity
install.packages("car")
library(car)
sort(vif(model),decreasing = T)

model1=lm(Losses.in.Thousands~.-Ac_No-Years.of.Experience, data=LGD_train)
sort(vif(model1),decreasing = T)
summary(model1)

# chek all the assumptions again for model1 59-92


# Fitting Multiple Linear Regression on LGD 
model2=lm(log(Losses.in.Thousands)~Age+Gender+Number.of.Vehicles+Married,data=LGD_train)
summary(model2)
model2=lm(log(Losses.in.Thousands)~Age+Gender+Married,data=LGD_train)
summary(model2)

# chek all the assumptions again for model2 59-92

# predictions
pred_test=predict(model1,newdata = LGD_test)
errors_test=LGD_test$Losses.in.Thousands-pred_test

# RMSE for performance
sqrt(mean(errors_test**2))

# RMSE for model1= 214.7737

# prediction on model2
pred_test1=predict(model2,newdata = LGD_test)
errors_test1=LGD_test$Losses.in.Thousands-exp(pred_test1)

# RMSE for performance
sqrt(mean(errors_test1**2))

# RMSE for model1= 218.4096

AIC(model)
AIC(model1)

library(Metrics)
rmse(LGD_test$Losses.in.Thousands,pred_test1)
