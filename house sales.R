library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

Path<-"C:\\Users\\SONY\\Documents\\IVY\\R.2\\Linear Regression\\Assignment 2"
setwd(Path)
getwd()

data=read.csv(file.choose())
data1=data#To create a backup of original data

###data exploration###

dim(data1)
data1[1:3,]
str(data1)
summary(data1)
data1$floors<-as.factor(data$floors)
levels(data1$floors)
str(data1)
summary(data1)
data$waterfront
data1$waterfront<-as.factor(data1$waterfront)
levels(data1$waterfront)
data1$bathrooms
data1$bedrooms
data1$bedrooms<-as.factor(data$bedrooms)
levels(data1$bedrooms)
str(data1)
data1$view
data1$view<-as.factor(data1$view)
levels(data1$view)
data1$condition
data1$condition<-as.factor(data1$condition)
levels(data1$condition)
str(data1)

hist(data1$price)##skewed
data1$price=log(data1$price)
hist(data1$price)

corr = cor(data1$price, data1$sqft_living)
corrplot(corr)

boxplot(data1$price)
quantile(data1$price,seq(0,1,0.05))
boxplot(data1$price, outline = FALSE)
data2=data1[data1$price<1156480,]         
boxplot(data2$price)
quantile(data2$price, seq(0,1,0.025))
data3=data2[data2$price<979362.5,]
boxplot(data3$price)
boxplot(data3)
summary(data3)
boxplot(data3$sqft_lot, outline=FALSE)
quantile(data3$sqft_lot, seq(0,1,0.05))
data4=data3[data3$sqft_lot<41608.40,]
boxplot(data4$sqft_lot)
quantile(data4$sqft_lot, seq(0,1,0.025))
data5=data4[data4$sqft_lot<16151.4,]
boxplot(data5$sqft_lot)
summary(data5)
View(data5$yr_renovated)
as.data.frame(colSums(is.na(data5))) ##no missing values
scatterplot(data5$price, data5$sqft_living)
boxplot(data5$sqft_living)
quantile(data5$sqft_living, seq(0,1,0.025))
data6=data5[data5$sqft_living<3500.0,]
boxplot(data6$sqft_living)
boxplot(data6)
data7= data6[, !(colnames(data6) %in% "date")]
View(data7)
model<-lm(price~., data=data7)
summary(model)
names(model)
colnames(data7)

##visualisation using boxplot

boxplot(price~sqft_living, data=data7,  col=(c("gold","darkgreen")), main="Price vs. Sqft_living", xlab="Sqft_living", ylab="Price") ##as sqft increases, price increases as well
boxplot(price~bathrooms, data=data7, col=(c("yellow", "blue")), main="Price vs Bathrooms", xlab = "Bathrooms", ylab= "Price") ##with increase in number of bathrooms, price increases as well
boxplot(price~grade, data=data7, col=(c("gold","darkgreen")),main="Price vs. Grade", xlab="Grade", ylab="Price")##grade increases, price increases as well
boxplot(price~view, data=data7, col=(c("pink","purple")),main="Price vs. View", xlab="View", ylab="Price")##view increases, price increases as well
scatterplot(price~sqft_living, data = data7)##price increases with increase in sqft living
boxplot(price~waterfront, data=data7, col=(c("maroon","grey")), main="Price vs. waterfront", xlab="waterfront", ylab="Price")
boxplot(price~data7$sqft_basement, data=data7, col=(c("blue","red")), main="Price vs. sqft basement", xlab="sqftbasement", ylab="Price")
boxplot(price~zipcode, data=data7, col=(c("orange","blue")), main="Price vs. zipcode", xlab="zipcode", ylab="Price")

avg <-aggregate(price~bathrooms, FUN=mean, data=data7)
plot(avg,main="Avg. Price by # Bathroom")
lin_model_bathroom<-lm(price~bathrooms,data=avg)
summary(lin_model_bathroom)
abline(lin_model_bathroom)

##fitting  a model

model<-lm(price~id+bedrooms+bathrooms+sqft_living+sqft_lot+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15, data=data7)
summary(model)

model<-lm(price~id+bedrooms+bathrooms+sqft_living+sqft_lot+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+sqft_living15+sqft_lot15, data=data7)
summary(model)

model<-lm(price~id+bedrooms+bathrooms+sqft_living+sqft_lot+sqft_lot+I(floors=="1.5")+I(floors=="2")+I(floors=="2.5")+I(floors=="3")+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+sqft_living15+sqft_lot15, data=data7)
summary(model)

model<-lm(price~id+I(bedrooms=="2")+I(bedrooms=="9")+bathrooms+sqft_living+sqft_lot+sqft_lot+I(floors=="1.5")+I(floors=="2")+I(floors=="2.5")+I(floors=="3")+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+sqft_living15+sqft_lot15, data=data7)
summary(model)

model<-lm(price~I(bedrooms=="2")+I(bedrooms=="9")+bathrooms+sqft_living+sqft_lot+sqft_lot+I(floors=="1.5")+I(floors=="2")+I(floors=="2.5")+I(floors=="3")+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+sqft_living15+sqft_lot15, data=data7)
summary(model)

vif(model)

fitted(model)

par(mfrow=c(2,2))
plot(model)

data7$pred <- fitted(model)
write.csv(data,"mape.csv")

#MAPE
#(sum(|actual - predicted|)/actual)/nrows
attach(data7)
MAPE<-print((sum((abs(price-pred))/price))/nrow(data7)) ###0.1964087

##assumption tests

durbinWatsonTest(model) ##p-value is 0.374>0.05, we fail to reject H0: (No Autocorrelation)
dwt(model)

bptest(model) # (p value is less than 0.05)--> Null hypothesis is rejected -> error homogeneous

resids <- model$residuals
par(mfrow=c(1,1))
plot(density(resids))
ad.test(resids)

diff=price-pred
percdiff=abs(diff)/price
me=mean(diff)

n=length(data7$price)
n1=1000
n2=n-n1
rmse=sqrt(sum(diff**2)/n2)
mape=100*(mean(percdiff))
me # -8.476095e-13
rmse # 107197.6
mape # 19.64087
