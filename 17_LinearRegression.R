# Linear Regression in R
#1.

mouse.data <- data.frame(
  weight=c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3),
  size=c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3))

mouse.data # print the data to the screen in a nice format

## plot a x/y scatter plot with the data
plot(mouse.data$weight, mouse.data$size)

## create a "linear model" - that is, do the regression
mouse.regression <- lm(size ~ weight, data=mouse.data)

## generate a summary of the regression
summary(mouse.regression)

## add the regression line to our x/y scatter plot
abline(mouse.regression, col="blue")


#2.

vehicle <- read.csv("vehicle.csv", header = TRUE)

head(vehicle)
pairs(vehicle)

# Data Review and Data Preparation
vehicle$lh[vehicle$lh==0] <- mean(vehicle$lh)
vehicle$lc[vehicle$lc==0] <- mean(vehicle$lc)

# Data Partition
set.seed(1234)

#sample()
ind <- sample(2, nrow(vehicle), 
              replace = TRUE, 
              prob = c(0.7, 0.3))
training <- vehicle[ind==1,]
testing <- vehicle[ind==2,]

# simple Linear Regression
model <- lm(lc~lh, data=training)
model
summary(model)
plot(lc~lh, training)
abline(model, col = "blue")

# # Model Diagnostics
# 
# #par()
# 
# par(mfrow=c(2,2))
# plot(model)
# vehicle[1620,]

# Prediction
pred <- predict(model, testing)
pred
predict(model, data.frame(lh=10))



#3.

input <- mtcars[,c("mpg","disp","hp","wt")]
# Create the relationship model.
model <- lm(mpg ~ hp+wt, data = input)
# Show the model.
print(model)
# Get the Intercept and coefficients as vector
# elements.
a <- coef(model)[1]
Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]

print(a)
print(Xdisp)
print(Xhp)
print(Xwt)
newdata <- data.frame(hp=102,wt=2.91,disp=221)
Y <- predict(model,newdata)
print(Y)
summary(model)


#4.
housedata <- read.csv("D:/Jan May 2020/R labs/boston-housing-dataset/HousingData.csv", header = TRUE)

# This data frame contains the following columns:
#   
#   crim
# per capita crime rate by town.
# 
# zn
# proportion of residential land zoned for lots over 25,000 sq.ft.
# 
# indus
# proportion of non-retail business acres per town.
# 
# chas
# Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# 
# nox
# nitrogen oxides concentration (parts per 10 million).
# 
# rm
# average number of rooms per dwelling.
# 
# age
# proportion of owner-occupied units built prior to 1940.
# 
# dis
# weighted mean of distances to five Boston employment centres.
# 
# rad
# index of accessibility to radial highways.
# 
# tax
# full-value property-tax rate per \$10,000.
# 
# ptratio
# pupil-teacher ratio by town.
# 
# black
# 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# 
# lstat
# lower status of the population (percent).
# 
# medv
# median value of owner-occupied homes in \$1000s.

str(housedata)

sum(is.na(housedata))

summary(housedata)

housedata$CRIM
#install.packages("dplyr")
library(dplyr)
housedata<- housedata %>% 
  mutate(CRIM = replace(CRIM, is.na(CRIM), mean(CRIM,na.rm = TRUE)))

housedata$CRIM

summary(housedata)

housedata<- housedata %>% 
  mutate(ZN = replace(ZN, is.na(ZN), mean(ZN,na.rm = TRUE)))

housedata<- housedata %>% 
  mutate(INDUS = replace(INDUS, is.na(INDUS), mean(INDUS,na.rm = TRUE)))

housedata<- housedata %>% 
  mutate(CHAS = replace(CHAS, is.na(CHAS), mean(CHAS,na.rm = TRUE)))

housedata<- housedata %>% 
  mutate(AGE = replace(AGE, is.na(AGE), mean(AGE,na.rm = TRUE)))

housedata<- housedata %>% 
  mutate(LSTAT = replace(LSTAT, is.na(LSTAT), mean(LSTAT,na.rm = TRUE)))

summary(housedata)

#install.packages(caTools)
library(caTools)
split <- sample.split(housedata,SplitRatio=0.8)

split

train_data <- subset(housedata,split=TRUE)
test_data <- subset(housedata, split=FALSE)

#to plot scatterplot
plot(train_data[1:5])

#to check correlation
cr <- cor(housedata)
cr

model <- lm(MEDV~.,housedata)

summary(model)

predicted <- predict(model, test_data)
predicted

#compare predicted values and actual values

plot(test_data$MEDV,type = "l",lty=1.8, col="green")

lines(predicted,type="l", col="red")



#5.

df <- read.csv2("D:\\R Programs\\student\\student-mat.csv")


#EDA

head(df)

summary(df)

any(is.na(df))

str(df)

#check the correlation between variables

numeric_columns = sapply(df,is.numeric)

correlated_data = cor(df[,numeric_columns])

correlated_data
#install.packages("corrplot")

library(corrplot)


#plot.new(); dev.off()
corrplot(correlated_data, method = "color")

#install.packages("corrgram")

library(corrgram)

corrgram(df)

corrgram(df, order = TRUE, lower.panel = panel.shade, 
         upper.panel = panel.pie, text.panel = panel.txt)


library(ggplot2)
ggplot(df,aes(x = G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = "blue", color = "gold")


#split the dataset into training and testing set

#install.packages("caTools")

library(caTools)
  
set.seed(101)   

sample <- sample.split(df$G3, SplitRatio = 0.8)                                   

trainset <- subset(df,sample == TRUE)

testset <- subset(df, sample == FALSE)

# train the model

model <- lm(G3 ~ ., data = trainset)

summary(model)

res <- residuals(model)
class(res)

res <- as.data.frame(res)

ggplot(res, aes(res)) + geom_histogram(fill = "blue", bins = 40)

#plot(model)

#predicting the data

predicted_G3 <- predict(model,testset)

result <- cbind(predicted_G3, testset$G3)

colnames(result) <- c("predicted", "actual")

result <- as.data.frame(result)

set_zero <- function(x){
  
  if (x< 0){
    return(0)
  }else{
    return(x)
  }
}

result$predicted <- sapply(result$predicted,set_zero)

mse <- mean((result$actual - result$predicted)^2)

print("MSE")
print(mse)

rmse <- mse^0.5

print("RMSE")
print(rmse)

SSE <- sum((result$predicted - result$actual)^2)

SST <- sum((mean(df$G3) - result$actual)^2)

R2 <- 1 - SSE/SST

print("R squared")

print(R2)

