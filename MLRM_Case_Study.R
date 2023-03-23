list.of.packages <- c("boot", "car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools", "pastecs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(boot)#Linear Regression Model Diagnostics
library(car)# Multicollinearity
library(QuantPsyc)
library(lmtest)# Linear Regression Model Diagnostics
library(sandwich)# Linear Regression Model Diagnostics
library(vars)# Linear Regression Model Diagnostics
library(nortest)# Linear Regression Model Diagnostics
library(MASS)# Linear Regression Model Diagnostics
library(caTools)# Splitting data into train & test data
library(dplyr)#Data Handling

setwd("C:/Users/Arhit Roy Chowdhury/Desktop/RStudio Class/Case Studies/MLRM")
getwd()

main_data = read.csv("State_data.csv")

data0 = main_data

dim(data0)
str(data0)
summary(data0)

colnames(data0)

remove_col = c("state.abb","state.area","x","y","state.division","state.name")

data1 = data0[,!names(data0) %in% remove_col]

summary(data1)

data2 = data1
data2$state.region = as.factor(data1$state.region)

summary(data2) 

hist(data2$Life.Exp)
quantile(data2$Life.Exp,  c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1.0))

as.data.frame(colSums(is.na(data2)))

write.csv(data2, "MLRM _data", row.names = FALSE)

set.seed(125)

spl = sample.split(data2$Life.Exp, 0.7)
train_data = subset(data2, spl==TRUE)
test_data = subset(data2, spl==FALSE)

linear_model1 = lm(Life.Exp ~ ., data = train_data)
summary(linear_model1)

linear_model2 = lm(Life.Exp ~ Population+
                              Illiteracy+
                              Murder+
                              HS.Grad+
                              Frost+
                              Area+
                              state.region, data = train_data)
summary(linear_model2)


linear_model3 = lm(Life.Exp ~ Population+
                     Murder+
                     HS.Grad+
                     Frost+
                     Area+
                     state.region, data = train_data)
summary(linear_model3)

linear_model4 = lm(Life.Exp ~ Murder+
                     HS.Grad+
                     Frost+
                     Area+
                     state.region, data = train_data)
summary(linear_model4)

linear_model5 = lm(Life.Exp ~ Murder+
                     HS.Grad+
                     Frost+
                     state.region, data = train_data)
summary(linear_model5)

linear_model6 = lm(Life.Exp ~ Murder+
                     HS.Grad+
                     Frost+
                     I(state.region == "Northeast")+
                     I(state.region == "West"), data = train_data)
summary(linear_model6)

linear_model7 = lm(Life.Exp ~ Murder+
                     HS.Grad+
                     Frost, data = train_data)
summary(linear_model7)

final_model = linear_model7

vif(final_model) #values less than 1.5 thus no multicollinearity

durbinWatsonTest(final_model)
dwt(final_model) #D-W Stat value is 2.15 hence negligible negative autocorrelation is there

bptest(final_model) #p-value for Breusch-Pagan test is greater than 0.05; thus, homoscedasticity is present

resid = final_model$residuals
pearson.test(resid) #pearson chi-square p-value is less than 0.05 thus, data is normal

fit = fitted(final_model)
train_data$pred = fit

attach(train_data)
(sum((abs(Life.Exp-pred))/Life.Exp))/nrow(train_data) #MAPE value is 0.009, hence, the predicted value deviates
                                                      #less than 1%

linear_model_test = lm(Life.Exp ~ Murder +
                     HS.Grad, data = test_data)
summary(linear_model_test)

final_test_model = linear_model_test

vif(final_test_model) #values less than 1.5 thus no multicollinearity

durbinWatsonTest(final_test_model)
dwt(final_test_model) #D-W Stat value is 2.08 hence negative autocorrelation is there, however,
#However, it is ignored since the value ranges between 1.5 to 2.5

bptest(final_test_model) #p-value for Breusch-Pagan test is greater than 0.05
#thus, heteroscedasticity is present

resid_test = final_test_model$residuals
qqnorm(resid_test) #datapoints are in 45-degree ref line
#thus, data is normal

fit_test = fitted(final_test_model)
test_data$pred = fit_test

attach(test_data)
(sum((abs(Life.Exp-pred))/Life.Exp))/nrow(test_data) #MAPE value is 0.005, hence, the predicted value deviates 0.5%