install.packages("gsubfn")
library(gsubfn)
library(data.table)
library(zoo)
library(ggplot2)
library(lubridate)
library(GGally)
library(forecast)

raw_data=read.csv("IE360_Spring22_HW2_data.csv")

summary(raw_data)
str(raw_data)

raw_data$Unleaded.Gasoline.Sale..UGS.=gsub(" ","",raw_data$Unleaded.Gasoline.Sale..UGS.)
raw_data$Unleaded.Gasoline.Sale..UGS.=as.numeric(raw_data$Unleaded.Gasoline.Sale..UGS.)
raw_data$X..LPG.Vehicles..NLPG.=gsub(" ","",raw_data$X..LPG.Vehicles..NLPG.)
raw_data$X..LPG.Vehicles..NLPG. =as.numeric(raw_data$X..LPG.Vehicles..NLPG.)
raw_data$X..Unleaded.Gasoline.Vehicles..NUGV.= gsub(" ","",raw_data$X..Unleaded.Gasoline.Vehicles..NUGV.)
raw_data$X..Unleaded.Gasoline.Vehicles..NUGV.=as.numeric(raw_data$X..Unleaded.Gasoline.Vehicles..NUGV.)
raw_data$GNP.Agriculture=gsub(" ","",raw_data$GNP.Agriculture)
raw_data$GNP.Agriculture=as.numeric(raw_data$GNP.Agriculture)
raw_data$GNP.Commerce=gsub(" ","",raw_data$GNP.Commerce)
raw_data$GNP.Commerce=as.numeric(raw_data$GNP.Commerce)
raw_data$GNP.Total=gsub(" ","",raw_data$GNP.Total)
raw_data$GNP.Total=as.numeric(raw_data$GNP.Total)

raw_data$Quarter= as.Date(as.yearqtr(raw_data$Quarter, format = "%Y_Q%q"))

#The data is ready to be manipulated and analyzed, unleaded gasoline sales will be plotted.
#ggplot(data=raw_data, aes(x=Quarter, y=`Unleaded.Gasoline.Sale..UGS.`))+xlab("")+ ylab("UGS") +geom_line()

#Part 1.1
plot(raw_data$Quarter,raw_data$Unleaded.Gasoline.Sale..UGS.,
     type='l',col='blue',
     xlab = "Quarterly Data",
     ylab = "Unleaded Gasoline Sales",
     main = "Time Series")

#After observing the plot, it is apparent that there is a decreasing trend. For stationary(weakly), there must be 2 conditions
#that should be satisfied. One of them is that the mean of the time series should not depend on time. The other one is, 
#for any lag, autocovariance should not depend on time. In this example, the mean depends on time, hence, it is concluded that
#this time series is not stationary. Also it seems that there is a seasonal pattern, so, it is expected that autocorrelation of 
#this time series will violate the stationary rules. For the variance, we could see that it does not change significantly,
#there is no need for transformation

#To have a result analytically, Ljung Box Q test could be performed. Lets try with lag value 10

Box.test(raw_data$Unleaded.Gasoline.Sale..UGS.,lag = 10,type = "Ljung-Box")

#We obtained a p-value as 0.000213. Null hypothesis statement used in this test was the time series is stationary, we have
#quite small p-value which indicates that null hypothesis can be rejected without a doubt.

#Part 1.2
acf(raw_data$Unleaded.Gasoline.Sale..UGS.,10,na.action = na.pass)

# After autocorrelation functions plot, at certain lags, there are significant correlations. This implies the seasonality
#behavior and therefore the time series is not stationary.

#Forecasting with regression, Part2

#1
#We will start with the adding trend variable, from the plot, we observed a decreasing pattern.
raw_data=data.table(raw_data)
data_copy=raw_data

#We will preserve the raw data and make column additions to the copy of the raw data
data_copy[,trend:=1:nrow(data_copy)]
only_trend=lm(data_copy$Unleaded.Gasoline.Sale..UGS.~trend,data = data_copy)
#We will look for further models that are better, if there are any.
summary(only_trend)

#We have an adjusted R squared value of 0.46, we will improve the model by adding reasonable regressors.
#Now we will add seasonality
data_copy[,Quarter_class:=rep(seq(1:4),nrow(data_copy)/4)]
data_copy$Quarter_class=as.character(data_copy$Quarter_class)

trend_seasonality=lm(data_copy$Unleaded.Gasoline.Sale..UGS.~trend+Quarter_class,data = data_copy)
summary(trend_seasonality)
checkresiduals(trend_seasonality)

#There is a significant improvement with adding seasonality variables, adjusted R square value is 0.897 and when we check the
#residuals, they seem like white noise series and to have an analytical interpretation, we can look at Breusch-Godfrey test
#p value is high enough to say that we fail rejecting null hypothesis which states that residuals are not autocorrelated

#For further improvement, we will add other variables in the given data as regressors.However, multicollinearity
#will exist, we will check it.

with_all=lm(Unleaded.Gasoline.Sale..UGS.~.,data=data_copy)

summary(with_all)

#We have a nice adjusted R squared value, however, when we look at the p values, it can be clearly seen that most of the
#variables we used as regressors seem insignificant. P values are not that reliable, due to multicollinearity.
#With investigating pair-wise correlations, we will try to find related regressors with the response variable,UGS.

ggpairs(data_copy,binwidth=0.1)

#When the output is investigated, it can be seen that GNP.Agriculture seems correlated enough. We should also check that
#if it is related with the already available regressor, since if it is correlated enough with the already available 
#regressors, then adding the new one might lead to multicollinearity. However, this is not the case for GNP.Agriculture

added1=lm(Unleaded.Gasoline.Sale..UGS.~trend+Quarter_class+GNP.Agriculture,data = data_copy)

summary(added1)

#We have a slightly better model in terms of adjusted R square
#Now we will add RNUV, since it seems correlated with UGS, and evaluating descriptively, it makes sense to add it
#There are variables which seem more correlated with the response variable, however, they are too correlated with
#the trend, an existing variable. We would like to have independent variables.

added2=lm(Unleaded.Gasoline.Sale..UGS.~trend+Quarter_class+RNUV,data = data_copy)

summary(added2)

#Now we will add autoregressive variable for lag 4. The reason why it is chosen is that we see significant autocorrelation
#with lag 4. Lets put acf again.
acf(data_copy$Unleaded.Gasoline.Sale..UGS., 10,na.action = na.pass)

data_copy[,lag2:=shift(Unleaded.Gasoline.Sale..UGS.,2)]

added3=lm(Unleaded.Gasoline.Sale..UGS.~trend+Quarter_class+RNUV+lag4,data = data_copy)

summary(added3)

  #We see a decrease in adjusted R squared and p values seem distorted. Actually, I am not sure why this is happened
# but I think that lag4 variable includes some aspects of already used regressors and multicollinearity distorted
#our model. We will use "added2" model as the last model. There could have been models that higher Rsquared value,
#however, they will include too correlated variables as regressors or lead to overfitting. We have an consensus
#among bias and variance

last_model=added2
summary(last_model)

#Validity of last model
checkresiduals(last_model)
#we do not see autocorrelation between lagged residuals, which is a nice indicator that our model explains 
#response variable satisfactorily.

mean(last_model$residuals)
plot(last_model)

#Forecast


predictions=predict(last_model,newdata=data_copy[29:32,])

predictions

#Conclusion
#We have used various regression models, compare them in several aspects. It was non-trivial to find independent variables
#multicollinearity would be occured if correlated regressors were used. At the end, the model seems satisfactory since resi-
#dual analysis fits the regression assumptions




#References
#https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/

