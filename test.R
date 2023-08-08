### Modeling Part

## read data for training
df = read.csv("data.csv")
dim(df)
head(df)

library(lubridate)
date = substr(as.character(df$DataTime),1,10)
time = substr(as.character(df$DataTime),12,19)
week = strftime(parse_date_time(date, orders = "Ymd"), format = "%A")
hour = hour(parse_date_time(time, orders = "HMS"))

df$Day = "Weekday"
df$Day[week == "Saturday" | week == "Sunday"] = "Weekend"

df$Time = "Other"
df$Time[hour == 9 | hour == 14] = "ClassStart"
df$Time[hour == 12 | hour == 17] = "ClassEnd"
head(df)

## read data for testing
df2 = read.csv("data2.csv")
dim(df2)
head(df2)

library(lubridate)
date = substr(as.character(df2$DataTime),1,10)
time = substr(as.character(df2$DataTime),12,19)
week = strftime(parse_date_time(date, orders = "Ymd"), format = "%A")
hour = hour(parse_date_time(time, orders = "HMS"))

df2$Day = "Weekday"
df2$Day[week == "Saturday" | week == "Sunday"] = "Weekend"

df2$Time = "Other"
df2$Time[hour == 9 | hour == 14] = "ClassStart"
df2$Time[hour == 12 | hour == 17] = "ClassEnd"
head(df2)

## plot
plot(df$AirPressure, df$Sufficient)
plot(df$AirTemperature, df$Sufficient)
plot(df$RelativeHumidity, df$Sufficient)
plot(df$WindSpeed, df$Sufficient)
plot(df$Precipitation, df$Sufficient)
plot(df$SunshineDuration, df$Sufficient)
boxplot(df$Sufficient~df$Day)
boxplot(df$Sufficient~df$Time)

## modeling
df$Day = relevel(factor(df$Day), ref = "Weekend")
df$Time = relevel(factor(df$Time), ref = "Other")
glm = glm(Sufficient ~ Day + Time + AirPressure + AirTemperature + RelativeHumidity + WindSpeed + Precipitation + SunshineDuration, data = df)
summary(glm)

## selection
library(leaps)
best.fit = regsubsets(Sufficient ~ ., data = df[,c(4:12)], nvmax = 16)
summary(best.fit)
coef(best.fit,which.min(summary(best.fit)$bic)) 

forward.fit = regsubsets(Sufficient ~ ., data = df[,c(4:12)], nvmax = 16, method = "forward") 
summary(forward.fit)
coef(forward.fit,which.min(summary(forward.fit)$bic)) 

backward.fit = regsubsets(Sufficient ~ ., data = df[,c(4:12)], nvmax = 16, method = "backward") 
summary(backward.fit)
coef(backward.fit,which.min(summary(backward.fit)$bic)) 

m1 = glm(Sufficient ~ Time + AirPressure + RelativeHumidity + WindSpeed, data = df, family = binomial())
summary(m1)

m2 = glm(Sufficient ~ Day + Time + AirPressure + AirTemperature + RelativeHumidity + WindSpeed, data = df, family = binomial())
summary(m2)

m3 = glm(Sufficient ~ Time + AirPressure + RelativeHumidity + WindSpeed, data = df, family = binomial())
summary(m3)

## testing

testing = df2
pred = predict(m1, newdata = testing, type = "response")
pred = round(pred)
table(pred, testing$Sufficient)
sum(diag(table(pred, testing$Sufficient)))/sum(table(pred, testing$Sufficient))

## ROC

#install.packages("pROC")
library(pROC)
rr = roc(as.numeric(pred), as.numeric(testing$Sufficient))
rr$auc
plot(rr, col="red")
