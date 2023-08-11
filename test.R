### Data Part

## Weather

library(rjson)
result <- fromJSON(file = "weather.json")
locations = result[["cwbdata"]][["resources"]][["resource"]][["data"]][["surfaceObs"]][["location"]]

# check data
for (i in 1:length(locations)){
  print(locations[[i]][["station"]]["StationName"])
} # taipei: 5

# extract taipei data
taipei = locations[[5]]
taipei_data = taipei[["stationObsTimes"]][["stationObsTime"]]

# check taipei data
taipei_data[[1]][["DataTime"]]
taipei_data[[1]][["weatherElements"]]

# fill null
for (i in 1:length(taipei_data)){
  if (is.null(taipei_data[[i]]$weatherElements$SunshineDuration)){
    taipei_data[[i]]$weatherElements$SunshineDuration = ""}
}

# to dataframe
col_num = length(unlist(taipei_data[[1]]))
df_weather = as.data.frame(matrix(unlist(taipei_data), byrow = T, ncol = col_num))
names(df_weather) = names(unlist(taipei_data)[1:col_num])

df_weather$DataTime = substr(gsub("T", " ", df_weather$DataTime), 1, 19)
df_weather$DataTime = as.POSIXct(df_weather$DataTime, format="%Y-%m-%d %H:%M:%S")

df_weather$weatherElements.Precipitation[df_weather$weatherElements.Precipitation == "T"] = 0.1
df_weather$weatherElements.SunshineDuration[df_weather$weatherElements.SunshineDuration == ""] = 0.0
df_weather$weatherElements.SunshineDuration = as.double(df_weather$weatherElements.SunshineDuration)

head(df_weather, 50)
nrow(df_weather)

## Bike for training

raw = read.csv("Bike.csv")
head(raw)

rent_data = raw[raw$rent_station == "捷運公館站(2號出口)" | raw$rent_station == "捷運公館站(3號出口)",]
head(rent_data)

return_data = raw[raw$return_station == "捷運公館站(2號出口)" | raw$return_station == "捷運公館站(3號出口)",]
head(return_data)

rent_data$DataTime = as.POSIXct(rent_data$rent_time, format="%Y-%m-%d %H:%M:%S")
return_data$DataTime = as.POSIXct(return_data$return_time, format="%Y-%m-%d %H:%M:%S")

rent_num = aggregate(rent_data$rent_station, list(rent_data$DataTime), function(x) length(x))
colnames(rent_num) = c("DataTime", "Rent")
head(rent_num)

return_num = aggregate(return_data$return_station, list(return_data$DataTime), function(x) length(x))
colnames(return_num) = c("DataTime", "Return")
head(return_num)

df_bike = merge(rent_num, return_num, by = "DataTime", all = T)
df_bike[is.na(df_bike)] = 0

head(df_bike, 10)
nrow(df_bike)

## Bike for testing

raw = read.csv("Bike2.csv")
head(raw)

rent_data = raw[raw$rent_station == "捷運公館站(2號出口)" | raw$rent_station == "捷運公館站(3號出口)",]
head(rent_data)

return_data = raw[raw$return_station == "捷運公館站(2號出口)" | raw$return_station == "捷運公館站(3號出口)",]
head(return_data)

rent_data$DataTime = as.POSIXct(rent_data$rent_time, format="%Y-%m-%d %H:%M:%S")
return_data$DataTime = as.POSIXct(return_data$return_time, format="%Y-%m-%d %H:%M:%S")

rent_num = aggregate(rent_data$rent_station, list(rent_data$DataTime), function(x) length(x))
colnames(rent_num) = c("DataTime", "Rent")
head(rent_num)

return_num = aggregate(return_data$return_station, list(return_data$DataTime), function(x) length(x))
colnames(return_num) = c("DataTime", "Return")
head(return_num)

df_bike2 = merge(rent_num, return_num, by = "DataTime", all = T)
df_bike2[is.na(df_bike2)] = 0

head(df_bike2, 10)
nrow(df_bike2)

## merge for training

df = merge(df_weather, df_bike, by = "DataTime")
head(df)

df$Sufficient = 0
df$Sufficient[df$Rent < df$Return] = 1
df[is.na(df)] = 0

df = df[,c(1,9,10,11,2,3,4,5,7,8)]
colnames(df) = c("DataTime", "Rent", "Return", "Sufficient", "AirPressure", "AirTemperature", "RelativeHumidity", "WindSpeed", "Precipitation", "SunshineDuration")
head(df)

#write.csv(df, "data.csv", row.names = F)

## merge for testing

df2 = merge(df_weather, df_bike2, by = "DataTime")
head(df2)

df2$Sufficient = 0
df2$Sufficient[df2$Rent < df2$Return] = 1
df2[is.na(df2)] = 0

df2 = df2[,c(1,9,10,11,2,3,4,5,7,8)]
colnames(df2) = c("DataTime", "Rent", "Return", "Sufficient", "AirPressure", "AirTemperature", "RelativeHumidity", "WindSpeed", "Precipitation", "SunshineDuration")
head(df2)

#write.csv(df2, "data2.csv", row.names = F)


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
