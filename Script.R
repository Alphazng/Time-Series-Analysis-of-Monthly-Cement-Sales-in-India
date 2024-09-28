library(tseries)
library(ggplot2)
library(car)
library(forecast)
library(lmtest)
library(nortest)
library(MASS)
library(stats)
library(caret)

# 1. Collect data
df1 = read.csv(file.choose(), header=T)
head(df1)
summary(df1)


# 2. data preprocessing
missing_values <- sum(is.na(df1))
print(missing_values)


df1$Month <- as.character(df1$Month)

df1
unique(df1$Month)
df1$Month <- ifelse(substr(df1$Month, 1, 2) == "10" | substr(df1$Month, 1, 2) == "11" | substr(df1$Month, 1, 2) == "12", 
                    paste("20", df1$Month, sep = ""), 
                    paste("20", df1$Month, sep = ""))
head(df1)

df1$Month <- as.Date(paste(df1$Month, "01", sep="-"), format="%Y-%b-%d")
df1

datats <- ts(df1$Sales, start = c(2010, 1), frequency = 12)
datats

# 3. checking pattern of the data
plot.ts(datats) #seasonal data

# 4. model making
# train_end <- floor(0.9 * length(datats))
start(datats)
end(datats)

start_year = 2010
end_year = 2022

train_data <- window(datats, start = c(start_year, 1), end = c(end_year - 1, 12))
test_data <- window(datats, start = c(end_year, 1), end = c(end_year, 11))

train_data
test_data

length(train_data)
length(test_data)

train = df1[,2][1:144]

# a. ARIMA
# check stasioner thdp varians
power_transform_result <- powerTransform(train_data)
summary(power_transform_result) #tidak stasioner dgn alpha = 0.01

train_data2 = train_data^(1/2)
train_data2

power_transform_result <- powerTransform(train_data2)
summary(power_transform_result)#stasioner dgn alpha = 0.01

#check stasioner thdp mean
adf.test(train_data2) #stasioner thdp mean

#cari ordo p dan q
pacf(train)
acf(train)


#auto arima
model_arima_auto <- auto.arima(train_data2, d=0, seasonal=TRUE)
summary(model_arima_auto)


model_SARIMA1 = arima(train_data2, order=c(2,0,0), seasonal=list(order=c(0,1,1), period=12))
coeftest(model_SARIMA1) # Signifikan
summary(model_SARIMA1)

model_SARIMA2 = arima(train_data2, order=c(1, 0, 1), seasonal=list(order=c(1, 0, 1), period=12))
coeftest(model_SARIMA2) # Tidak Signifikan

model_SARIMA3 = arima(train_data2, order=c(2, 0, 1), seasonal=list(order=c(2, 0, 1), period=12))
coeftest(model_SARIMA3) # Tidak Signifikan

model_SARIMA4 = arima(train_data2, order=c(0, 0, 1), seasonal=list(order=c(0, 0, 1), period=12))
coeftest(model_SARIMA4) # Signifikan
summary(model_SARIMA4)

model_SARIMA5 = arima(train_data2, order=c(2, 0, 0), seasonal=list(order=c(2, 0, 0), period=12))
coeftest(model_SARIMA5) # Tidak Signifikan

model_SARIMA6 = arima(train_data2, order=c(1, 0, 2), seasonal=list(order=c(1, 0, 2), period=12))
coeftest(model_SARIMA6) # Tidak Signifikan

model_SARIMA7 = arima(train_data2, order=c(2, 0, 2), seasonal=list(order=c(2, 0, 2), period=12))
coeftest(model_SARIMA7) # Tidak Signifikan

model_SARIMA8 = arima(train_data2, order=c(1, 0, 0), seasonal=list(order=c(1, 0, 0), period=12))
coeftest(model_SARIMA8) # Signifikan
summary(model_SARIMA8)

model_SARIMA9 = arima(train_data2, order=c(0, 0, 2), seasonal=list(order=c(0, 0, 2), period=12))
coeftest(model_SARIMA9) # Signifikan
summary(model_SARIMA9)


#uji asumsi
err1 <- residuals(model_SARIMA1)
Box.test(err1, type="Ljung-Box") #lulus uji asumsi white noise
lillie.test(err1) #lulus uji distribusi normal

err2 <- residuals(model_SARIMA4)
Box.test(err2, type="Ljung-Box") # Tidak lulus uji asumsi white noise
lillie.test(err2)#lulus uji distribusi normal

err3 <- residuals(model_SARIMA8)
Box.test(err3, type="Ljung-Box") #lulus uji asumsi white noise
lillie.test(err3)  #lulus uji distribusi normal

err4 <- residuals(model_SARIMA9)
Box.test(err4, type="Ljung-Box") #lulus uji asumsi white noise
lillie.test(err4)#lulus uji distribusi normal

# predict
forecast_values1 <- forecast(model_SARIMA1, h = length(test_data))$mean
forecast_values2 <- forecast(model_SARIMA8, h = length(test_data))$mean
forecast_values3 <- forecast(model_SARIMA9, h = length(test_data))$mean

# Transform forecasted values back to original scale
forecast_values_original1 <- forecast_values1^2
forecast_values_original2 <- forecast_values2^2
forecast_values_original3<- forecast_values3^2

# Calculate residuals
residuals1 <- test_data - forecast_values_original1
residuals2 <- test_data - forecast_values_original2
residuals3 <- test_data - forecast_values_original3

# Compute MSE
mse1 <- mean(residuals1^2)
mse2 <- mean(residuals2^2)
mse3 <- mean(residuals3^2)
mse1
mse2
mse3
# Compute RMSE
rmse1 <- sqrt(mse1)
rmse2 <- sqrt(mse2)
rmse3 <- sqrt(mse3)
rmse1
rmse2
rmse3


# b. Model Regresi timeseries

# Create a dummy variable for the time series data
train_reg = df1[,2][1:144] # bikin train manual
train_reg = data.frame(train_reg)
train_reg

#Initialize a counter
counter <- 1
month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#Loop over each row in the dataframe
for (i in 1:144) {
  # Assign month value based on counter
  train_reg$month[i] <- month[counter]
  
  #Increment the counter
  counter <- counter + 1
  
  #Reset counter if it exceeds 12
  if (counter > 12) {
    counter <- 1
  }
}

train_reg
dummy <- dummyVars("~.", data = train_reg)

hasil <- data.frame(predict(dummy, newdata = train_reg))
hasil$t <- seq(1:nrow(hasil))
hasil

# Remove unnecessary columns
dataset <- hasil[, -c(5)]
head(dataset)

# Fit the linear regression model
regModel <- lm(train_reg~., data = dataset)

# Print the summary of the model
summary(regModel)

#Uji asumsi
error_reg = residuals(regModel)
lillie.test(error_reg) 
bptest(regModel) 
dwtest(regModel)

regMSE = mean(error_reg^2)
regMSE

regRMSE = sqrt(regMse)
regRMSE

# LAG 1 TimeSeries Regression
dataset
nrow(dataset)
a = dataset[2:nrow(dataset), ]
a
nrow(a)
xt1 = data.frame(dataset$train_reg[1:143])
xt1


lag1 = data.frame(a,xt1)
head(lag1)
regModel2 = lm(train_reg~., data=lag1)
summary(regModel2)

#Uji asumsi
error_reg2 = residuals(regModel2)
lillie.test(error_reg2) #lulus
bptest(regModel2) #ga lulus
dwtest(regModel2) #lulus


# LAG 2 TimeSeries Regression
dataset
nrow(dataset)
a2 = dataset[3:nrow(dataset), ]
a2
nrow(a2)
xt2 = data.frame(dataset$train_reg[1:142])
xt2


lag2 = data.frame(a2,xt2)
head(lag2)
regModel3 = lm(train_reg~., data=lag2)
summary(regModel3)

#Uji asumsi
error_reg3 = residuals(regModel3)
lillie.test(error_reg3) #ga lulus
bptest(regModel3) #lulus
dwtest(regModel3) #ga lulus

# LAG 3 TimeSeries Regression
dataset
nrow(dataset)
a3 = dataset[4:nrow(dataset), ]
a3
nrow(a3)
xt3 = data.frame(dataset$train_reg[1:141])
xt3


lag3 = data.frame(a3,xt3)
head(lag3)
regModel4 = lm(train_reg~., data=lag3)
summary(regModel4)

#Uji asumsi
error_reg4 = residuals(regModel4)
lillie.test(error_reg4) #ga lulus
bptest(regModel4) #lulus
dwtest(regModel4) #ga lulus

#kesimpulan time series regression: karna pada setiap model tidak ada yang lolos semua uji asumsi, 
#maka model time series regression tidak cocok dipakai pada kasus ini


# TES
tes_model1 <- HoltWinters(train_data, seasonal = "additive")
print(tes_model1)

fitted_values <- fitted(tes_model1)

# Compute residuals for the training data
residuals <- train_data - fitted_values[, 1]

# Compute MSE for the training data
mse_train <- mean(residuals^2)
print(paste("Training MSE:", round(mse_train, 2)))

# Compute RMSE for the training data
rmse_train <- sqrt(mse_train)
print(paste("Training RMSE:", round(rmse_train, 2)))
  
forecast_values1 <- forecast(tes_model1, h = 12)
actual_values <- test_data
  
# Compute MSE
mse1 <- mean((forecast_values1$mean - actual_values)^2)
print(paste("MSE:", round(mse1, 2)))
  
# Compute RMSE
rmse1 <- sqrt(mean((forecast_values1$mean - actual_values)^2))
print(paste("RMSE:", round(rmse1, 2)))

alpha <- tes_model$alpha
beta <- tes_model$beta
gamma <- tes_model$gamma
print(paste("Alpha:", alpha))
print(paste("Beta:", beta))
print(paste("Gamma:", gamma))

#TES model 2
tes_model2 <- HoltWinters(train_data, alpha=0.5, beta=0.1, gamma=0.8, seasonal = "multiplicative")
print(tes_model2)

fitted_values <- fitted(tes_model2)

# Compute residuals for the training data
residuals <- train_data - fitted_values[, 1]

# Compute MSE for the training data
mse_train <- mean(residuals^2)
print(paste("Training MSE:", round(mse_train, 2)))

# Compute RMSE for the training data
rmse_train <- sqrt(mse_train)
print(paste("Training RMSE:", round(rmse_train, 2)))

forecast_values2 <- forecast(tes_model2, h = 12)
actual_values <- test_data

# Compute MSE
mse2 <- mean((forecast_values2$mean - actual_values)^2)
print(paste("MSE:", round(mse2, 2)))

# Compute RMSE
rmse2 <- sqrt(mean((forecast_values2$mean - actual_values)^2))
print(paste("RMSE:", round(rmse2, 2)))

# TES Model 3
tes_model3 <- HoltWinters(train_data, alpha=0.3, beta=0.1, gamma=0.8, seasonal = "multiplicative")
print(tes_model3)

fitted_values <- fitted(tes_model3)
# Compute residuals for the training data
residuals <- train_data - fitted_values[, 1]

# Compute MSE for the training data
mse_train <- mean(residuals^2)
print(paste("Training MSE:", round(mse_train, 2)))

# Compute RMSE for the training data
rmse_train <- sqrt(mse_train)
print(paste("Training RMSE:", round(rmse_train, 2)))

forecast_values3 <- forecast(tes_model3, h = 12)
actual_values <- test_data

# Compute MSE
mse3 <- mean((forecast_values3$mean - actual_values)^2)
print(paste("MSE:", round(mse3, 2)))

# Compute RMSE
rmse3 <- sqrt(mean((forecast_values3$mean - actual_values)^2))
print(paste("RMSE:", round(rmse3, 2)))

# TES Model 4
tes_model4 <- HoltWinters(train_data, alpha=0.3, beta=0.5, gamma=0.8, seasonal = "additive")
print(tes_model4)

fitted_values <- fitted(tes_model4)
# Compute residuals for the training data
residuals <- train_data - fitted_values[, 1]

# Compute MSE for the training data
mse_train <- mean(residuals^2)
print(paste("Training MSE:", round(mse_train, 2)))

# Compute RMSE for the training data
rmse_train <- sqrt(mse_train)
print(paste("Training RMSE:", round(rmse_train, 2)))

forecast_values4 <- forecast(tes_model4, h = 12)
actual_values <- test_data

# Compute MSE
mse4 <- mean((forecast_values4$mean - actual_values)^2)
print(paste("MSE:", round(mse4, 2)))

# Compute RMSE
rmse4 <- sqrt(mean((forecast_values4$mean - actual_values)^2))
print(paste("RMSE:", round(rmse4, 2)))

# TES Model 5
tes_model5 <- HoltWinters(train_data, alpha=0.3, beta=0.1, gamma=0.5, seasonal = "additive")
print(tes_model5)


fitted_values <- fitted(tes_model5)
# Compute residuals for the training data
residuals <- train_data - fitted_values[, 1]

# Compute MSE for the training data
mse_train <- mean(residuals^2)
print(paste("Training MSE:", round(mse_train, 2)))

# Compute RMSE for the training data
rmse_train <- sqrt(mse_train)
print(paste("Training RMSE:", round(rmse_train, 2)))
forecast_values5 <- forecast(tes_model5, h = 12)
actual_values <- test_data

# Compute MSE
mse5 <- mean((forecast_values5$mean - actual_values)^2)
print(paste("MSE:", round(mse5, 2)))

# Compute RMSE
rmse5 <- sqrt(mean((forecast_values5$mean - actual_values)^2))
print(paste("RMSE:", round(rmse5, 2)))

# TES Model 6
tes_model6 <- HoltWinters(train_data, alpha=0.2, beta=0.1, gamma=0.3, seasonal = "additive")
print(tes_model6)

fitted_values <- fitted(tes_model6)
# Compute residuals for the training data
residuals <- train_data - fitted_values[, 1]

# Compute MSE for the training data
mse_train <- mean(residuals^2)
print(paste("Training MSE:", round(mse_train, 2)))

# Compute RMSE for the training data
rmse_train <- sqrt(mse_train)
print(paste("Training RMSE:", round(rmse_train, 2)))
forecast_values6 <- forecast(tes_model6, h = 12)
actual_values <- test_data

# Compute MSE
mse6 <- mean((forecast_values6$mean - actual_values)^2)
print(paste("MSE:", round(mse6, 2)))

# Compute RMSE
rmse6 <- sqrt(mean((forecast_values6$mean - actual_values)^2))
print(paste("RMSE:", round(rmse6, 2)))



# NAive seasonal
naive_model <- snaive(train_data)
summary(naive_model)

fitted_values_naive <- fitted(naive_model)

# Compute residuals for the training data
residuals_naive <- train_data - fitted_values_naive

# Compute MSE for the training data
mse_train_naive <- mean(residuals_naive^2, na.rm = TRUE)
print(mse_train_naive)

# Compute RMSE for the training data
rmse_train_naive <- sqrt(mse_train_naive)
print(rmse_train_naive)
forecast_values_naive <- forecast(naive_model, h = 12) # forecasting for next 12 periods

# Extract actual values from the test data set
actual_values_naive <- test_data

# Compute MSE
mse_naive <- mean((forecast_values_naive$mean - actual_values_naive)^2)
mse_naive

# Compute RMSE
rmse_naive <- sqrt(mean((forecast_values_naive$mean - actual_values_naive)^2))
rmse_naive


# Neural Network

#Model NN2
nn2 = nnetar(train_data, p=1, size = 5)
nn2
fitted_val2 = nn2$fitted
fitted_val2
accuracy(nn2)

#Model NN2 dengan test_data
nn2 = nnetar(test_data, p=1, size = 5)
nn2
fitted_val2 = nn2$fitted
fitted_val2
accuracy(nn2)

#Model NN3
nn3 = nnetar(train_data, p=1, size = 10)
nn3
fitted_val3 = nn3$fitted
fitted_val3
accuracy(nn3)

#Model NN3 dengan test_data
nn3 = nnetar(test_data, p=1, size = 10)
nn3
fitted_val3 = nn3$fitted
fitted_val3
accuracy(nn3)

#Model NN4
nn4 = nnetar(train_data, p=1, size = 3)
nn4
fitted_val4 = nn4$fitted
fitted_val4
accuracy(nn4)

#Model NN4 dengan test_data
nn4 = nnetar(test_data, p=1, size = 3)
nn4
fitted_val4 = nn4$fitted
fitted_val4
accuracy(nn4)



