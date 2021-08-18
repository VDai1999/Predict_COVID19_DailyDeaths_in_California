# Load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(forecast)
library(knitr)   
library(stringr)
library(dplyr)
library(TSA)
library(xts)
library(fpp2)
library(tseries)
library(stats)
library(gridExtra)


# Read the data sets
setwd("/Users/daidong/Downloads/STA483/Final Project")
cdata <- read.csv("data_table_for_daily_death_trends__california.csv", skip=2)
fdata <- read.csv("data_table_for_daily_death_trends__florida.csv", skip=2)


# Clean data
clean_dat <- function(data) {
  data <- data %>%
    mutate(Date = mdy(Date)) %>%
    select(-c(State, X7.Day.Moving.Avg))
  
  # Reverse the order of the data
  data <- data %>% map_df(rev)
  
  return(data)
}
cdata <- clean_dat(cdata) # California data
fdata <- clean_dat(fdata) # Florida data


# Since COVID-19 was declared as a global pandemic on March 11, 2020 and there were barely no 
# deaths due to COVID-19 in California before March 11, 2020, we decided to only analyze data after March 11, 2020.
# Delete the first 49 observations
cdata <- tail(cdata, nrow(cdata)-49) # California data
fdata <- tail(fdata, nrow(fdata)-49) # Florida data
head(cdata)
head(fdata)


# Create time-series objects
cali_ts <- ts(cdata[,2], start = 1) # California
florida.ts <- ts(fdata[,2], start = 1) # Florida


# Time-series plots
plot_df <- cbind(cdata, fdata[,2]) %>%
  rename(Cali_NDeaths = "New.Deaths") %>%
  rename(Flo_NDeaths = "New.Deaths")
ggplot(data=plot_df) +
  geom_line(aes(x=Date, y=Cali_NDeaths, color="Cali_NDeaths")) + 
  geom_line(aes(x=Date, y=Flo_NDeaths, color="Flo_NDeaths")) + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y") +
  labs(x = "Time",
       y = "Daily Deaths",
       title = "Daily Deaths in California and Florida, U.S",
       subtitle = "From March 11, 2020 to April 26, 2021", 
       caption = "Source: Centers for Disease Control and Prevention (CDC)") +
  scale_colour_manual(values = c("Cali_NDeaths" = "red", "Flo_NDeaths" = "black"),
                      name = "",
                      labels = c("California", "Florida")) +
  theme_bw()


# ACF plots of the original data
grid.arrange(
  # California
  ggAcf(cali_ts, lag.max = 30) +
    labs(caption = "California",
         title = "",
         x = "Lag",
         y = "ACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  ggAcf(florida.ts, lag.max = 30) +
    labs(caption = "Florida",
         title = "",
         x = "Lag",
         y = "ACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)

# PACF plots of the original data
grid.arrange(
  # California
  ggPacf(cali_ts, lag.max = 30) +
    labs(caption = "California",
         title = "",
         x = "Lag",
         y = "PACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  ggPacf(florida.ts, lag.max = 30) +
    labs(caption = "Florida",
         title = "",
         x = "Lag",
         y = "PACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# Split data into the training and test set
# Training set: From March 11, 2020 to April 19, 2021
# Test set: From April 20, 2021 to April 26, 2021
# California
cali_train <- window(cali_ts, end=c(405)) #
cali_test <- window(cali_ts, start=c(406), end=c(412))
# Florida
flor.train <- window(florida.ts, end=c(405)) 
flor.test <- window(florida.ts, start=c(406), end=c(412))


# Create time-series plots for the training data
grid.arrange(
  # California
  autoplot(cali_train) +
    labs(caption = "California",
         y = "Daily Deaths",
         title = "") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  autoplot(flor.train) +
    labs(caption = "Florida",
         y = "Daily Deaths",
         title = "") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# ACF plots of the training data
grid.arrange(
  # California
  ggAcf(cali_train) +
    labs(caption = "California",
         title = "",
         x = "Lag",
         y = "ACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  ggAcf(flor.train) +
    labs(caption = "Florida",
         title = "",
         x = "Lag",
         y = "ACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# Dickey-Fuller test to check stationary
adf.test(cali_train) # California
adf.test(flor.train) # Florida


# auto.arima() to select the number of differencing
# Use all 3 criteria: BIC, AIC, and AICc
# California
auto.arima(cali_train, ic="bic")
auto.arima(cali_train, ic="aic")
auto.arima(cali_train, ic="aicc")
# FLorida
auto.arima(flor.train, ic="bic")
auto.arima(flor.train, ic="aic")
auto.arima(flor.train, ic="aicc")


# Time-series plots of the first differencing
grid.arrange(
  # California
  autoplot(diff(cali_train)) +
    labs(caption = "California",
         y = "Daily Deaths",
         title = "") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  autoplot(diff(flor.train)) +
    labs(caption = "Florida",
         y = "Daily Deaths",
         title = "") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# Dickey-Fuller test to check stationary of the first differencing
adf.test(diff(cali_train)) # California
adf.test(diff(flor.train)) # Florida


# ACF plots of the first differencing
grid.arrange(
  # California
  ggAcf(diff(cali_train)) +
    labs(caption = "California",
         title = "",
         x = "Lag",
         y = "ACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  ggAcf(diff(flor.train)) +
    labs(caption = "Florida",
         title = "",
         x = "Lag",
         y = "ACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# PACF plots of the first differencing
grid.arrange(
  # California
  ggPacf(diff(cali_ts), lag.max = 30) +
    labs(caption = "California",
         title = "",
         x = "Lag",
         y = "PACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  ggPacf(diff(florida.ts), lag.max = 30) +
    labs(caption = "Florida",
         title = "",
         x = "Lag",
         y = "PACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# Fit models
# California
# ARIMA(4,1,2) model
arima_mod_cali <- auto.arima(cali_train)
arima_mod_cali
# ARI(6,1) model
ari_mod_cali <- Arima(cali_train, order=c(6, 1, 0))
ari_mod_cali
# Check stationary for the AR(6,1) model using the unit root test
Mod(polyroot(c(1, 0.6453, 0.6389, 0.6579 , 0.6421, 0.6389, 0.4276)))

# Florida
# ARIMA(2,1,2) model
arima_mod_flo <- auto.arima(flor.train)
arima_mod_flo
# ARI(6,1) model
ari_mod_flo <- Arima(flor.train, order=c(6, 1, 0))
ari_mod_flo
# Check stationary for the AR(6,1) model using the unit root test
Mod(polyroot(c(1, 0.7907, 0.7214, 0.6924, 0.6731, 0.6942, 0.4385)))


# Log-transformation to stabalize the variance of the series
grid.arrange(
  # California
  autoplot(log(cali_train+1)) +
    labs(caption = "California",
         y = "Log of Daily Deaths",
         title = "") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  autoplot(log(flor.train+1)) +
    labs(caption = "Florida",
         y = "Log of Daily Deaths",
         title = "") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# Dickey-Fuller test to check stationary
adf.test(log(cali_train+1))
adf.test(log(flor.train+1))


# auto.arima() to select the diffencing and orders of the models
# California
auto.arima(log(cali_train+1), ic="bic")
auto.arima(log(cali_train+1), ic="aic")
auto.arima(log(cali_train+1), ic="aicc")
# Florida
auto.arima(log(flor.train+1), ic="bic")
auto.arima(log(flor.train+1), ic="aic")
auto.arima(log(flor.train+1), ic="aicc")


# Time-series plots of the first differencing of the log-transformed series
grid.arrange(
  # California
  autoplot(diff(log(cali_train+1))) +
    labs(caption = "California",
         y = "Log of Daily Deaths",
         title = "") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  autoplot(diff(log(flor.train+1))) +
    labs(caption = "Florida",
         y = "Log of Daily Deaths",
         title = "") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# ACF plots of the first differencing of the log-transformed series
grid.arrange(
  # California
  ggAcf(diff(log(cali_train+1))) +
    labs(caption = "California",
         title = "",
         x = "Lag",
         y = "ACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  ggAcf(diff(log(flor.train+1))) +
    labs(caption = "Florida",
         title = "",
         x = "Lag",
         y = "ACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# PACF plots of the first differencing
grid.arrange(
  # California
  ggPacf(diff(log(cali_ts+1))) +
    labs(caption = "California",
         title = "",
         x = "Lag",
         y = "PACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  ggPacf(diff(log(florida.ts+1))) +
    labs(caption = "Florida",
         title = "",
         x = "Lag",
         y = "PACF") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# Fit models
# California
# ARIMA(5,1,2) model
arima_log_mod1_cali <- auto.arima(log(cali_train+1), ic="bic")
arima_log_mod1_cali 
# ARIMA(5,1,3) model
arima_log_mod2_cali <- auto.arima(log(cali_train+1), ic="aic")
arima_log_mod2_cali

# Florida
# ARIMA(5,1,2) model
arima_log_mod1_flo <- auto.arima(log(flor.train+1), ic="bic")
arima_log_mod1_flo
# ARIMA(5,1,3) model
arima_log_mod2_flo <- auto.arima(log(flor.train+1), ic="aic")
arima_log_mod2_flo


# Additive exponential smoothing and additive Holt-Winters for trend only models
# California
# Additive exponential smoothing
cali.exp.add <- HoltWinters(cali_train, beta=FALSE, gamma=FALSE, seasonal = c("additive"))
cali.exp.add
# Additive Holt for trend
cali.trend.add <- HoltWinters(cali_train, gamma=FALSE, seasonal = c("additive"))
cali.trend.add

# Florida
# Additive exponential smoothing
flor.exp.add <- HoltWinters(flor.train, beta=FALSE, gamma=FALSE, seasonal = c("additive"))
flor.exp.add
# Additive Holt for trend
flor.trend.add <- HoltWinters(flor.train, gamma=FALSE, seasonal = c("additive"))
flor.trend.add


# Cross-validation: Fit models to test sets
# California
arima_mod_est_cali <- forecast(arima_mod_cali, h=length(cali_test))
ari_mod_est_cali <- forecast(ari_mod_cali, h=length(cali_test))
arima_log_mod1_est_cali <- forecast(arima_log_mod1_cali, h=length(cali_test))
arima_log_mod2_est_cali <- forecast(arima_log_mod2_cali, h=length(cali_test))
cali.exp.add_est <- forecast(cali.exp.add, h=length(cali_test))
cali.trend.add_est <- forecast(cali.trend.add, h=length(cali_test))
# Florida
arima_mod_est_flo <- forecast(arima_mod_flo, h=length(flor.test))
ari_mod_est_flo <- forecast(ari_mod_flo, h=length(flor.test))
arima_log_mod1_est_flo <- forecast(arima_log_mod1_flo, h=length(flor.test))
arima_log_mod2_est_flo <- forecast(arima_log_mod2_flo, h=length(flor.test))
flor.exp.add_est <- forecast(flor.exp.add, h=length(flor.test))
flor.trend.add_est <- forecast(flor.trend.add, h=length(flor.test))


# Plot the predicted daily death of four models (except for two log-transfored models) along with the actual daily deaths
grid.arrange(
  # California
  autoplot(window(cali_train, start=c(390))) + 
    autolayer(arima_mod_est_cali, PI=FALSE, series="ARIMA(4,1,2) ") +
    autolayer(ari_mod_est_cali, PI=FALSE, series="ARI(6,1)") +
    autolayer(cali.exp.add_est, PI=FALSE, series="Exp Smoothing") +
    autolayer(cali.trend.add_est, PI=FALSE, series="HW Trend Only") +
    autolayer(cali_test, series="True Daily Deaths", size=1.5) +
    labs(x="Time", 
         y="Daily Deaths",
         caption="California") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  # Florida
  autoplot(window(flor.train, start=c(390))) + 
    autolayer(arima_mod_est_flo, PI=FALSE, series="ARIMA(2,1,2) ") +
    autolayer(ari_mod_est_flo, PI=FALSE, series="ARI(6,1)") +
    autolayer(flor.exp.add_est, PI=FALSE, series="Exp Smoothing") +
    autolayer(flor.trend.add_est, PI=FALSE, series="HW Trend Only") +
    autolayer(flor.test, series="True Daily Deaths", size=1.5) +
    labs(x="Time", 
         y="Daily Deaths",
         caption="Florida") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# Plot the predicted daily death of 2 transformed models along with the actual daily deaths
grid.arrange(
  autoplot(window(log(cali_train+1), start=c(390))) + 
    autolayer(arima_log_mod1_est_cali, PI=FALSE, series="Log-transformed ARIMA(5,1,2)") +
    autolayer(arima_log_mod2_est_cali, PI=FALSE, series="Log-transformed ARIMA(5,1,3)") +
    autolayer(log(cali_test+1), series="True Daily Deaths", size=1.5) +
    labs(x="Time", 
         y="Daily Deaths",
         caption="California") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  autoplot(window(log(flor.train+1), start=c(390))) + 
    autolayer(arima_log_mod1_est_flo, PI=FALSE, series="Log-transformed ARIMA(5,1,2)") +
    autolayer(arima_log_mod2_est_flo, PI=FALSE, series="Log-transformed ARIMA(5,1,5)") +
    autolayer(log(flor.test+1), series="True Daily Deaths", size=1.5) +
    labs(x="Time", 
         y="Daily Deaths",
         caption="Florida") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)


# Summary table reports the RMSE, MAE, and MAPE of 4 models (except for 2 log-transformed models)
# California
summary_table <- rbind(accuracy(arima_mod_est_cali, cali_test)[2,c(2, 3, 5)],
                       accuracy(ari_mod_est_cali, cali_test)[2,c(2, 3, 5)],
                       accuracy(cali.exp.add_est, cali_test)[2,c(2, 3, 5)],
                       accuracy(cali.trend.add_est, cali_test)[2,c(2, 3, 5)])
rownames(summary_table) <- c("ARIMA(4,1,2)", "ARI(6,1)", "Additive Exp Smoothing", "Additive HW (Trend)")
kable(summary_table)
# Florida
summary_table <- rbind(accuracy(arima_mod_est_flo, flor.test)[2,c(2, 3, 5)],
                       accuracy(ari_mod_est_flo, flor.test)[2,c(2, 3, 5)],
                       accuracy(flor.exp.add_est, flor.test)[2,c(2, 3, 5)],
                       accuracy(flor.trend.add_est, flor.test)[2,c(2, 3, 5)])
rownames(summary_table) <- c("ARIMA(2,1,2)", "ARI(6,1)", "Additive Exp Smoothing", "Additive HW (Trend)")
kable(summary_table)


# Summary table reports the RMSE, MAE, and MAPE of all 2 log-transformed models.
# California
summary_table <- rbind(accuracy(arima_log_mod1_est_cali, log(cali_test+1))[2,c(2, 3, 5)],
                       accuracy(arima_log_mod2_est_cali, log(cali_test+1))[2,c(2, 3, 5)])
rownames(summary_table) <- c("Log-transformed ARIMA(5,1,2)", "Log-transformed ARIMA(5,1,3)")
kable(summary_table)
# Florida
summary_table <- rbind(accuracy(arima_log_mod1_est_flo, log(flor.test+1))[2,c(2, 3, 5)],
                       accuracy(arima_log_mod2_est_flo, log(flor.test+1))[2,c(2, 3, 5)])
rownames(summary_table) <- c("Log-transformed ARIMA(5,1,2)", "Log-transformed ARIMA(5,1,5)")
kable(summary_table)


# Diagnostic test

# California
# The non-transformed ARI(6,1) versus log-transformed ARIMA(5,1,3) models.
# Refit 2 models to the whole data set.
# ARI(6,1) model
ari_mod_full_cali <- Arima(cali_ts, order=c(6, 1, 0))
ari_mod_full_cali
# ARIMA(5,1,3) model
arima_log_mod2_full_cali <- Arima(log(cali_ts+1), order=c(5, 1, 3))
arima_log_mod2_full_cali

# Check residuals
checkresiduals(ari_mod_full_cali) # ARI(6,1) model
checkresiduals(arima_log_mod2_full_cali) # ARIMA(5,1,3) model

# Ljung-Box test
Box.test(residuals(ari_mod_full_cali), lag=15, type="Ljung-Box", fitdf=6) # ARI(6,1) model
Box.test(residuals(arima_log_mod2_full_cali), lag=15, type="Ljung-Box", fitdf=8) # ARIMA(5,1,3) model

# Panel plots
par(mar=c(1,1,1,1))
tsdiag(ari_mod_full_cali, gof.lag = 15, omit.initial=F) # ARI(6,1) model
tsdiag(arima_log_mod2_full_cali, gof.lag = 15, omit.initial=F)

# Florida
# The non-transformed ARI(6,1) versus log-transformed ARIMA(5,1,5) models
# Refit 2 models to the whole data set.
# ARI(6,1) model
ari_mod_full_flo <- Arima(florida.ts, order=c(6, 1, 0))
ari_mod_full_flo
# ARIMA(5,1,5) model
arima_log_mod2_full_flo <- Arima(log(florida.ts+1), order=c(5, 1, 5))
arima_log_mod2_full_flo

# Check residuals
checkresiduals(ari_mod_full_flo) # ARI(6,1) model
checkresiduals(arima_log_mod2_full_flo) # ARIMA(5,1,5) model

# Ljung-Box test
Box.test(residuals(ari_mod_full_flo), lag=30, type="Ljung-Box", fitdf=6) # ARI(6,1) model
Box.test(residuals(arima_log_mod2_full_flo), lag=30, type="Ljung-Box", fitdf=10) # ARIMA(5,1,5) model

# Panel plots
tsdiag(ari_mod_full_flo, gof.lag = 30, omit.initial=F)
tsdiag(arima_log_mod2_full_flo, gof.lag = 30, omit.initial=F)


# Forecast the number of daily deaths from April 27, 2021 to April 30, 2021
daily.deaths.preds_cali <- forecast(arima_log_mod2_full_cali, h=4) # California
daily.deaths.preds_flo <- forecast(arima_log_mod2_full_flo, h=4) # Florida

# Plot
grid.arrange(
  autoplot(window(log(cali_ts+1), start=c(390))) +
    autolayer(daily.deaths.preds_cali) +
    labs(x = "Time",
         y = "Daily Deaths",
         caption="California") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  autoplot(window(log(florida.ts+1), start=c(390))) +
    autolayer(daily.deaths.preds_flo) +
    labs(x = "Time",
         y = "Daily Deaths",
         caption="Florida") +
    theme_bw() +
    theme(plot.caption = element_text(hjust= 0.5, size=14)),
  
  nrow = 1
)

# Rescale to get the actual prediction for daily deaths from April 27, 2021 to April 30, 2021
rescale_fc <- function(obj) {
  daily.deaths.preds.df <- as.data.frame(obj) %>%
    mutate(Date = seq(as.Date("2021-04-27"), as.Date("2021-04-30"), by = "day")) %>%
    relocate(Date)
  daily.deaths.preds.df$Retransform_Forecast <- exp(daily.deaths.preds.df[,2]) - 1
  daily.deaths.preds.df <- daily.deaths.preds.df %>%
    select(Date, Retransform_Forecast)
  
  return(daily.deaths.preds.df)
}

# California
daily.deaths.preds.df_cali <- rescale_fc(daily.deaths.preds_cali)
daily.deaths.df_cali <- daily.deaths.preds.df_cali %>%
  rename(Forecast_Deaths = Retransform_Forecast)
daily.deaths.df_cali$Actual_Deaths <- c(5, 65, 89, 105)
kable(daily.deaths.df_cali)

# Florida
daily.deaths.preds.df_flo <- rescale_fc(daily.deaths.preds_flo)
daily.deaths.df_flo <- daily.deaths.preds.df_flo %>%
  rename(Forecast_Deaths = Retransform_Forecast)
daily.deaths.df_flo$Actual_Deaths <- c(46, 72, 54, 77)
kable(daily.deaths.df_flo)
