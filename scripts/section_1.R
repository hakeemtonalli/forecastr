# Script for reading in data, building a model, and generating a forecast 
#
# uncomment these lines if you need to install the packages
# install.packages("forecast")
# install.packages("tidyverse")
# install.packages("zoo")
# install.packages("lubridate")


library(forecast)
library(tidyverse)
library(zoo)
library(lubridate)

# load a csv from the data/ folder and store it as a dataframe
# the path relative to the current working directory
my_data <- read_csv('data/visitors.csv')

# view the first few lines of a dataframe
head(my_data)

# convert dataframe to a weekly time series, starting 1/2014
my_ts <- ts(my_data$Visitors, start=2014, freq=52)


# view the time series as a plot
autoplot(my_ts) +
  ggtitle("My Time Series Plot")

# fit the model and store in a variable called my_model
my_model <- tslm(my_ts ~ trend) 

# provide the fitted values of the model
model_fit <- my_model %>% fitted()

autoplot(my_ts) +
  # autolayer() is used to add another time series to the plot
  autolayer(model_fit, color="red") +
  ggtitle("Linear Regession Fit") +
  xlab("Time") + 
  ylab("Unique Visitors")

# predict the value of the new data 
next_years_visitors <- my_model %>% forecast(h=52) 

autoplot(my_ts) +
  # the "series" argument labels the data in the legend
  autolayer(next_years_visitors, series="Forecast") +
  ggtitle("Linear Regession Forecast") +
  xlab("Time") + 
  ylab("Unique Visitors")

# fit a model with trend and season
seasonal_model <- tslm(my_ts ~ trend + season)

# forecast the next 52 weeks with the model
seasonal_forecast <- seasonal_model %>% forecast(h=52)

autoplot(my_ts) +
  autolayer(seasonal_forecast, series="Forecast") +
  ggtitle("Linear Regession with Seasonal Forecast") +
  xlab("Time") + 
  ylab("Unique Visitors")

write.csv(seasonal_forecast, 'my_predictions.csv')