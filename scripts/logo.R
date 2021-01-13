# generate the plot for the book

# install.packages("hexSticker")
#install.packages("svglite)

library(forecast)
library(lubridate)
library(tidyverse)
library(hexSticker)

data <- read_csv('../data/visitors.csv')

ts_data <- ts(data$Visitors, frequency=52, start=2014)

# over fit a model real quick
model <- ts_data %>% auto.arima()
model_fit <- model %>% fitted()
upper_conf <- model_fit + 2.576*sqrt(model$sigma2)
lower_conf <- model_fit - 2.576*sqrt(model$sigma2)

# plot fitted model on data
p <- ggplot() + 
  geom_line(mapping = aes(x = time(model_fit), y = model_fit), 
            color='dodgerblue', size=0.7) + 
  geom_ribbon(aes(x = time(ts_data), ymin = lower_conf, ymax = upper_conf),
              alpha = 0.6,
              fill = "dodgerblue"
              ) +
  geom_point(mapping = aes(x = time(ts_data), y = ts_data), size = 0.2) +
  xlab("Time Index") + 
  ylab("Pickups") + 
  ylim(-50, 110)

# make hex sticker
p %>% sticker(
  package="",
  h_color="gray",
  h_size=1,
  url="forecastr", 
  u_color = "black",
  u_size=5,
  u_x = 1.05,
  u_y = 0.15,
  u_angle=30,
  s_x=0.77, 
  s_y = .85, 
  s_width=3.6, 
  s_height=3.6,
  white_around_sticker = TRUE,
  filename="../media/forecastr_sticker.png"
  )

# make hex sticker
p %>% sticker(
  package="",
  h_color="gray",
  h_size=1,
  url="forecastr", 
  u_color = "black",
  u_size=5,
  u_x = 1.05,
  u_y = 0.15,
  u_angle=30,
  s_x=0.77, 
  s_y = .85, 
  s_width=3.6, 
  s_height=3.6,
  white_around_sticker = TRUE,
  filename="../media/forecastr_sticker.svg"
)

# make hex sticker
p %>% sticker(
  package="",
  h_color="gray",
  h_size=1,
  url="", 
  u_color = "black",
  u_size=5,
  u_x = 1.05,
  u_y = 0.15,
  u_angle=30,
  s_x=0.77, 
  s_y = .85, 
  s_width=3.6, 
  s_height=3.6,
  white_around_sticker = TRUE,
  filename="../media/forecastr_background.png"
)

# make hex sticker
p %>% sticker(
  package="",
  h_color="gray",
  h_size=1,
  url="", 
  u_color = "black",
  u_size=5,
  u_x = 1.05,
  u_y = 0.15,
  u_angle=30,
  s_x=0.77, 
  s_y = .85, 
  s_width=3.6, 
  s_height=3.6,
  white_around_sticker = TRUE,
  filename="../media/forecastr_background.svg"
)

