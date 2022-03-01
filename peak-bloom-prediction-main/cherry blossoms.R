#packages
install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(ggplot2)

#appending data sets
weather <- read.csv("data/weather.csv")
colnames(weather)[1] <- "location"
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv")) %>%
  left_join(weather)
cherry <- subset(cherry, year >= 1950)

#plotting time series of bloom DOY since 1950
cherry %>% 
  ggplot(aes(x = year, y = bloom_doy)) +
  geom_point() +
  geom_step(linetype = 'dotted', color = 'gray50') +
  scale_x_continuous(breaks = seq(1950, 2020, by = 20)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")

#creating new data sets by location
dc <- subset(cherry, location == 'washingtondc')
kyoto <- subset(cherry, location == 'kyoto')
liestal <- subset(cherry, location == 'liestal')

#fitting linear model
ls_fit <- lm(bloom_doy ~ location * (year + PRCP + TMIN + TMAX), data = cherry)

#fitting linear models to expand weather data through 2031
prcp_fit <- lm(PRCP ~ location*year, data = cherry)
tmin_fit <- lm(TMIN ~ location*year, data = cherry)
tmax_fit <- lm(TMAX ~ location*year, data = cherry)

#creating prediction data set with years 2022 to 2031 and predicted weather data
predictions <- expand_grid(location = unique(cherry$location), year = 2022:2031) %>% 
  bind_cols(PRCP = predict(prcp_fit, newdata = .)) %>%
  bind_cols(TMIN = predict(tmin_fit, newdata = .)) %>%
  bind_cols(TMAX = predict(tmax_fit, newdata = .)) %>% 
  #predicting bloom day from new weather data
  bind_cols(predicted_doy = round(predict(ls_fit, newdata = .)))
#We now have a data frame of predicted bloom dates for 2022 to 2031 based on predicted weather
#variables for the same years.  For example, in Liestal this year, the bloom DOY has been
#predicted to be 90 days after January 1st, or April 1st.

#function for converting day of year to date
doy_to_date <- function (year, doy) {
  strptime(paste(year, doy, sep = '-'), '%Y-%j') %>% # create date object
    strftime('%Y-%m-%d') # translate back to date string in ISO 8601 format
}

#fitting model to estimate Vancouver bloom dates from 1950-2021
ls_fit_van_hist <- lm(bloom_doy ~ year, data = cherry)
vancouver <- expand_grid(location = 'vancouver', year = 1950:2021) %>%
  bind_cols(bloom_doy = round(predict(ls_fit_van_hist, newdata = .))) 
vancouver <- vancouver %>%
  bind_cols(bloom_date = doy_to_date(vancouver$year, vancouver$bloom_doy)) %>%
  #appending weather data
  left_join(read.csv("vancouverweather.csv"))

#fitting linear models to expand weather data through 2031
prcp_fit_van <- lm(PRCP ~ year, data = vancouver)
tmin_fit_van <- lm(TMIN ~ year, data = vancouver)
tmax_fit_van <- lm(TMAX ~ year, data = vancouver)

#fitting linear models to predict bloom date from weather
ls_fit_van <- lm(bloom_doy ~ (year + PRCP + TMIN + TMAX), data = vancouver)

#creating prediction data set with years 2022 to 2031 and predicted weather data for Vancouver
predictions_van <- expand_grid(location = 'vancouver', year = 2022:2031) %>%
  bind_cols(PRCP = predict(prcp_fit_van, newdata = .)) %>%
  bind_cols(TMIN = predict(tmin_fit_van, newdata = .)) %>%
  bind_cols(TMAX = predict(tmin_fit_van, newdata = .)) %>%
  #predicting bloom day from new weather data
  bind_cols(predicted_doy = round(predict(ls_fit_van, newdata = .)))

#we now have a data frame of predicted bloom dates in Vancouver from 2022 to 2031 based on predicted weather
#variables for the same years.

##plotting predicted bloom_doy alongside actual bloom dates for Liestal, Kyoto, and Washington DC
cherry %>% 
  full_join(predictions, by = c('year', 'location')) %>%
  ggplot(aes(x = year, y = predicted_doy)) +
  geom_line(aes(color = year > 2021), size = 1) +
  geom_point(aes(y = bloom_doy)) +
  scale_color_manual(values = c('FALSE' = 'gray50', 'TRUE' = 'blue'),
                     guide = 'none') +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")

#adding Vancouver predictions to predictions data frame
predictions <- predictions %>%
  full_join(predictions_van)

write.csv(predictions, file = "predictions.csv",
          row.names = FALSE)