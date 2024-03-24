################################################################
### Browne Academy Science Fair Project 2024 ###################
### Investigator: Ramsay Quillin ###############################
### Topic: Experiments in weather forecasting & data science ###
################################################################


# Load packages -----------------------------------------------------------

library(tidyverse)
library(readxl)

# Analysis of 15-day forecasts from Visual Crossing -----------------------

### Forecast data

# Read data for 15-day forecasts from Excel
data_forecast <- read_excel("Weather_history_master.xlsx") 

# Convert the two date columns into date format
data_forecast$datetime <- as.Date(data_forecast$datetime) 
data_forecast$Basisdate <- as.Date(data_forecast$Basisdate)

# Extract the dates and min and max temperatures and save in a new dataframe
data_temps <- data_forecast[, c("Basisdate", "datetime", "tempmax", "tempmin")]

# Rename columns to reflect that these are the forecasts
colnames(data_temps)[colnames(data_temps) == "tempmax"] <- "tempmax_forecast"
colnames(data_temps)[colnames(data_temps) == "tempmin"] <- "tempmin_forecast"

### Historical data

# Read data for actual weather from Excel
data_history <- read_excel("Weather_history.xlsx")

# Convert the date column into date format
data_history$datetime <- as.Date(data_history$datetime)

### Merge the two dataframes

# Merge the weather history and forecast datasets & reorder columns
weather_merged <- merge(data_temps, data_history, by = "datetime", all = F)
weather_merged <- weather_merged[, c("Basisdate", "datetime",
                                     "tempmax_forecast", "tempmax",
                                     "tempmin_forecast", "tempmin")]

# Create a column with the spread of the number of days between the forecast and history columns
weather_merged$forecast_length <- as.numeric(weather_merged$datetime - weather_merged$Basisdate)

# We seem to have picked up 16-day differences so removing rows so that 15-day is the maximum
weather_merged <- weather_merged %>% filter(forecast_length != 16)

# Create columns showing the difference between the history & forecast for min and max temperatures
weather_merged$max_accuracy <- as.numeric(weather_merged$tempmax - weather_merged$tempmax_forecast)
weather_merged$min_accuracy <- as.numeric(weather_merged$tempmin - weather_merged$tempmin_forecast)

# Conduct analysis of forecast accuracy by forecast vintage ---------------
summary_stats_max <- weather_merged %>% 
  group_by(forecast_length) %>% 
  summarise(mean_value = mean(max_accuracy),
            median_value = median(max_accuracy),
            min_value = min(max_accuracy),
            max_value = max(max_accuracy))

summary_stats_min <- weather_merged %>% 
  group_by(forecast_length) %>% 
  summarise(mean_value = mean(min_accuracy),
            median_value = median(min_accuracy),
            min_value = min(min_accuracy),
            min_value = min(min_accuracy))









