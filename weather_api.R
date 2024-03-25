################################################################
### Browne Academy Science Fair Project 2024 ###################
### Investigator: Ramsay Quillin ###############################
### Topic: Experiments in weather forecasting & data science ###
## Analysis for Alexandria, Virginia during 2021-2024 ##########
################################################################
################################################################


# Install libraries -------------------------------------------------------
library(tidyverse)
library(lubridate)
library(httr)

# Download forcecasted weather forecast data through the Visual Crossing API --------

# Define the base URL
base_url <- "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/22314/"

# Define the start and end dates
start_date <- ymd("2021-01-01")  # Using ymd() from lubridate to ensure consistent date objects
end_date <- ymd("2024-03-01")     # Using ymd() from lubridate to ensure consistent date objects

# Initialize an empty list to store the generated URLs
url_list <- list()

# Loop to generate URLs
for (i in 0:(as.numeric(end_date - start_date))) {
  # Calculate the start and end dates for the current iteration by moving both dates forward
  target_start_date <- start_date + days(i)
  target_end_date <- target_start_date + days(16)
  
  # Calculate the forecast basis date for the current iteration by moving it forward
  forecast_basis_date <- start_date - days(1) + days(i)  # Ensure consistent date object types
  
  # Convert dates to character strings
  target_start_date_str <- as.character(target_start_date)
  target_end_date_str <- as.character(target_end_date)
  forecast_basis_date_str <- format(forecast_basis_date, "%Y-%m-%d")  # Format the date as YYYY-MM-DD
  
  # Construct the URL for the current date range
  url <- paste0(base_url, target_start_date_str, "/", target_end_date_str, "?unitGroup=us&key=7ZNU9ZCTWVVPRND4YBZAA3QNV&include=days&forecastBasisDate=", forecast_basis_date_str, "&contentType=csv")
  
  # Store the generated URL in the list
  url_list[[i + 1]] <- url
}

# Initialize an empty list to store dataframes
dataframes <- list()

# Read CSV data from each URL and store it in separate dataframes
for (i in 1:length(url_list)) {
  # Read CSV data from the current URL
  data <- read.csv(url_list[[i]])
  
  # Add BasisDate column
  data$BasisDate <- format(start_date - 1 + days(i), "%Y-%m-%d")  # Calculate BasisDate dynamically
  
  # Store the dataframe in the list
  dataframes[[i]] <- data
}

# Merge all dataframes into one dataframe
merged_data <- do.call(rbind, dataframes)


# Cleaning of forecast data -----------------------------------------------

# Convert the two date columns into date format
merged_data$datetime <- as.Date(merged_data$datetime) 
merged_data$BasisDate <- as.Date(merged_data$BasisDate)

# Rename columns to reflect that these are the forecasts
colnames(merged_data)[colnames(merged_data) == "tempmax"] <- "tempmax_forecast"
colnames(merged_data)[colnames(merged_data) == "tempmin"] <- "tempmin_forecast"


# Download the historical weather data from Virtual Crossing --------------

# Define the API URL
api_url <- ENTER API KEY HERE

# Send GET request to the API URL
response <- GET(api_url)

# Check if the request was successful (status code 200)
if (status_code(response) == 200) {
# Read CSV data from the response and save it in a dataframe
  historical_dataframe <- read.csv(text = content(response, "text"))
  
  # Print the dataframe or further process it as needed
  print(historical_dataframe)
} else {
  # If request was not successful, print the status code
  print(paste("Error:", status_code(response)))
}

# Convert the date column into date format
historical_dataframe$datetime <- as.Date(historical_dataframe$datetime)


# Merge historical and forecast dataframes --------------------------------

# Start by renaming the historical min and max to make clear that it is history
colnames(historical_dataframe)[colnames(historical_dataframe) == "tempmax"] <- "tempmax_history"
colnames(historical_dataframe)[colnames(historical_dataframe) == "tempmin"] <- "tempmin_history"

# Next let's select some columns for merging from the history by subsetting into a new dataframe
history_subset <- historical_dataframe[, c("datetime", "tempmax_history", "tempmin_history")]

# Merge the two dataframes
weather_merged <- merge(merged_data, history_subset, by = "datetime", all = F)


# Analyze and summarize forecast accuracy ---------------------------------

# Create a column with the spread of the number of days between the forecast and history columns
weather_merged$forecast_length <- as.numeric(weather_merged$datetime - weather_merged$BasisDate)

# Create columns showing the difference between the history & forecast for min and max temperatures
weather_merged$max_accuracy <- as.numeric(weather_merged$tempmax_history - weather_merged$tempmax_forecast)
weather_merged$min_accuracy <- as.numeric(weather_merged$tempmin_history - weather_merged$tempmin_forecast)

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
            max_value = max(max_accuracy))

print(summary_stats_max)
print(summary_stats_min)

# Analyze forecast accuracy by season -------------------------------------

# Extract month from the date column
weather_merged$month <- month(weather_merged$date)

# Define a function to categorize months into seasons
get_season <- function(month) {
  if (month %in% 3:5) {
    return("spring")
  } else if (month %in% 6:8) {
    return("summer")
  } else if (month %in% 9:11) {
    return("fall")
  } else {
    return("winter")
  }
}

# Apply the function to the month column to create a new column for seasons
weather_merged$season <- sapply(weather_merged$month, get_season)

# Calculate the averages for each season for max temperatures
seasonal_averages_max <- weather_merged %>%
  group_by(season) %>%
  summarise(average_value = mean(max_accuracy, na.rm = TRUE))

# Calculate the averages for each season for min temperatures
seasonal_averages_min <- weather_merged %>%
  group_by(season) %>%
  summarise(average_value = mean(min_accuracy, na.rm = TRUE))

# Print the seasonal averages
print(seasonal_averages_max)
print(seasonal_averages_min)


# Create bar charts -------------------------------------------------------

# Rename columns
summary_stats_max <- summary_stats_max[-nrow(summary_stats_max), ]
summary_stats_min <- summary_stats_min[-nrow(summary_stats_min), ]
#summary_stats_max <- summary_stats_max[-1, ]

colnames(summary_stats_max) <- c("Forecast Horizon (days)",
                                 "Reality Minus Forecast (average)", 
                                 "Reality Minus Forecast (median)",
                                 "Smallest Forecast Miss",
                                 "Largest Forecast Miss"
                                 )


colnames(summary_stats_min) <- c("Forecast Horizon (days)",
                                 "Reality Minus Forecast (average)", 
                                 "Reality Minus Forecast (median)",
                                 "Smallest Forecast Miss",
                                 "Largest Forecast Miss"
)

# Create a bar plot for average values - max temps
alex_plot_max <- ggplot(summary_stats_max, aes(x = factor(`Forecast Horizon (days)`), y = `Reality Minus Forecast (average)`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(`Reality Minus Forecast (average)`, 2)), 
            position = position_stack(vjust = 0.5), 
            color = "red", size = 3) +  # Add text with red color
  labs(x = "Forecast Horizon (days)", y = "Reality Minus Forecast (average)") +
  ggtitle("Alexandria, VA: High Temperature Forecast Accuracy") +
  labs(subtitle = "Bars show real maximum temperature minus forecasted temp over different forecast horizons") +
  labs(caption = "Source: Data from Visual Crossing. Analysis by Ramsay Quillin.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.0)) 

ggsave("alex.plot.max.png", alex_plot_max, dpi = 500)

# Create a bar plot for average values - min temps
alex_plot_min <- ggplot(summary_stats_min, aes(x = factor(`Forecast Horizon (days)`), y = `Reality Minus Forecast (average)`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(`Reality Minus Forecast (average)`, 2)), 
            position = position_stack(vjust = 0.5), 
            color = "red", size = 3) +  # Add text with red color
  labs(x = "Forecast Horizon (days)", y = "Reality Minus Forecast (average)") +
  ggtitle("Alexandria, VA: Low Temperature Forecast Accuracy") +
  labs(subtitle = "Bars show real minimum temperature minus forecasted temp over different forecast horizons") +
  labs(caption = "Source: Data from Visual Crossing. Analysis by Ramsay Quillin.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.0)) 

ggsave("alex.plot.min.png", alex_plot_min, dpi = 500)

### Forecast errors by season

# Max temperatures
alex_plot_seaons_max <- ggplot(seasonal_averages_max, aes(x = season, y = average_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(average_value, 2)), 
            position = position_stack(vjust = 0.5), 
            color = "red", size = 3) +
  labs(x = "", y = "Reality Minus Forecast (average)") +
  ggtitle("Alexandria, VA: Forecast Accuracy by Seasons") +
  labs(subtitle = "Bars show average of max temperature minus forecasted temp") +
  labs(caption = "Source: Data from Visual Crossing. Analysis by Ramsay Quillin.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.0)) 

ggsave("alex_plot_seasons.max.png", alex_plot_max, dpi = 500)

# Min temperatures
alex_plot_seasons_min <- ggplot(seasonal_averages_min, aes(x = season, y = average_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(average_value, 2)), 
            position = position_stack(vjust = 0.5), 
            color = "red", size = 3) +
  labs(x = "", y = "Reality Minus Forecast (average)") +
  ggtitle("Alexandria, VA: Forecast Accuracy by Seasons") +
  labs(subtitle = "Bars show average of min temperature minus forecasted temp") +
  labs(caption = "Source: Data from Visual Crossing. Analysis by Ramsay Quillin.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.0)) 

ggsave("alex_plot_seasons_min.png", alex_plot_seasons_min, dpi = 500)

# Descriptive statistics --------------------------------------------------

# Forecast errors by season
print(seasonal_averages_max)
print(seasonal_averages_min)

# Variance of temperatures
sd(weather_merged$tempmax_history)
sd(weather_merged$tempmin_history)
