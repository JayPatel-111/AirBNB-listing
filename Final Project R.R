# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# Set the file path to your dataset
file_path <- "C:/Users/jaypa/Downloads/listings.csv"



# Importing the dataset
airbnb_data <- read_csv(file_path)

# Basic Data Exploration
summary(airbnb_data)
str(airbnb_data)
head(airbnb_data, 10)

# Data Cleaning
# Removing rows with NA values in critical columns and filtering price outliers
airbnb_clean <- airbnb_data %>%
  drop_na(name, host_id, neighbourhood_group, room_type, price) %>%
  dplyr::filter(price < 1000)

# Data Transformation
# Convert last_review to date format and extract year
airbnb_clean <- airbnb_clean %>%
  mutate(last_review = as.Date(last_review, format = "%Y-%m-%d"),
         review_year = year(last_review))

# Advanced Data Exploration
# Descriptive statistics for numerical and categorical data
airbnb_clean %>%
  summarise(across(where(is.numeric), ~mean(., na.rm = TRUE)))
airbnb_clean %>%
  count(neighbourhood_group)

# Visualization 1: Room types in different neighbourhoods
ggplot(airbnb_clean, aes(x = neighbourhood_group, fill = room_type)) +
  geom_bar() +
  labs(title = "Distribution of Room Types in NYC Neighbourhoods",
       x = "Neighbourhood Group",
       y = "Count")

# Visualization 2: Average Price in each neighbourhood
avg_price_neighbourhood <- airbnb_clean %>%
  group_by(neighbourhood_group) %>%
  summarise(average_price = mean(price))
ggplot(avg_price_neighbourhood, aes(x = neighbourhood_group, y = average_price)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Price in NYC Neighbourhoods",
       x = "Neighbourhood Group",
       y = "Average Price (USD)")

# Visualization 3: Reviews per Month by Room Type
reviews_room_type <- airbnb_clean %>%
  group_by(room_type) %>%
  summarise(average_reviews = mean(reviews_per_month, na.rm = TRUE))
ggplot(reviews_room_type, aes(x = room_type, y = average_reviews)) +
  geom_col(fill = "darkcyan") +
  labs(title = "Average Reviews per Month by Room Type",
       x = "Room Type",
       y = "Average Reviews per Month")

# Visualization 4: Price Distribution by Neighbourhood and Room Type
ggplot(airbnb_clean, aes(x = price, fill = neighbourhood_group)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~room_type) +
  labs(title = "Price Distribution by Neighbourhood and Room Type",
       x = "Price",
       y = "Count")

# Visualization 5: Availability of Airbnb Listings over Year
ggplot(airbnb_clean, aes(x = review_year, fill = neighbourhood_group)) +
  geom_bar() +
  labs(title = "Airbnb Listings Availability Over Years",
       x = "Year of Last Review",
       y = "Number of Listings")

