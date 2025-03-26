# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

crime_data <- read_csv("montgomery_crime.csv")

# Convert date column to Date format
crime_data <- crime_data %>% 
  mutate(Start_Date = as.Date(Start_Date_Time, format = "%m/%d/%Y %I:%M:%S %p"))

# Consilidate city names
crime_data <- crime_data %>%
  mutate(City = str_trim(City),  # Remove leading/trailing whitespace
         City = str_to_lower(City),  # Convert to lowercase
         City = case_when(
           City %in% c("silver spring", "silverspring", "sliver spring", "slivver spring") ~ "Silver Spring",
           City %in% c("rockville", "rockvile", "rockvllle") ~ "Rockville",
           City %in% c("bethesda", "bethesdaa", "bethesada") ~ "Bethesda",
           City %in% c("gaithersburg", "gaithersburgg", "gathersburg") ~ "Gaithersburg",
           City %in% c("germantown", "germantwon", "germntown") ~ "Germantown",
           TRUE ~ str_to_title(City)  # Capitalize all other cities for display
         ))

# Filter data for the year 2024
crime_data_2024 <- crime_data %>% 
  filter(year(Start_Date) == 2024)

# Find the top 10 cities with the most crime in 2024
top_cities_2024 <- crime_data_2024 %>% 
  group_by(City) %>% 
  summarise(Total_Crimes = n()) %>% 
  arrange(desc(Total_Crimes)) %>% 
  head(10)

print(top_cities_2024)

# Find the most common crime types within those top cities in 2024
common_crimes_top_cities_2024 <- crime_data_2024 %>% 
  filter(City %in% top_cities_2024$City) %>% 
  group_by(City, `Crime Name2`) %>% 
  summarise(Incident_Count = n()) %>% 
  arrange(desc(Incident_Count))

print(common_crimes_top_cities_2024)

# Define specific crime categories
specific_crimes <- c("Robbery", "Simple Assault", "Aggravated Assault", "Motor Vehicle Theft", "Murder")

# Separate data for each crime type
robbery_data <- crime_data_2024 %>% filter(`Crime Name2` == "Robbery")
simple_assault_data <- crime_data_2024 %>% filter(`Crime Name2` == "Simple Assault")
aggravated_assault_data <- crime_data_2024 %>% filter(`Crime Name2` == "Aggravated Assault")
motor_vehicle_theft_data <- crime_data_2024 %>% filter(`Crime Name2` == "Motor Vehicle Theft")
murder_data <- crime_data_2024 %>% filter(`Crime Name2` == "Murder")

# Find top 10 cities for each crime type
robbery_top_10 <- robbery_data %>% group_by(City) %>% summarise(Incident_Count = n()) %>% arrange(desc(Incident_Count)) %>% head(10)
simple_assault_top_10 <- simple_assault_data %>% group_by(City) %>% summarise(Incident_Count = n()) %>% arrange(desc(Incident_Count)) %>% head(10)
aggravated_assault_top_10 <- aggravated_assault_data %>% group_by(City) %>% summarise(Incident_Count = n()) %>% arrange(desc(Incident_Count)) %>% head(10)
motor_vehicle_theft_top_10 <- motor_vehicle_theft_data %>% group_by(City) %>% summarise(Incident_Count = n()) %>% arrange(desc(Incident_Count)) %>% head(10)
murder_top_10 <- murder_data %>% group_by(City) %>% summarise(Incident_Count = n()) %>% arrange(desc(Incident_Count)) %>% head(10)

# Print each crime's top 10 cities
print(robbery_top_10)
print(simple_assault_top_10)
print(aggravated_assault_top_10)
print(motor_vehicle_theft_top_10)
print(murder_top_10)

generate_bar_chart <- function(data, crime_type, color) {
  ggplot(data, aes(x = reorder(City, -Incident_Count), y = Incident_Count)) +
    geom_bar(stat = "identity", fill = color) +
    theme_minimal() +
    labs(title = paste("Top 10 Cities for", crime_type, "in 2024"),
         x = "City",
         y = "Incident Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Generate bar charts
generate_bar_chart(robbery_top_10, "Robbery", "red")
generate_bar_chart(simple_assault_top_10, "Simple Assault", "blue")
generate_bar_chart(aggravated_assault_top_10, "Aggravated Assault", "green")
generate_bar_chart(motor_vehicle_theft_top_10, "Motor Vehicle Theft", "purple")
generate_bar_chart(murder_top_10, "Murder", "black")
