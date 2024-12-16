rm(list=ls())
setwd("E:/Desktop/BU/2024 Fall/MA615/Final")
library(readr)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(terra)
library(lubridate)

data <- read.csv("A&B_list.csv", fileEncoding = "latin1", stringsAsFactors = FALSE)


# Rename columns
colnames(data) <- c("Country", "Series", "Year", "Value")

# Change data types
data$Country <- as.character(data$Country)
data$Series <- as.character(data$Series)
data$Year <- as.numeric(data$Year)
data$Value <- as.numeric(data$Value)

# Filter for Aruba and Barbados
AB <- data %>%
  filter(Country %in% c("Aruba", "Barbados"))

aruba_gdp <- AB %>%
  filter(Country == "Aruba" & grepl("GDP", Series, ignore.case = TRUE))
ggplot(aruba_gdp, aes(x = Year, y = Value)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "GDP Yearly for Aruba",
       x = "Year",
       y = "GDP (in billions or units)") +
  theme_minimal()

aruba_co2 <- AB %>%
  filter(Country == "Aruba" & grepl("Transport \\(Energy\\)", Series, ignore.case = TRUE))

# Plot CO2 emissions from Transport
ggplot(aruba_co2, aes(x = Year, y = Value)) +
  geom_line(color = "green") +
  geom_point(color = "orange") +
  labs(title = "Carbon Dioxide (CO2) Emissions from Transport for Aruba",
       x = "Year",
       y = "CO2 Emissions (Mt CO2e)") +
  theme_minimal()

aruba_population_total <- AB %>%
  filter(Country == "Aruba" & grepl("Population, total", Series, ignore.case = TRUE))

# Create a Line Plot for Total Population
ggplot(aruba_population_total, aes(x = Year, y = Value)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Yearly Trend of Total Population in Aruba",
       x = "Year",
       y = "Total Population") +
  theme_minimal()



# Filter data for Aruba and Population Age Groups
aruba_age_groups <- AB %>%
  filter(Country == "Aruba" & grepl("Population ages", Series, ignore.case = TRUE))

# Create a Stacked Area Plot
ggplot(aruba_age_groups, aes(x = Year, y = Value, fill = Series)) +
  geom_area(alpha = 0.7) +
  labs(title = "Yearly Change in Population Proportions by Age Group in Aruba",
       x = "Year",
       y = "Proportion of Population (%)",
       fill = "Age Group") +
  theme_minimal()

# setwd("E:/Desktop/BU/2024 Fall/MA615/Final/datatif")
# tif_files <- "datatif"
# 
# # Get a list of all the TIF files in the directory
# tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)
# tif_names <- list.files(pattern = "\\.tif$", full.names = FALSE)
# 
# # Load all TIF files into a named list of rasters
# raster_list <- setNames(lapply(tif_files, rast), tif_names)
# 
# ############# Counting table ###########
# 
# # Count the number of vegetated pixels (value = 1) for each raster
# count_values <- sapply(raster_list, function(r) sum(values(r) == 1, na.rm = TRUE))
# 
# time_periods <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", tif_names))
# 
# # Create a data frame for ggplot
# df3 <- data.frame(
#   Vegetation_Count = count_values,
#   Time_Period = time_periods
# )
# 
# 
# 
# ################  Viz ################
# 
# # Find the maximum and minimum vegetation counts
# max_point <- df3[which.max(df3$Vegetation_Count), ]
# min_point <- df3[which.min(df3$Vegetation_Count), ]
# 
# # Create a combined plot with ggplot, highlighting max/min and removing mean line
# ggplot(df3, aes(x = Time_Period)) +
#   geom_line(aes(y = Vegetation_Count, color = "Vegetation Count"), size = 1) +
#   geom_point(aes(y = Vegetation_Count, color = "Vegetation Count")) +
#   
#   # Highlight the max and min points
#   geom_point(data = max_point, aes(x = Time_Period, y = Vegetation_Count), color = "green", size = 4) +
#   geom_point(data = min_point, aes(x = Time_Period, y = Vegetation_Count), color = "brown", size = 4) +
#   
#   # Adding labels to the max and min points
#   geom_text(aes(x = max_point$Time_Period, y = max_point$Vegetation_Count, label = "Max"), vjust = -1.5) +
#   geom_text(aes(x = min_point$Time_Period, y = min_point$Vegetation_Count, label = "Min"), vjust = 1.5) +
#   
#   # Formatting the x-axis with months
#   scale_x_date(date_labels = "%b %Y", breaks = "1 months") +
#   
#   scale_y_continuous(
#     name = "Count of Vegetated Pixels",
#   ) +
#   labs(title = "Vegetation Count Over Time", x = "Time Period") +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Enlarge x-axis text
#     axis.text.y = element_text(size = 25),  # Enlarge y-axis text
#     axis.title.x = element_text(size = 30),  # Enlarge x-axis title
#     axis.title.y = element_text(size = 30),  # Enlarge y-axis title
#     plot.title = element_text(size = 35, hjust = 0.5),  # Enlarge and center title
#     legend.text = element_text(size = 20),  # Enlarge legend text
#     legend.title = element_text(size = 21)  # Enlarge legend title
#   ) +
#   scale_color_manual(name = "Lable",
#                      values = c("Vegetation Count" = "darkgreen")
#   )
