
############### Loading ################
library(terra)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(tidyr)

# Set the working directory where the TIF files are located
setwd("E:/Desktop/BU/2024 Fall/MA675/Meta/Data")

# Get a list of all the TIF files in the directory
tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)
tif_names <- list.files(pattern = "\\.tif$", full.names = FALSE)

# Load all TIF files into a named list of rasters
raster_list <- setNames(lapply(tif_files, rast), tif_names)

############# Counting table ###########

# Count the number of vegetated pixels (value = 1) for each raster
count_values <- sapply(raster_list, function(r) sum(values(r) == 1, na.rm = TRUE))
View(count_values)

# Calculate the mean for each raster
mean_values <- sapply(raster_list, function(r) mean(values(r), na.rm = TRUE))

# Extract time periods from the file names
time_periods <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", tif_names))

# Create a data frame for ggplot
df3 <- data.frame(
  Vegetation_Count = count_values,
  Mean_Vegetation = mean_values,
  Time_Period = time_periods
)



################  Viz ################

# Find the maximum and minimum vegetation counts
max_point <- df3[which.max(df3$Vegetation_Count), ]
min_point <- df3[which.min(df3$Vegetation_Count), ]

# Create a combined plot with ggplot, highlighting max/min and removing mean line
ggplot(df3, aes(x = Time_Period)) +
  geom_line(aes(y = Vegetation_Count, color = "Vegetation Count"), size = 1) +
  geom_point(aes(y = Vegetation_Count, color = "Vegetation Count")) +
  
  # Highlight the max and min points
  geom_point(data = max_point, aes(x = Time_Period, y = Vegetation_Count), color = "green", size = 4) +
  geom_point(data = min_point, aes(x = Time_Period, y = Vegetation_Count), color = "brown", size = 4) +
  
  # Adding labels to the max and min points
  geom_text(aes(x = max_point$Time_Period, y = max_point$Vegetation_Count, label = "Max"), vjust = -1.5) +
  geom_text(aes(x = min_point$Time_Period, y = min_point$Vegetation_Count, label = "Min"), vjust = 1.5) +
  
  # Formatting the x-axis with months
  scale_x_date(date_labels = "%b %Y", breaks = "1 months") +
  
  scale_y_continuous(
    name = "Count of Vegetated Pixels",
  ) +
  labs(title = "Vegetation Count Over Time", x = "Time Period") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Enlarge x-axis text
    axis.text.y = element_text(size = 25),  # Enlarge y-axis text
    axis.title.x = element_text(size = 30),  # Enlarge x-axis title
    axis.title.y = element_text(size = 30),  # Enlarge y-axis title
    plot.title = element_text(size = 35, hjust = 0.5),  # Enlarge and center title
    legend.text = element_text(size = 20),  # Enlarge legend text
    legend.title = element_text(size = 21)  # Enlarge legend title
  ) +
  scale_color_manual(name = "Lable",
    values = c("Vegetation Count" = "darkgreen")
  )


#######################################

# ne_shp <- vect("E:/Desktop/BU/2024 Fall/MA675/Meta/SHP/gadm41_ABW_shp/gadm41_ABW_0.shp")
# plot(ne_shp, main = "Natural Earth Shapefile - Country Boundaries")
# 
# 
# par(mfrow=c(2, 3))
# # 22: 04-05
# # Load the two rasters (replace with your actual file paths)
# raster1 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2022-04-01_2022-05-01.tif")
# raster2 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2022-05-01_2022-06-01.tif")
# 
# # Perform change detection by subtracting raster1 from raster2
# change_detection1 <- raster2 - raster1
# 
# # Plot the original rasters and the change detection results
# #par(mfrow=c(1, 3))  # Set up plot window for 3 plots side-by-side
# 
# # Plot the two original rasters
# plot(raster1, main = "Raster 1 (Original)")
# plot(raster2, main = "Raster 2 (Original)")
# 
# # Plot the change detection (areas of change)
# plot(change_detection1, main = "Change Detection (Raster 2 - Raster 1)", col = terrain.colors(10))
# 
# 
# # 23: 04-05
# raster3 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2023-04-01_2023-05-01.tif")
# raster4 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2023-05-01_2023-06-01.tif")
# 
# # Perform change detection by subtracting raster1 from raster2
# change_detection2 <- raster4 - raster3
# 
# # Plot the original rasters and the change detection results
# #par(mfrow=c(1, 6))  # Set up plot window for 3 plots side-by-side
# 
# # Plot the two original rasters
# plot(raster3, main = "Raster 3 (Original)")
# plot(raster4, main = "Raster 4 (Original)")
# 
# # Plot the change detection (areas of change)
# plot(change_detection2, main = "Change Detection (Raster 4 - Raster 3)", col = terrain.colors(10))

###########
# Check if the file GEO info match

# Load the shapefile and rasters
shapefile <- vect("E:/path_to_shapefile/gadm41_ABW_0.shp")
raster1 <- rast("E:/path_to_raster/raster1.tif")

# Check the CRS of both
shapefile_crs <- crs(ne_shp)
raster_crs <- crs(raster1)

# Print the CRS information
cat("Shapefile CRS:", shapefile_crs, "\n")
cat("Raster CRS:", raster_crs, "\n")

##################

par(mfrow=c(2, 3))
ne_shp <- vect("E:/Desktop/BU/2024 Fall/MA675/Meta/SHP/gadm41_ABW_shp/gadm41_ABW_0.shp")
# 22: April - May 2022
# Load rasters
raster1 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2024-02-01_2024-03-01.tif")
raster2 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2024-08-01_2024-09-01.tif")

# Load rasters
raster3 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2024-08-01_2024-09-01.tif")
raster4 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2024-10-01_2024-11-01.tif")

# Perform change detection
change_detection1 <- raster2 - raster1
change_detection2 <- raster4 - raster3

#plot(change_detection1, col = change_colors, main = "Change Detection 2022")
main_size <- 2  # For the main title
label_size <- 1.5  # For axis labels
axis_size <- 1.2  # For axis text

# Plot Feb-2024 Raster
plot(raster1, main = "Feb-2024", cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 1)  # Overlay shapefile

# Plot Aug-2024 Raster
plot(raster2, main = "Aug-2024", cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 1)  # Overlay shapefile

# Plot Feb-Aug 2024
plot(change_detection1, main = "Feb-Aug 2024", col = terrain.colors(10), cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 1)  # Overlay shapefile

# Plot Aug-2024 Raster
plot(raster3, main = "Aug-2024", cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 0.5)  # Overlay shapefile

# Plot Oct 2024 Raster
plot(raster4, main = "Oct 2024", cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 0.5)  # Overlay shapefile

# Plot Change Detection 2024
plot(change_detection2, main = "Aug-Oct 2024", col = terrain.colors(10), cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 0.5)  # Overlay shapefile

################################    STAT    #############

vg_vegetation_count <- mean(df3$Vegetation_Count, na.rm = TRUE)
vg_vegetation_count

# Assuming change_detection1 is your change detection raster
# Calculate the number of pixels with positive change (1) and negative change (-1)
positive_change_pixels <- sum(values(change_detection1) == 1, na.rm = TRUE)
negative_change_pixels <- sum(values(change_detection1) == -1, na.rm = TRUE)

cat("Positive Change (in number of pixels):", positive_change_pixels, "\n")
cat("Negative Change (in number of pixels):", negative_change_pixels, "\n")

pixel_area_m2 <- 10 * 10  # Each pixel represents 100 square meters

# Calculate the real-world area of positive and negative changes
positive_change_area_m2 <- positive_change_pixels * pixel_area_m2
negative_change_area_m2 <- negative_change_pixels * pixel_area_m2

# Convert to hectares (1 hectare = 10,000 square meters)
positive_change_area_ha <- positive_change_area_m2 / 10000
negative_change_area_ha <- negative_change_area_m2 / 10000

cat("Positive Change Area (in hectares):", positive_change_area_ha, "\n")
cat("Negative Change Area (in hectares):", negative_change_area_ha, "\n")

# # Convert the Vegetation_Count to a time series object with a monthly frequency
# ts_data <- ts(df3$Vegetation_Count, start = c(2019, 5), frequency = 12)
# 
# # Decompose the time series into seasonal, trend, and residual components
# decomposed_data <- decompose(ts_data)
# 
# # Plot the decomposition
# plot(decomposed_data)

##### year 

time_periods <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", tif_names))

# Create a data frame for ggplot
df3 <- data.frame(
  Vegetation_Count = count_values,
  Time_Period = time_periods
)

# Extract year from Time_Period
df3$Year <- format(df3$Time_Period, "%Y")

# Summarize vegetation count by year
df_summary <- aggregate(Vegetation_Count ~ Year, data = df3, sum)

################ Bar Chart ################

# Create a bar chart showing the total vegetation count for each year
ggplot(df_summary, aes(x = Year, y = Vegetation_Count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Total Vegetation Count by Year", x = "Year", y = "Vegetation Count") +
  theme_minimal()

################### summary
df3 <- data.frame(
  Vegetation_Count = count_values,
  Time_Period = time_periods
)

# Extract year and month from Time_Period
df3$Year <- as.numeric(format(df3$Time_Period, "%Y"))
df3$Month <- as.numeric(format(df3$Time_Period, "%m"))

# Filter for data from January to June (Months 1-6) for the years 2021, 2022, and 2023
df_filtered_Jan_June <- df3[df3$Year %in% c(2020,2021, 2022, 2023,2024) & df3$Month %in% 1:10, ]

# Summarize vegetation count by year for the selected months
df_summary <- aggregate(Vegetation_Count ~ Year, data = df_filtered_Jan_June, sum)

################ Year-to-Year Differences ################
# Calculate the year-to-year difference in vegetation count
df_summary$Difference <- c(NA, diff(df_summary$Vegetation_Count))

# Calculate the changing rate as the percentage change
df_summary$Change_Rate <- c(NA, diff(df_summary$Vegetation_Count) / df_summary$Vegetation_Count[-length(df_summary$Vegetation_Count)] * 100)

# Identify decreases and calculate the proportion of decreases
df_summary$Decreased <- df_summary$Difference < 0
proportion_decrease <- sum(df_summary$Decreased, na.rm = TRUE) / sum(!is.na(df_summary$Decreased)) * 100

# View the summary including change rate and decrease flags
print(df_summary)

cat("Proportion of years with a decrease in vegetation count:", proportion_decrease, "%\n")

################ Visualization ################
# Bar plot with the changing rate labeled for 2021-2023 comparison (Jan-Jun)
ggplot(df_summary, aes(x = Year, y = Vegetation_Count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(Change_Rate, 2), y = Vegetation_Count + 100), vjust = -0.5) + # Display change rates above bars
  labs(title = "Vegetation Count Comparison (Jan-Jun) for 2021-2023", x = "Year", y = "Vegetation Count") +
  theme_minimal()

########################### EDA ###################

water_temp <- read.csv("F:/Edge/surfside_TEMPERATURE_2019-01-07_2024-11-08.csv")
water_cond <- read.csv("F:/Edge/surfside_CONDUCTIVITY_2019-01-01_2024-11-08.csv")
water_oxy <- read.csv("F:/Edge/surfside_DISSOLVED_OXYGEN_2019-01-07_2024-11-08.csv")



water_temp <- water_temp %>%
  rename(Temperature = value)

water_cond <- water_cond %>%
  rename(conductivity = value)

water_oxy <- water_oxy %>%
  rename(oxygen = value)

water_temp$timestamp <- as.Date(water_temp$timestamp)
water_cond$timestamp <- as.Date(water_cond$timestamp)
water_oxy$timestamp <- as.Date(water_oxy$timestamp)
df3$timestamp <- as.Date(df3$Time_Period)

water_oxy <- water_oxy %>% distinct(timestamp, .keep_all = TRUE)
water_cond <- water_cond %>% distinct(timestamp, .keep_all = TRUE)
water_temp <- water_temp %>% distinct(timestamp, .keep_all = TRUE)

# Then perform the full join
# combined_df <- water_oxy %>%
#   full_join(water_cond, by = "timestamp") %>%
#   full_join(water_temp, by = "timestamp")

# many-many
combined_df_mm <- water_oxy %>%
  full_join(water_cond, by = "timestamp", relationship = "many-to-many") %>%
  full_join(water_temp, by = "timestamp", relationship = "many-to-many")

combined_df_mm <- combined_df_mm %>%
  select(timestamp, oxygen, conductivity, Temperature)


# Filter out rows with outliers
cleaned_df <- combined_df_mm %>%
  filter(
    between(oxygen, quantile(oxygen, 0.25, na.rm = TRUE) - 1.5 * IQR(oxygen, na.rm = TRUE), 
            quantile(oxygen, 0.75, na.rm = TRUE) + 1.5 * IQR(oxygen, na.rm = TRUE)),
    between(conductivity, quantile(conductivity, 0.25, na.rm = TRUE) - 1.5 * IQR(conductivity, na.rm = TRUE), 
            quantile(conductivity, 0.75, na.rm = TRUE) + 1.5 * IQR(conductivity, na.rm = TRUE)),
    between(Temperature, quantile(Temperature, 0.25, na.rm = TRUE) - 1.5 * IQR(Temperature, na.rm = TRUE), 
            quantile(Temperature, 0.75, na.rm = TRUE) + 1.5 * IQR(Temperature, na.rm = TRUE))
  )
# Calculate the mean
monthly_means <- cleaned_df %>%
  mutate(year_month = floor_date(ymd(timestamp), "month")) %>%  # Extract year-month
  group_by(year_month) %>%
  summarise(
    mean_oxygen = mean(oxygen, na.rm = TRUE),
    mean_conductivity = mean(conductivity, na.rm = TRUE),
    mean_temperature = mean(Temperature, na.rm = TRUE)
  )

monthly_means$timestamp <- monthly_means$year_month

#
combined_df <- monthly_means %>%
  mutate(timestamp = year_month) %>%
  full_join(df3, by = "timestamp") %>%
  distinct(timestamp, .keep_all = TRUE)%>%
  drop_na()%>%
  select(-year_month, -Time_Period)

# Only have 2022 Aug - 2023 Sep
model <- lm(Mean_Vegetation ~ mean_oxygen + mean_conductivity + mean_temperature , data = combined_df)
summary(model)

plot(model)
