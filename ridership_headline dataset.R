# Set working directory (optional)
# setwd("C:/Users/luqma/Downloads")
# setwd("C:/Users/zahir.LAPTOP-G2RVRKDO/Desktop/USM/Year 3/Sem 1/CPC351/Project")

# Read CSV file into 'ride' dataframe
ride <- read.csv("ridership_headline.csv")

# Install and load required packages
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)

# Replace NA values with 0 for specific columns
ride <- mutate(ride,
               bus_rkl = replace_na(ride$bus_rkl, 0),
               bus_rkn = replace_na(ride$bus_rkn, 0),
               bus_rpn = replace_na(ride$bus_rpn, 0),
               rail_lrt_ampang = replace_na(ride$rail_lrt_ampang, 0),
               rail_mrt_kajang = replace_na(ride$rail_mrt_kajang, 0),
               rail_lrt_kj = replace_na(ride$rail_lrt_kj, 0),
               rail_monorail = replace_na(ride$rail_monorail, 0),
               rail_mrt_pjy = replace_na(ride$rail_mrt_pjy, 0),
               rail_ets = replace_na(ride$rail_ets, 0),
               rail_intercity = replace_na(ride$rail_intercity, 0),
               rail_komuter_utara = replace_na(ride$rail_komuter_utara, 0),
               rail_tebrau = replace_na(ride$rail_tebrau, 0),
               rail_komuter = replace_na(ride$rail_komuter, 0))

# Check for missing values
check_missing_values <- function(data) {
  missing_count <- sapply(data, function(x) sum(is.na(x)))
  missing_data <- data.frame(Column = names(data), Missing_Count = missing_count)
  print(missing_data)
}

# Usage
check_missing_values(ride)

# Check data structure
str(ride)

# Change Date data type
ride$date <- as.Date(ride$date, format = "%d/%m/%Y")

# Check summary of data
summary(ride)

# Check dimension
dim(ride)

# Add month and year column for better data manipulation
ride$day <- format(ride$date, "%d")
ride$month <- format(ride$date, "%m")
ride$year <- format(ride$date, "%Y")

#filter data for different years
ride_2019 <- filter(ride, year == "2019")
ride_2020 <- filter(ride, year == "2020")
ride_2021 <- filter(ride, year == "2021")
ride_2022 <- filter(ride, year == "2022")
ride_2023 <- filter(ride, year == "2023")

# Subset data for different modes of transport
ride_rapidbus <- subset(ride, select = c("year", "month", "date", "bus_rkl", "bus_rkn", "bus_rpn"))
ride_lrt <- subset(ride, select = c("year", "month", "date", "rail_lrt_ampang", "rail_lrt_kj"))
ride_mrt <- subset(ride, select = c("year", "month", "date", "rail_mrt_kajang", "rail_mrt_pjy"))
ride_monorail <- subset(ride, select = c("year", "month", "date", "rail_monorail"))
ride_ktm <- subset(ride, select = c("year", "month", "date", "rail_komuter", "rail_komuter_utara", "rail_intercity", "rail_ets"))
ride_shuttle <- subset(ride, select = c("year", "month", "date", "rail_tebrau"))

# Format function for millions
format_millions <- function(x) {
  paste0(round(x / 1e6, 1), "M")
}

# Format function for thousands
format_thousands <- function(x) {
  paste0(round(x / 1e3, 1), "K")
}

### Data Exploratory ###

# Total ride by Year####

# Sum total ride for each type of ride
total_ride <- summarize(
  group_by(ride, year, month),
  total_bus_rkl = sum(bus_rkl),
  total_bus_rkn = sum(bus_rkn),
  total_bus_rpn = sum(bus_rpn),
  total_rail_lrt_ampang = sum(rail_lrt_ampang),
  total_rail_mrt_kajang = sum(rail_mrt_kajang),
  total_rail_lrt_kj = sum(rail_lrt_kj),
  total_rail_monorail = sum(rail_monorail),
  total_rail_mrt_pjy = sum(rail_mrt_pjy),
  total_rail_ets = sum(rail_ets),
  total_rail_intercity = sum(rail_intercity),
  total_rail_komuter_utara = sum(rail_komuter_utara),
  total_rail_tebrau = sum(rail_tebrau),
  total_rail_komuter = sum(rail_komuter)
)

# Calculate the total column by summing up relevant columns
total_ride$total <- rowSums(total_ride[, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)])

# Keep only necessary columns
total_ride <- total_ride[, c("year", "month", "total")]

# Plot the line graph for total rides by month and year
ggplot(total_ride, aes(x = as.numeric(month), y = total, group = year, color = as.factor(year))) +
  geom_line() +
  geom_point() +
  labs(title = "Total Rides by Month",
       x = "Month",
       y = "Total Rides",
       color = "Year") +
  scale_x_continuous(breaks = 1:12, labels = month.abb, minor_breaks = NULL) +
  theme_minimal() +
  scale_y_continuous(labels = format_millions)

#==============================================================================#

# LRT Comparison by Type####

# Subset the dataset to select relevant columns
ride_lrt <- subset(ride, select = c("year", "rail_lrt_ampang", "rail_lrt_kj"))

# Summarize the yearly rides for each LRT line
yearly_ride_lrt <- summarize(
  group_by(ride_lrt, year),
  total_rail_lrt_ampang = sum(rail_lrt_ampang),
  total_rail_lrt_kj = sum(rail_lrt_kj),
)

# Rename the columns for better clarity
colnames(yearly_ride_lrt) <- c("Year", "Total LRT Ampang", "Total LRT Kelana Jaya")

# Print the table in a stylized format
kable_styling(kable(yearly_ride_lrt, "html"))

# Plot a line graph for yearly rides on Ampang and Kelana Jaya LRT Lines
ggplot(yearly_ride_lrt, aes(x = Year)) +
  geom_line(aes(y = `Total LRT Ampang`, group = 1, color = "Ampang Line")) +
  geom_line(aes(y = `Total LRT Kelana Jaya`, group = 1, color = "Kelana Jaya Line")) +
  labs(title = "Yearly Rides on Ampang and Kelana Jaya LRT Lines",
       x = "Year",
       y = "Total Rides (in Millions)") +
  theme_minimal() +
  scale_color_manual(values = c("Ampang Line" = "blue", "Kelana Jaya Line" = "red")) +
  scale_y_continuous(labels = format_millions)

#==============================================================================#

# MRT Comparison by Type for year 2021####

# Subset the dataset to select relevant columns
ride_mrt <- subset(ride, select = c("year", "month", "rail_mrt_kajang", "rail_mrt_pjy"))

# Filter data for the year 2022
ride_mrt_2022 <- filter(ride_mrt, year == "2022")

# Summarize monthly rides for MRT Kajang and Putrajaya
monthly_ride_mrt <- summarize(
  group_by(ride_mrt_2022, month),
  total_rail_mrt_kajang = sum(rail_mrt_kajang),
  total_rail_mrt_pjy = sum(rail_mrt_pjy),
)

# Display a summary of the monthly rides
summary(monthly_ride_mrt)

# Filter for months 5 to 12
monthly_ride_mrt <- filter(monthly_ride_mrt, month == "05" | month == "06"
                           | month == "07" | month == "08" | month == "09"
                           | month == "10" | month == "11" | month == "12")

# Rename columns for clarity
colnames(monthly_ride_mrt) <- c("Month", "Total MRT Kajang", "Total MRT Putrajaya")

# Print the table in a stylized format
kable_styling(kable(monthly_ride_mrt, "html"))

# Plot separate line graphs for Monthly Rides of MRT Lines
ggplot(monthly_ride_mrt, aes(x = Month)) +
  geom_line(aes(y = `Total MRT Kajang`, group = 1, color = "MRT Kajang")) +
  geom_line(aes(y = `Total MRT Putrajaya`, group = 1, color = "MRT Putrajaya")) +
  labs(title = "Monthly Rides MRT Lines (May to December 2022)",
       x = "Month",
       y = "Total Rides (in Millions)") +
  theme_minimal() +
  scale_color_manual(values = c("MRT Kajang" = "limegreen", "MRT Putrajaya" = "skyblue")) +
  scale_y_continuous(labels = format_millions)

#==============================================================================#

# KTM Comparison by Type####

# Subset the dataset to select relevant columns
ride_ktm <- subset(ride, select = c("year", "rail_komuter", "rail_komuter_utara", "rail_intercity", "rail_ets"))

# Summarize yearly rides for different KTM types
yearly_ride_ktm <- summarize(
  group_by(ride_ktm, year),
  total_rail_komuter = sum(rail_komuter),
  total_rail_komuter_utara = sum(rail_komuter_utara),
  total_rail_intercity = sum(rail_intercity),
  total_rail_ets = sum(rail_ets),
)

# Rename columns for clarity
colnames(yearly_ride_ktm) <- c("Year", "Total KTM Komuter", "Total KTM Komuter Utara", "Total KTM Intercity", "Total KTM ETS")

# Print the table in a stylized format
kable_styling(kable(yearly_ride_ktm, "html"))

# Plot separate line graphs for Yearly Rides of KTM Lines
ggplot(yearly_ride_ktm, aes(x = Year)) +
  geom_line(aes(y = `Total KTM Komuter`, group = 1, color = "KTM Komuter")) +
  geom_line(aes(y = `Total KTM Komuter Utara`, group = 1, color = "KTM Komuter Utara")) +
  geom_line(aes(y = `Total KTM Intercity`, group = 1, color = "KTM Intercity")) +
  geom_line(aes(y = `Total KTM ETS`, group = 1, color = "KTM ETS")) +
  labs(title = "Yearly Rides KTM Lines",
       x = "Year",
       y = "Total Rides (in Millions)") +
  theme_minimal() +
  scale_color_manual(values = c("KTM Komuter" = "blue", "KTM Komuter Utara" = "red" , "KTM Intercity" = "yellow" , "KTM ETS" = "green")) +
  scale_y_continuous(labels = format_millions)

#==============================================================================#

# Rapid Bus Comparison by Type####

# Subset the dataset to select relevant columns
ride_rapidbus <- subset(ride, select = c("year", "bus_rkl", "bus_rkn", "bus_rpn"))

# Summarize yearly rides for different Rapid Bus types
yearly_ride_rapidbus <- summarize(
  group_by(ride_rapidbus, year),
  total_rail_bus_rkl = sum(bus_rkl),
  total_rail_bus_rkn = sum(bus_rkn),
  total_rail_bus_rpn = sum(bus_rpn),
)

# Rename columns for clarity
colnames(yearly_ride_rapidbus) <- c("Year", "Total Bus Rapid KL", "Total Bus Rapid Kuantan", "Total Bus Rapid Penang")

# Filter data for the years 2021, 2022, and 2023
yearly_ride_rapidbus <- filter(yearly_ride_rapidbus, Year == "2021" | Year == "2022" | Year == "2023")

# Print the table in a stylized format
kable_styling(kable(yearly_ride_rapidbus, "html"))

# Plot separate line graphs for Yearly Rides of Rapid Bus Lines
ggplot(yearly_ride_rapidbus, aes(x = Year)) +
  geom_line(aes(y = `Total Bus Rapid KL`, group = 1, color = "Bus Rapid KL")) +
  geom_line(aes(y = `Total Bus Rapid Kuantan`, group = 1, color = "Bus Rapid Kuantan")) +
  geom_line(aes(y = `Total Bus Rapid Penang`, group = 1, color = "Bus Rapid Penang")) +
  labs(title = "Yearly Rides Rapid Bus Lines",
       x = "Year",
       y = "Total Rides (in Millions)") +
  theme_minimal() +
  scale_color_manual(values = c("Bus Rapid KL" = "blue", "Bus Rapid Kuantan" = "red" , "Bus Rapid Penang" = "green")) +
  scale_y_continuous(labels = format_millions)

#==============================================================================#

# Monorail Year Comparison (2019 vs 2022)####

# Filter data for Monorail rides in 2019 and 2022
ride_monorail2019 <- filter(ride_monorail, year == "2019")
ride_monorail2022 <- filter(ride_monorail, year == "2022")

# Ensure the "year" column is present

# Summarize monthly rides for Monorail in 2019
monthly_monorail2019 <- summarize(
  group_by(ride_monorail2019, year, month),
  monthly_ride = sum(rail_monorail),
  .groups = 'drop'
)

# Summarize monthly rides for Monorail in 2022
monthly_monorail2022 <- summarize(
  group_by(ride_monorail2022, year, month),
  monthly_ride = sum(rail_monorail),
  .groups = 'drop'
)

# Combine the data for both years
monorail_19_22 <- rbind(monthly_monorail2019, monthly_monorail2022)

# View the combined data
View(monorail_19_22)

# Plot the line graph for Monorail rides in 2019 and 2022
ggplot(monorail_19_22, aes(x = month, y = monthly_ride / 1000, group = year, color = as.factor(year))) +
  geom_line() +
  labs(title = "Monthly Monorail Ridership in 2019 and 2022",
       x = "Month",
       y = "Monthly Ridership (in Thousands)",
       color = "Year") +
  theme_minimal() +
  scale_y_continuous(labels = format_thousands)

#==============================================================================#

# Shuttle Year Comparison (2022 vs 2023)####

# Filter data for Shuttle rides in 2022 and 2023
ride_shuttle2022 <- filter(ride_shuttle, year == "2022")
ride_shuttle2023 <- filter(ride_shuttle, year == "2023")

# Summarize monthly rides for Shuttle in 2022
monthly_shuttle2022 <- summarize(
  group_by(ride_shuttle2022, year, month),
  monthly_ride = sum(rail_tebrau),
  .groups = 'drop'
)

# Summarize monthly rides for Shuttle in 2023
monthly_shuttle2023 <- summarize(
  group_by(ride_shuttle2023, year, month),
  monthly_ride = sum(rail_tebrau),
  .groups = 'drop'
)

# Combine the data for both years
shuttle_22_23 <- rbind(monthly_shuttle2022, monthly_shuttle2023)

# View the combined data
View(shuttle_22_23)

# Plot the line graph for Shuttle rides in 2022 and 2023
ggplot(shuttle_22_23, aes(x = month, y = monthly_ride, group = year, color = as.factor(year))) +
  geom_line() +
  labs(title = "Monthly Shuttle Ridership in 2022 and 2023",
       x = "Month",
       y = "Monthly Ridership",
       color = "Year") +
  theme_minimal() +
  scale_y_continuous(labels = format_thousands)

#==============================================================================#


### Linear Regression ####
# With Covid Dates####
# Convert the year column to numeric
total_ride$year <- as.numeric(total_ride$year)

# Add a continuous month column starting from 1 in January 2019
total_ride$continuous_month <- (total_ride$year - 2019) * 12 + as.numeric(total_ride$month)


ggplot(total_ride, aes(x = continuous_month, y = total, group = year, color = as.factor(year))) +
  geom_line() +
  geom_point() +
  labs(title = "Total Rides By Month Throughout Year (2019-2022)",
       x = "Month",
       y = "Total Rides",
       color = "Year") +
  theme_minimal()+
  scale_y_continuous(labels = format_millions)

# Convert 'year' and 'month' to numeric in the total_ride data frame
total_ride$year <- as.numeric(as.character(total_ride$year))
total_ride$month <- as.numeric(as.character(total_ride$month))

# Split data into training and test sets
set.seed(123) # for reproducibility
sample_size <- floor(0.8 * nrow(total_ride))
train_indices <- sample(seq_len(nrow(total_ride)), size = sample_size)

train <- total_ride[train_indices, ]
test <- total_ride[-train_indices, ]

# Build the linear regression model
model <- lm(total ~ continuous_month, data = train)

# Display model summary
summary(model)

# Scatter plot with regression line for training set
plot(train$continuous_month, train$total, main = "Scatter Plot with Regression Line",
     xlab = "Continuous Month", ylab = "Total Rides")
abline(model, col = "red")  # Adding the regression line in red

# Adding points for the test set to visualize
points(test$continuous_month, test$total, col = "blue", pch = 16)

# Predict on the test set
predictions <- predict(model, test)

# Compare predictions to actual values
comparison <- data.frame(Actual = test$total, Predicted = predictions)

# Create a plot comparing actual vs predicted values
ggplot(test, aes(x = continuous_month)) +
  geom_line(aes(y = total, color = "Actual")) +
  geom_line(aes(y = predictions, color = "Predicted")) +
  labs(title = "Actual vs Predicted Total Rides",
       x = "Continuous Month",
       y = "Total Rides") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_y_continuous(labels = format_millions) +
  scale_x_continuous(limits = c(0, 72), breaks = seq(0, 72, by = 10)) 


# Predict future values until 2024
last_month <- max(total_ride$continuous_month)
future_months <- data.frame(continuous_month = (last_month + 1):(last_month + 14))
predictions2024 <- predict(model, newdata = future_months)

# Combine existing and predicted data
future_months$total <- predictions2024
combined_data <- rbind(total_ride, future_months)

# Plot the data with predictions for 2023 and 2024
ggplot(combined_data, aes(x = continuous_month, y = total, color = continuous_month <= last_month)) +
  geom_line() +
  labs(title = "Ridership Prediction for 2023 and 2024",
       x = "Continuous Month (from Jan 2019)",
       y = "Total Rides") +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                     guide = guide_legend(title = "Data Type"),
                     labels = c("Actual", "Predicted")) +
  theme_minimal() +
  scale_y_continuous(labels = format_millions)


# Predict for a future month
future_data <- data.frame(continuous_month = 60)
future_prediction <- predict(model, future_data)
print(future_prediction)

# Calculate performance metrics
mae <- mean(abs(comparison$Actual - comparison$Predicted))
mse <- mean((comparison$Actual - comparison$Predicted)^2)
rmse <- sqrt(mse)
r_squared <- 1 - sum((comparison$Actual - comparison$Predicted)^2) / sum((comparison$Actual - mean(comparison$Actual))^2)

# Print performance metrics
print(mae)
print(mse)
print(rmse)
print(r_squared)

#==============================================================================#

# Without Covid Dates####

# Create a copy of the total_ride data frame
total_ride_wo_covid <- data.frame(total_ride)

# Filter out rows where the month is greater than or equal to 30
total_ride_wo_covid <- total_ride_wo_covid %>%
  filter(continuous_month >= 30)

# View the filtered data
View(total_ride_wo_covid)

# Split data into training and test sets
set.seed(123) # for reproducibility
sample_size_wo_covid <- floor(0.8 * nrow(total_ride_wo_covid))
train_indices_wo_covid <- sample(seq_len(nrow(total_ride_wo_covid)), size = sample_size_wo_covid)

train_wo_covid <- total_ride_wo_covid[train_indices_wo_covid, ]
test_wo_covid <- total_ride_wo_covid[-train_indices_wo_covid, ]

# Build the linear regression model
model_wo_covid <- lm(total ~ continuous_month, data = train_wo_covid)

# Display model summary
summary(model_wo_covid)

# Scatter plot with regression line for training set
plot(train_wo_covid$continuous_month, train_wo_covid$total, 
     main = "Scatter Plot with Regression Line",
     xlab = "Continuous Month",
     ylab = "Total Rides")
abline(model_wo_covid, col = "red")  # Adding the regression line in red

# Adding points for the test set to visualize
points(test_wo_covid$continuous_month, test_wo_covid$total, col = "blue", pch = 16)


# Predict on the test set
predictions_wo_covid <- predict(model_wo_covid, test_wo_covid)

# Compare predictions to actual values
comparison_wo_covid <- data.frame(Actual_wo_covid = test_wo_covid$total, Predicted_wo_covid = predictions_wo_covid)

print(comparison_wo_covid)

# Create a plot comparing actual vs predicted values
ggplot(test_wo_covid, aes(x = continuous_month)) +
  geom_line(aes(y = total, color = "Actual")) +
  geom_line(aes(y = predictions_wo_covid, color = "Predicted")) +
  labs(title = "Actual vs Predicted Total Rides",
       x = "Continuous Month",
       y = "Total Rides") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_y_continuous(labels = format_millions) +
  scale_x_continuous(limits = c(30, 60), breaks = seq(30, 60, by = 5))

# Predict future values until 2024
last_month_wo_covid <- max(total_ride_wo_covid$continuous_month)
future_months_wo_covid <- data.frame(continuous_month = (last_month_wo_covid + 1):(last_month_wo_covid + 14))
predictions_wo_covid2024 <- predict(model_wo_covid, newdata = future_months_wo_covid)

# Combine existing and predicted data
future_months_wo_covid$total <- predictions_wo_covid2024
combined_data_wo_covid <- rbind(total_ride, future_months_wo_covid)

# Filter data for months greater than or equal to 30
combined_data_wo_covid <- combined_data_wo_covid %>%
  filter(continuous_month >= 30)

# Plot the data with predictions for 2023 and 2024
ggplot(combined_data_wo_covid, aes(x = continuous_month, y = total, color = continuous_month <= last_month_wo_covid)) +
  geom_line() +
  labs(title = "Ridership Prediction for 2023 and 2024",
       x = "Continuous Month (from Jan 2019)",
       y = "Total Rides") +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                     guide = guide_legend(title = "Data Type"),
                     labels = c("Actual", "Predicted")) +
  theme_minimal() +
  scale_y_continuous(labels = format_millions)

# Predict for a future month
future_data_wo_covid <- data.frame(continuous_month = 60)
future_prediction_wo_covid <- predict(model_wo_covid, future_data_wo_covid)
print(future_prediction_wo_covid)

# Calculate performance metrics
mae <- mean(abs(comparison_wo_covid$Actual_wo_covid - comparison_wo_covid$Predicted_wo_covid))
mse <- mean((comparison_wo_covid$Actual_wo_covid - comparison_wo_covid$Predicted_wo_covid)^2)
rmse <- sqrt(mse)
r_squared <- 1 - sum((comparison_wo_covid$Actual_wo_covid - comparison_wo_covid$Predicted_wo_covid)^2) / sum((comparison_wo_covid$Actual_wo_covid - mean(comparison_wo_covid$Actual_wo_covid))^2)

# Print performance metrics
print(mae)
print(mse)
print(rmse)
print(r_squared)


