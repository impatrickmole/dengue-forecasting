install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("ggpattern")

#Load needed libraries
library("ggplot2") #Visualization
library("ggfortify") #Visualization
library("tseries") #Statistical Tests for Time Series data
library("forecast") #Modeling and Forecasting
library(dplyr)
library(lubridate)
library(stringr)
library(tibble)
library(ggpattern)

#load dataset
dengue_data <- read.csv("cleaned_pangasinan_dengue_cases_2019_2024.csv")

#Group by 'Municipality', 'month', 'year' and sum 'cases'
cleaned_data <- dengue_data %>%
  group_by(Municipality, Month, Year) %>%
  summarise(
    cases = sum(Dengue_Cases, na.rm = TRUE)
  )

# Ensure 'month' is treated as a factor with the correct order
cleaned_data$Month <- factor(cleaned_data$Month, 
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Sort the data by 'year' and 'month'
sorted_datadengue <- cleaned_data %>%
  arrange(Municipality, Year, Month)

# Save the cleaned data to a new CSV file
#write.csv(sorted_datadengue, "cleaned_philippines_denguecases.csv", row.names = FALSE)

#Aggregate monthly dataset to annual
denguecasesregion<-aggregate(cases~Year+Municipality,sorted_datadengue,sum)

# Ensure that the 'year' column is numeric
denguecasesregion$Year <- as.numeric(denguecasesregion$Year)

#Temporal Spatial Heatmap
ggplot(denguecasesregion,aes(Year,Municipality,fill=cases))+geom_tile()+
  scale_fill_gradient2(low = "white",mid = "blue",high = "red",midpoint = 400)+
  scale_x_continuous(breaks = c(seq(2019,2024,1)))+ xlab(label="YEAR")+ylab(label="MUNICIPALITY")+
  ggtitle("DENGUE CASES PER YEAR AND MUNICIPALITY")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"))


#Aggregate data per month and per region
denguecasesmonth<-aggregate(cases~Month+Municipality,sorted_datadengue,sum)

#Temporal Spatial Heatmap
ggplot(denguecasesmonth,aes(Month,Municipality,fill=cases))+geom_tile()+
  scale_fill_gradient2(low = "white",mid = "blue",high = "red",midpoint = 300)+
  xlab(label="MONTH")+ylab(label="MUNICIPALITY")+
  ggtitle("MONTHLY DENGUE CASES IN PANGASINAN (2019-2024)")+
  theme(
    axis.title.x = element_text(size=10, face="bold"),
    axis.title.y = element_text(size=10, face="bold"))


########## NEW FIGURE dark pattern #######
# Group by 'Municipality', 'Month', 'Year' and sum 'cases'
cleaned_data <- dengue_data %>%
  group_by(Municipality, Month, Year) %>%
  summarise(
    cases = sum(Dengue_Cases, na.rm = TRUE)
  )

# Ensure 'Month' is treated as a factor with the correct order
cleaned_data$Month <- factor(cleaned_data$Month, 
                             levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Sort the data by 'Municipality', 'Year', and 'Month'
sorted_datadengue <- cleaned_data %>%
  arrange(Municipality, Year, Month)

# Aggregate monthly dataset to annual
denguecasesregion <- aggregate(cases ~ Year + Municipality, sorted_datadengue, sum)

# Ensure that 'Year' column is numeric
denguecasesregion$Year <- as.numeric(denguecasesregion$Year)

# Convert 'cases' into categorical bins for pattern mapping
denguecasesregion$cases_category <- cut(denguecasesregion$cases, 
                                        breaks = c(0, 100, 300, 500, Inf), 
                                        labels = c("Low", "Moderate", "High", "Very High"))

# Temporal Spatial Heatmap with Patterns (Fixed Version)
ggplot(denguecasesregion, aes(x = Year, y = Municipality)) +
  geom_tile_pattern(aes(fill = cases_category, pattern_type = cases_category),
                    color = "black",  # Outline for better visibility
                    pattern_density = 0.5, pattern_spacing = 0.02) +  # Adjust pattern density
  scale_pattern_type_manual(values = c("Low" = "stripe", 
                                       "Moderate" = "crosshatch", 
                                       "High" = "circle", 
                                       "Very High" = "wave")) +  # Removed 'choices' argument
  scale_fill_manual(values = c("Low" = "gray80", 
                               "Moderate" = "gray60", 
                               "High" = "gray40", 
                               "Very High" = "gray20")) +  # Keep grayscale for better clarity
  scale_x_continuous(breaks = seq(2019, 2024, 1)) +
  xlab("YEAR") + ylab("MUNICIPALITY") +
  ggtitle("DENGUE CASES PER YEAR AND MUNICIPALITY (PATTERN-BASED)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.position = "right"
  )
############### END NEW FIGURE ############


##### NEW PLOT GEOM LINE ALTERNATIVE TO HEATMAP ############
# Group by 'Municipality', 'Year' and sum 'cases'
denguecasesregion <- dengue_data %>%
  group_by(Municipality, Year) %>%
  summarise(cases = sum(Dengue_Cases, na.rm = TRUE))

# Convert 'Year' to numeric for proper plotting
denguecasesregion$Year <- as.numeric(denguecasesregion$Year)

# Create the line plot
ggplot(denguecasesregion, aes(x = Year, y = cases, group = Municipality, color = Municipality)) +
  geom_line(size = 1) +  # Line graph for each municipality
  geom_point(size = 2) +  # Add points to highlight data points
  scale_x_continuous(breaks = seq(min(denguecasesregion$Year), max(denguecasesregion$Year), 1)) +
  labs(title = "DENGUE CASES PER YEAR AND MUNICIPALITY",
       x = "YEAR",
       y = "NUMBER OF DENGUE CASES",
       color = "Municipality") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

##### END OF GEOM LINE ########

############### BOX PLOT ##################
# Group by 'Year' and 'Municipality' and sum dengue cases
yearly_dengue_cases <- dengue_data %>%
  group_by(Year, Municipality) %>%
  summarise(total_cases = sum(Dengue_Cases, na.rm = TRUE))

# Create a box plot to compare yearly dengue cases
ggplot(yearly_dengue_cases, aes(x = factor(Year), y = total_cases, fill = factor(Year))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +  # Box plot with red outliers
  scale_fill_brewer(palette = "Set2") +  # Use a color scheme for better readability
  labs(title = "Yearly Comparison of Dengue Outbreaks in Pangasinan",
       x = "Year",
       y = "Total Dengue Cases per Municipality",
       fill = "Year") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

################ END OF BOX PLOT ###############

################ BAR GRAPH HORIZONTAL ##########################
# Group by 'Municipality' and sum total dengue cases across all years
total_dengue_cases <- dengue_data %>%
  group_by(Municipality) %>%
  summarise(total_cases = sum(Dengue_Cases, na.rm = TRUE)) %>%
  arrange(desc(total_cases))  # Sort in descending order

# Create a bar chart to show dengue cases per municipality
ggplot(total_dengue_cases, aes(x = reorder(Municipality, total_cases), y = total_cases, fill = total_cases)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar plot without legend
  coord_flip() +  # Flip for better readability
  scale_fill_gradient(low = "lightblue", high = "darkred") +  # Gradient for emphasis
  labs(title = "Total Dengue Cases per Municipality (2019-2024)",
       x = "Municipality",
       y = "Total Dengue Cases") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )
#########

##### TIME SERIES ANALYSIS ####

#Aggregate regional data to represent the entire country
denguecasesph<-aggregate(cases~Month+Year,sorted_datadengue,sum)
str(denguecasesph)

#Convert data into a time series / per thousand
denguecasesphts <- ts(denguecasesph$cases / 100, start = c(2019, 1), end = c(2024, 10), frequency = 12)


# Plot time series data with full year labels
autoplot(denguecasesphts) +
  xlab(label = "TIME") +
  ylab(label = "DENGUE CASES PER 1,000 POPULATION") +
  ggtitle("DENGUE CASES IN PANGASINAN") +
  scale_x_continuous(breaks = seq(2019, 2024, 1))  # Set breaks for each year from 2019 to 2024


######################## ARIMA MODEL STARTS HERE ######################## 
#Plot Decomposed Time Series Data
autoplot(decompose(denguecasesphts))

#Test Stationarity of Time Series data
adf.test((denguecasesphts))

#Check the number of lag/s differencing needed to stationarize time series
ndiffs(denguecasesphts)

#Perform lag 1 differencing
denguecasesphtsdiff<-diff(denguecasesphts)

#Plot differenced data
autoplot(denguecasesphtsdiff)

#Check stationarity of differenced data
adf.test(denguecasesphtsdiff)

##ARIMA MODELING ##
dengueph_arima<-auto.arima(denguecasesphts)
dengueph_arima

#Make forecast for 2025
dengueph_forecast<-forecast(dengueph_arima,12)

# Plot time series data with full year labels
autoplot(dengueph_forecast) +
  xlab(label = "TIME") +
  ylab(label = "DENGUE CASES PER 1,000 POPULATION") +
  ggtitle("FORECASTED DENGUE CASES IN PANGASINAN FOR THE YEAR 2025") +
  scale_x_continuous(breaks = seq(2019, 2026, 1))  # Set breaks for each year from 2019 to 2024



##### ARIMA MODEL TESTING AND VALIDATION #####
# Split the data into training (up to 2023) and testing (2024) sets
train_data <- window(denguecasesphts, end = c(2023, 12))
test_data <- window(denguecasesphts, start = c(2024, 1))

# Fit an ARIMA model (for example) on the training data
model_arima <- auto.arima(train_data)

# Forecast for the test period (2024)
forecast_arima <- forecast(model_arima, h = length(test_data))

# Check accuracy of the model
accuracy_metrics <- accuracy(forecast_arima, test_data)

# Print accuracy metrics
print(accuracy_metrics)

######################## ARIMA MODEL ENDS HERE ######################## 


### PREDICTION ON WHAT MUNICIPALITY MAY HAVE HIGHER DENGUE CASES FOR 2025 ###

# Prepare an empty list to store forecasted cases by municipality
municipality_forecasts <- list()

# Loop through each municipality and fit an ARIMA model
for (municipality in unique(sorted_datadengue$Municipality)) {
  
  # Filter data for the current municipality
  municipality_data <- sorted_datadengue %>%
    filter(Municipality == municipality) %>%
    arrange(Year, Month)
  
  # Create a time series object
  municipality_ts <- ts(municipality_data$cases, 
                        start = c(2019, 1), 
                        end = c(2024, 10), 
                        frequency = 12)
  
  # Fit an ARIMA model to the time series
  municipality_arima <- auto.arima(municipality_ts)  # No seasonal parameter
  
  # Forecast for the next 12 months (2025)
  municipality_forecast <- forecast(municipality_arima, h = 12)
  
  # Sum the forecasted cases for 2025, replace negative values with zero
  total_forecast_2025 <- sum(pmax(municipality_forecast$mean, 1))  # Ensure no negative forecasts
  
  # Store the result in the list, handling any NA values
  municipality_forecasts[[municipality]] <- ifelse(is.na(total_forecast_2025), 0, total_forecast_2025)
}

# Convert the list to a data frame
forecast_results <- data.frame(
  Municipality = names(municipality_forecasts),
  Forecasted_Cases_2025 = unlist(municipality_forecasts)
)

# Sort the data frame from highest to lowest forecasted cases
forecast_results <- forecast_results %>%
  arrange(desc(Forecasted_Cases_2025))

# Print sorted forecast results
print(forecast_results)

# Plot the results
library(ggplot2)
ggplot(forecast_results, aes(x = reorder(Municipality, -Forecasted_Cases_2025), y = Forecasted_Cases_2025)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Municipality") +
  ylab("Forecasted Dengue Cases for 2025") +
  ggtitle("Forecasted Dengue Cases per Municipality in 2025 (Highest to Lowest)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### PREDICTION ON WHAT MONTH MAY HAVE HIGHER DENGUE CASES FOR 2025 ###

# Create an empty list to store monthly forecasted cases by municipality
monthly_forecasts <- list()

# Loop through each municipality and forecast monthly cases for 2025
for (municipality in unique(sorted_datadengue$Municipality)) {
  
  # Filter data for the current municipality
  municipality_data <- sorted_datadengue %>%
    filter(Municipality == municipality) %>%
    arrange(Year, Month)
  
  # Create a time series object
  municipality_ts <- ts(municipality_data$cases, 
                        start = c(2019, 1), 
                        end = c(2024, 10), 
                        frequency = 12)
  
  # Fit an ARIMA model to the time series
  municipality_arima <- auto.arima(municipality_ts)
  
  # Forecast for the next 12 months (2025)
  municipality_forecast <- forecast(municipality_arima, h = 12)
  
  # Store the monthly forecast for 2025 in the list, replacing negative values with zero
  monthly_forecasts[[municipality]] <- pmax(municipality_forecast$mean, 0)  # Ensure no negative forecasts
}

# Combine all municipality forecasts into a data frame with months and cases
monthly_forecast_df <- do.call(cbind, monthly_forecasts) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Month") %>%
  mutate(Month = as.numeric(Month)) %>%
  mutate(Total_Forecast_Cases = rowSums(select(., -Month)))  # Sum cases across all municipalities

# Match month names to numbers (assuming months 1-12 represent Jan-Dec)
monthly_forecast_df$Month_Name <- month.abb

# Find the month with the highest total forecasted cases in 2025
peak_month <- monthly_forecast_df %>%
  filter(Total_Forecast_Cases == max(Total_Forecast_Cases))

print(peak_month)

# Ensure that Month_Name is a factor with the correct order
monthly_forecast_df$Month_Name <- factor(monthly_forecast_df$Month_Name, 
                                         levels = month.abb)

# Ensure Month_Name is ordered by month
monthly_forecast_df$Month_Name <- factor(monthly_forecast_df$Month_Name, 
                                         levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Plot forecasted cases by month as an area plot
ggplot(monthly_forecast_df, aes(x = Month_Name, y = Total_Forecast_Cases, group = 1)) +
  geom_area(fill = "steelblue", alpha = 0.6) +
  geom_point(color = "red", size = 2) +
  geom_line(group = 1, color = "blue") +
  xlab("Month") +
  ylab("Total Forecasted Dengue Cases for 2025") +
  ggtitle("Forecasted Monthly Dengue Cases in 2025 (Total for All Municipalities)") +
  theme_minimal()






############################### 70:30 RATIO PARTITION ####################################

# Set up the 70:30 split
train_size <- round(length(denguecasesphts) * 0.7)
train_data <- window(denguecasesphts, end = c(2019 + (train_size - 1) / 12))

# The testing data starts immediately after the training data
test_data <- window(denguecasesphts, start = c(2019 + train_size / 12))

# Plot to verify the split
autoplot(denguecasesphts) +
  autolayer(train_data, series = "Training Data", PI = FALSE) +
  autolayer(test_data, series = "Testing Data", PI = FALSE) +
  xlab("Year") +
  ylab("Dengue Cases per 1,000 Population") +
  ggtitle("70:30 Train-Test Split of Dengue Cases Time Series")

# Train the ARIMA model on the training set
dengueph_arima <- auto.arima(train_data)
summary(dengueph_arima)

# Forecast for the length of the test set
test_length <- length(test_data)
dengueph_forecast <- forecast(dengueph_arima, h = test_length)

# Plot the forecast alongside actual test data
autoplot(dengueph_forecast) +
  autolayer(test_data, series = "Actual Data", PI = FALSE) +
  xlab("Year") +
  ylab("Dengue Cases per 1,000 Population") +
  ggtitle("Forecasted vs. Actual Dengue Cases for Testing Period")

# Evaluate the model's accuracy
accuracy_metrics <- accuracy(dengueph_forecast, test_data)
print(accuracy_metrics)


#################### 70:30 RATIO PARTITION 2019-2023 DATA ONLY ######################
# Step 1: Filter data to only include years 2019 to 2023
denguecasesph_filtered <- subset(denguecasesph, Year >= 2019 & Year <= 2023)

# Step 2: Convert the filtered data to a time series
denguecasesphts_filtered <- ts(denguecasesph_filtered$cases, start = c(2019, 1), frequency = 12)

# Step 3: Split the filtered time series data into a 70:30 ratio
train_data <- window(denguecasesphts_filtered, end = c(2022, 6))  # Approx. 70% data till mid-2022
test_data <- window(denguecasesphts_filtered, start = c(2022, 7)) # Remaining 30%

# Step 4: Apply the ARIMA model to the training data
dengueph_arima <- auto.arima(train_data)

# Step 5: Forecast using the ARIMA model and evaluate accuracy on the test set
dengueph_forecast <- forecast(dengueph_arima, h = length(test_data))

# Step 6: Plot the forecast along with the training and test data
autoplot(dengueph_forecast) +
  autolayer(test_data, series = "Test Data", PI = FALSE) +
  xlab("Time") +
  ylab("Dengue Cases per 1,000 Population") +
  ggtitle("Dengue Cases Forecast for Pangasinan (2019-2023 Data)")

# Step 7: Evaluate forecast accuracy
accuracy(dengueph_forecast, test_data)


#################### 70:30 RATIO PARTITION 2019-2023 DATA ONLY PREDICTION FOR 2024######################
# Step 1: Filter data to only include years 2019 to 2023 (already done)
denguecasesph_filtered <- subset(denguecasesph, Year >= 2019 & Year <= 2023)

# Step 2: Convert the filtered data to a time series (already done)
denguecasesphts_filtered <- ts(denguecasesph_filtered$cases, start = c(2019, 1), frequency = 12)

# Step 3: Set the entire filtered time series data as the training data
train_data <- denguecasesphts_filtered  # Using all data from 2019 to 2023

# Step 4: Apply the ARIMA model to the updated training data
dengueph_arima <- auto.arima(train_data)

# Step 5: Forecast for the next 12 months (2024)
dengueph_forecast_2024 <- forecast(dengueph_arima, h = 12)

# Step 6: Plot the forecast
autoplot(dengueph_forecast_2024) +
  xlab("Time") +
  ylab("Dengue Cases per 1,000 Population") +
  ggtitle("Dengue Cases Forecast for Pangasinan (2024)")

# Optionally, Step 7: Review forecast values for 2024
dengueph_forecast_2024


###########################


#################### 80:20 RATIO PARTITION 2019-2023 DATA ONLY ######################
# Step 1: Filter data to only include years 2019 to 2023
denguecasesph_filtered <- subset(denguecasesph, Year >= 2019 & Year <= 2023)

# Step 2: Convert the filtered data to a time series
denguecasesphts_filtered <- ts(denguecasesph_filtered$cases, start = c(2019, 1), frequency = 12)

# Step 3: Split the filtered time series data into an 80:20 ratio
total_length <- length(denguecasesphts_filtered)
train_length <- round(total_length * 0.8)  # Calculate 80% of the total length

train_data <- window(denguecasesphts_filtered, end = c(2019 + floor((train_length - 1) / 12), (train_length - 1) %% 12 + 1))
test_data <- window(denguecasesphts_filtered, start = c(2019 + floor(train_length / 12), train_length %% 12 + 1))

# Step 4: Apply the ARIMA model to the training data
dengueph_arima <- auto.arima(train_data)

# Step 5: Forecast using the ARIMA model and evaluate accuracy on the test set
dengueph_forecast <- forecast(dengueph_arima, h = length(test_data))

# Step 6: Plot the forecast along with the training and test data
autoplot(dengueph_forecast) +
  autolayer(test_data, series = "Test Data", PI = FALSE) +
  xlab("Time") +
  ylab("Dengue Cases per 1,000 Population") +
  ggtitle("Dengue Cases Forecast for Pangasinan (2019-2023 Data)")

# Step 7: Evaluate forecast accuracy
accuracy(dengueph_forecast, test_data)


############## FORECASTING WITH SARIMA WITH 70:30 RATIO ############################
# Filter dataset to include only data from 2019 to 2023
denguecasesph_filtered <- subset(denguecasesph, Year >= 2019 & Year <= 2023)

# Convert filtered data into a time series object
denguecasesphts_filtered <- ts(denguecasesph_filtered$cases, start = c(2019, 1), frequency = 12)

# Split data into 70% training and 30% testing
train_data <- window(denguecasesphts_filtered, end = c(2022, 12))  # 70% for training
test_data <- window(denguecasesphts_filtered, start = c(2023, 1))   # 30% for testing

# Apply Seasonal ARIMA model
# `auto.arima()` function with seasonal=TRUE searches for optimal seasonal parameters (P, D, Q)
sarima_model <- auto.arima(train_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Display the SARIMA model summary
summary(sarima_model)

# Forecast dengue cases for the test period (1 year ahead)
sarima_forecast <- forecast(sarima_model, h = length(test_data))

# Plot forecast with full year labels and actual data
autoplot(sarima_forecast) +
  autolayer(denguecasesphts_filtered, series = "Actual Data", color = "blue") +  # Add actual data in blue
  xlab(label = "TIME") +
  ylab(label = "DENGUE CASES PER 1,000 POPULATION") +
  ggtitle("FORECASTED VS ACTUAL DENGUE CASES IN PANGASINAN (2019-2023)") +
  scale_x_continuous(breaks = seq(2019, 2024, 1)) +  # Set breaks for each year from 2019 to 2024
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Series")  # Add legend label

# Evaluate the model's accuracy on both training and test sets
accuracy_metrics <- accuracy(sarima_forecast, test_data)
print(accuracy_metrics)


# Filter dataset to include only data from 2019 to 2023
denguecasesph_filtered <- subset(denguecasesph, Year >= 2019 & Year <= 2023)

# Convert filtered data into a time series object
denguecasesphts_filtered <- ts(denguecasesph_filtered$cases, start = c(2019, 1), frequency = 12)

# Split data into 80% training and 20% testing
total_length <- length(denguecasesphts_filtered)
train_length <- round(total_length * 0.8)  # Calculate 80% of the total length

train_data <- window(denguecasesphts_filtered, end = c(2019 + floor((train_length - 1) / 12), (train_length - 1) %% 12 + 1))
test_data <- window(denguecasesphts_filtered, start = c(2019 + floor(train_length / 12), train_length %% 12 + 1))

# Apply Seasonal ARIMA model
# `auto.arima()` function with seasonal=TRUE searches for optimal seasonal parameters (P, D, Q)
sarima_model <- auto.arima(train_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Display the SARIMA model summary
summary(sarima_model)

# Forecast dengue cases for the test period (1 year ahead)
sarima_forecast <- forecast(sarima_model, h = length(test_data))

# Plot forecast with full year labels and actual data
autoplot(sarima_forecast) +
  autolayer(denguecasesphts_filtered, series = "Actual Data", color = "blue") +  # Add actual data in blue
  xlab(label = "TIME") +
  ylab(label = "DENGUE CASES PER 1,000 POPULATION") +
  ggtitle("FORECASTED VS ACTUAL DENGUE CASES IN PANGASINAN (2019-2023)") +
  scale_x_continuous(breaks = seq(2019, 2024, 1)) +  # Set breaks for each year from 2019 to 2024
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "Series")  # Add legend label

# Evaluate the model's accuracy on both training and test sets
accuracy_metrics <- accuracy(sarima_forecast, test_data)
print(accuracy_metrics)


############## EDA USING 2019-2023 DATA ONLY ############################

# Filter dataset to only include data from 2019 to 2023
sorted_datadengue_filtered <- subset(sorted_datadengue, Year >= 2019 & Year <= 2023)

# Aggregate monthly dataset to annual
denguecasesregion <- aggregate(cases ~ Year + Municipality, sorted_datadengue_filtered, sum)

# Ensure that the 'year' column is numeric
denguecasesregion$Year <- as.numeric(denguecasesregion$Year)

# Temporal Spatial Heatmap for Annual Data
ggplot(denguecasesregion, aes(Year, Municipality, fill = cases)) + 
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "blue", high = "red", midpoint = 400) +
  scale_x_continuous(breaks = seq(2019, 2023, 1)) +  # Adjusted to show 2023 as the last year
  xlab(label = "YEAR") + 
  ylab(label = "MUNICIPALITY") +
  ggtitle("DENGUE CASES PER YEAR AND MUNICIPALITY (2019-2023)") +
  theme(
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold")
  )

# Aggregate data per month and per region
denguecasesmonth <- aggregate(cases ~ Month + Municipality, sorted_datadengue_filtered, sum)

# Temporal Spatial Heatmap for Monthly Data
ggplot(denguecasesmonth, aes(Month, Municipality, fill = cases)) + 
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "blue", high = "red", midpoint = 300) +
  xlab(label = "MONTH") + 
  ylab(label = "MUNICIPALITY") +
  ggtitle("MONTHLY DENGUE CASES IN PANGASINAN (2019-2023)") +
  theme(
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold")
  )


##### TIME SERIES ANALYSIS ####

#Aggregate regional data to represent the entire country
denguecasesph<-aggregate(cases~Month+Year,sorted_datadengue,sum)
str(denguecasesph)

#Convert data into a time series / per thousand
denguecasesphts <- ts(denguecasesph$cases / 100, start = c(2019, 1), end = c(2023, 12), frequency = 12)


# Plot time series data with full year labels
autoplot(denguecasesphts) +
  xlab(label = "TIME") +
  ylab(label = "DENGUE CASES PER 1,000 POPULATION") +
  ggtitle("DENGUE CASES IN PANGASINAN") +
  scale_x_continuous(breaks = seq(2019, 2023, 1))  # Set breaks for each year from 2019 to 2024







### PREDICTION ON WHAT MUNICIPALITY MAY HAVE HIGHER DENGUE CASES FOR 2024 USING 2019-2023 DATA IN 70:30 RATIO ###

# Prepare an empty list to store forecasted cases by municipality
municipality_forecasts <- list()

# Loop through each municipality and fit an ARIMA model
for (municipality in unique(sorted_datadengue$Municipality)) {
  
  # Filter data for the current municipality
  municipality_data <- sorted_datadengue %>%
    filter(Municipality == municipality, Year >= 2019 & Year <= 2023) %>%
    arrange(Year, Month)
  
  # Create a time series object (2019 to 2023 only)
  municipality_ts <- ts(municipality_data$cases, 
                        start = c(2019, 1), 
                        frequency = 12)
  
  # Fit an ARIMA model to the time series
  municipality_arima <- auto.arima(municipality_ts, seasonal = TRUE)  # Adds seasonality if present
  
  # Forecast for the next 12 months (2025)
  municipality_forecast <- forecast(municipality_arima, h = 12)
  
  # Sum the forecasted cases for 2025, replace negative values with zero
  total_forecast_2025 <- sum(pmax(municipality_forecast$mean, 1))  # Ensure no negative forecasts

  
  # Store the result in the list, handling any NA values
  municipality_forecasts[[municipality]] <- ifelse(is.na(total_forecast_2025), 0, total_forecast_2025)
}

# Convert the list to a data frame
forecast_results <- data.frame(
  Municipality = names(municipality_forecasts),
  Forecasted_Cases_2025 = unlist(municipality_forecasts)
)

# Sort the data frame from highest to lowest forecasted cases
forecast_results <- forecast_results %>%
  arrange(desc(Forecasted_Cases_2025))

# Print sorted forecast results
print(forecast_results)

# Plot the results
library(ggplot2)
ggplot(forecast_results, aes(x = reorder(Municipality, -Forecasted_Cases_2025), y = Forecasted_Cases_2025)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Municipality") +
  ylab("Forecasted Dengue Cases for 2024") +
  ggtitle("Forecasted Dengue Cases per Municipality in 2024 (Highest to Lowest)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



### PREDICTION ON WHAT MONTH MAY HAVE HIGHER DENGUE CASES FOR 2025 ###

# Create an empty list to store monthly forecasted cases by municipality
monthly_forecasts <- list()

# Loop through each municipality and forecast monthly cases for 2025
for (municipality in unique(sorted_datadengue$Municipality)) {
  
  # Filter data for the current municipality
  municipality_data <- sorted_datadengue %>%
    filter(Municipality == municipality, Year >= 2019 & Year <= 2023) %>%
    arrange(Year, Month)
  
  # Create a time series object (2019 to 2023 only)
  municipality_ts <- ts(municipality_data$cases, 
                        start = c(2019, 1), 
                        frequency = 12)
  
  # Fit a seasonal ARIMA model to capture monthly patterns
  municipality_arima <- auto.arima(municipality_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  
  # Forecast for the next 12 months (2025)
  municipality_forecast <- forecast(municipality_arima, h = 12)
  
  # Store the monthly forecast for 2025 in the list, with a minimum threshold to avoid zero predictions
  monthly_forecasts[[municipality]] <- ifelse(municipality_forecast$mean < 1, 1, municipality_forecast$mean)  # Minimum of 1 case
}

# Combine all municipality forecasts into a data frame with months and cases
monthly_forecast_df <- do.call(cbind, monthly_forecasts) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Month") %>%
  mutate(Month = as.numeric(Month)) %>%
  mutate(Total_Forecast_Cases = rowSums(select(., -Month)))  # Sum cases across all municipalities

# Match month names to numbers (assuming months 1-12 represent Jan-Dec)
monthly_forecast_df$Month_Name <- month.abb

# Find the month with the highest total forecasted cases in 2025
peak_month <- monthly_forecast_df %>%
  filter(Total_Forecast_Cases == max(Total_Forecast_Cases))

print(peak_month)

# Ensure that Month_Name is a factor with the correct order
monthly_forecast_df$Month_Name <- factor(monthly_forecast_df$Month_Name, 
                                         levels = month.abb)

# Plot forecasted cases by month as an area plot
library(ggplot2)
ggplot(monthly_forecast_df, aes(x = Month_Name, y = Total_Forecast_Cases, group = 1)) +
  geom_area(fill = "steelblue", alpha = 0.6) +
  geom_point(color = "red", size = 2) +
  geom_line(group = 1, color = "blue") +
  xlab("Month") +
  ylab("Total Forecasted Dengue Cases for 2024") +
  ggtitle("Forecasted Monthly Dengue Cases in 2024 (Total for All Municipalities)") +
  theme_minimal()

