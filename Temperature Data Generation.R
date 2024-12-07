# Load required libraries
library(dplyr)
library(psych)
library(zoo)
library(plotly)
# Specify parameters
set.seed(1234)
time_sequence <- seq(
    from = as.POSIXct("2024-12-01 00:00:00"),
    to = as.POSIXct("2024-12-01 23:59:00"),
    by = "15 min"
)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#===========================================================#
####               (1) Dataset Preparation               ####
#===========================================================#
# Simulate winter temperature data with gradual changes
simulate_temperature_winter <- function(time_sequence) {
  # Initialize temperatures
  temperature <- numeric(length(time_sequence))

  # Define base temperatures by time of day
  for (i in seq_along(time_sequence)) {
    hour <- as.numeric(format(time_sequence[i], "%H"))

    if (hour < 6 || hour >= 20) {
      base_temp <- 4  # Night: Very cold
    } else if (hour >= 6 && hour < 12) {
      base_temp <- 5  # Morning: Slightly warming
    } else if (hour >= 12 && hour < 18) {
      base_temp <- 7  # Afternoon: Warmest period
    } else {
      base_temp <- 4  # Evening: Cooling down
    }

    # Add gradual changes to temperature
    if (i == 1) {
      temperature[i] <- rnorm(1, mean = base_temp, sd = 1)  # Initial value
    } else {
      temperature[i] <- temperature[i - 1] + rnorm(1, mean = 0, sd = 0.5)  # Gradual change
      # Prevent unrealistic jumps by constraining within a reasonable range
      temperature[i] <- max(min(temperature[i], base_temp + 1.5), base_temp - 1.5)
    }
  }

  return(round(temperature, 3))
}


# Generate clean temperature data
# This is a baseline for evaluating the applied
# preprocessing techniques
temperature_data_clean <- data.frame(
  datetime = time_sequence,
  temperature = simulate_temperature_winter(time_sequence)
)



# Variation 1: Multiple spikes in temperature over a short timeframe
temperature_data_spike <- temperature_data_clean
spike_start_time <- sample(
  temperature_data_spike$datetime[format(temperature_data_spike$datetime, "%H") < 6], 1
)

# Define the duration and magnitude of the spikes
spike_duration <- 1  # Number of consecutive 15-minute intervals
spike_values <- seq(25, 35, length.out = spike_duration) +
  rnorm(spike_duration, mean = 0, sd = 1)  # Slight variations

# Apply the spikes
spike_indices <- which(
  temperature_data_spike$datetime >= spike_start_time &
    temperature_data_spike$datetime < spike_start_time + (spike_duration * 15 * 60)
)

temperature_data_spike$temperature[spike_indices] <- spike_values


# Variation 2: Missing data
# Ensure at least one value remains per hour while introducing missing data
temperature_data_missing <- temperature_data_clean

# Identify unique hours
unique_hours <- unique(format(temperature_data_missing$datetime, "%Y-%m-%d %H"))

# Select one random index to keep per hour
keep_indices <- sapply(unique_hours, function(hour) {
  sample(which(format(temperature_data_missing$datetime, "%Y-%m-%d %H") == hour), 1)
})

# Generate missing indices, excluding the kept indices
all_indices <- 1:nrow(temperature_data_missing)
missing_indices <- setdiff(all_indices, keep_indices)
missing_indices <- sample(missing_indices, size = 0.30 * length(missing_indices))  # 65% missing

# Constructing the dirty dataset
temperature_data_dirty <- temperature_data_spike
temperature_data_dirty$temperature[missing_indices] <- NA

temperature_data_dirty$datetime <- as.POSIXct(temperature_data_dirty$datetime)
temperature_data_dirty$datetime <- format(temperature_data_dirty$datetime, "%d/%m/%Y %H:%M:%S")
write.csv(temperature_data_dirty, "temperature_data_dirty.csv", row.names = FALSE, quote = FALSE)

temperature_data_clean$datetime <- as.POSIXct(temperature_data_clean$datetime)
temperature_data_clean$datetime <- format(temperature_data_clean$datetime, "%d/%m/%Y %H:%M:%S")
write.csv(temperature_data_clean, "temperature_data_clean.csv", row.names = FALSE, quote = FALSE)

#===========================================================#
####               (2) Dataset Exploration               ####
#===========================================================#

temperature_data_dirty <- read.csv("temperature_data_dirty.csv")
temperature_data_dirty$datetime <- as.POSIXct(temperature_data_dirty$datetime, format = "%d/%m/%Y %H:%M:%S")
temperature_data_dirty$temperature <- as.numeric(temperature_data_dirty$temperature)

temperature_data_clean <- read.csv("temperature_data_clean.csv")
temperature_data_clean$datetime <- as.POSIXct(temperature_data_clean$datetime, format = "%d/%m/%Y %H:%M:%S")
temperature_data_clean$temperature <- as.numeric(temperature_data_clean$temperature)

plot_temperature_combined <- function(data_list, title, line_colors) {
  # Initialize the plot
  plot <- plot_ly()
  
  # Loop over each dataset and add it to the plot
  for (i in seq_along(data_list)) {
    plot <- plot %>% add_trace(
      data = data_list[[i]],
      x = ~datetime,
      y = ~temperature,
      type = 'scatter',
      mode = 'lines+markers',
      name = names(data_list)[i],
      line = list(color = line_colors[i], width = 2),
      marker = list(size = 0.1)
    )
  }
  # Add layout to the plot
  plot <- plot %>% layout(
    title = title,
    xaxis = list(title = 'Hour'),
    yaxis = list(title = 'Temperature (°C)'),
    plot_bgcolor = 'white',
    legend = list(title = list(text = 'Data Type'))
  )
  
  # Return the plot
  return(plot)
}

head(temperature_data_dirty)
round(t(describe(temperature_data_dirty$temperature)), 3)

data_list <- list(
  "Sensor Data" = temperature_data_dirty
)
# Define the colors for each dataset
line_colors <- c('blue')
# Plot the combined graph
plot_temperature_combined(
  data_list,
  "Hourly Temperature Sensor Results",
  line_colors
)


#=============================================================#
####               (3) Dataset Preprocessing               ####
#=============================================================#

# 1.1 Preprocessing the spike data #
#----------------------------------#

# Preprocessing function to handle spikes using Winsorization
preprocess_spike_data <- function(data) {
  # Identify quartiles and IQR
  Q1 <- quantile(data$temperature, 0.25, na.rm = TRUE)
  Q3 <- quantile(data$temperature, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Calculate lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Replace outliers with NA
  data$temperature[data$temperature < lower_bound | data$temperature > upper_bound] <- NA
  
  return(data)
}

# Preprocess the spike data
temperature_data_dirty_processed <- preprocess_spike_data(temperature_data_dirty)

data_list <- list(
  "Sensor Data" = temperature_data_dirty_processed
)
# Define the colors for each dataset
line_colors <- c('blue')
# Plot the combined graph
plot_temperature_combined(
  data_list,
  "Hourly Temperature Sensor Results Without Outliers",
  line_colors
)


# 1.2 Preprocessing the missing data (imputation using mean) #
#------------------------------------#
# Preprocessing function to handle missing data using imputation
preprocess_missing_data_imputation <- function(data) {
  
  mean_temperature <- mean(data$temperature, na.rm = TRUE)
  
  data$temperature[is.na(data$temperature)] <- mean_temperature
  return(data)
}

temperature_data_dirty_processed_imputed <- preprocess_missing_data_imputation(temperature_data_dirty_processed)

data_list <- list(
  "Sensor Data" = temperature_data_dirty_processed_imputed
)
# Define the colors for each dataset
line_colors <- c('blue')
# Plot the combined graph
plot_temperature_combined(
  data_list,
  "Hourly Temperature Sensor Results With Mean Imputation",
  line_colors
)

# 1.3 Preprocessing the missing data (interpolation) #
#------------------------------------#
# Preprocessing function to handle missing data using interpolation
preprocess_missing_data_interpolation <- function(data) {
  # Impute missing values using linear interpolation
  data$temperature <- na.approx(data$temperature, x = data$datetime, na.rm = FALSE, rule = 2)
  
  return(data)
}

# Preprocess the missing data
temperature_data_dirty_processed <- preprocess_missing_data_interpolation(temperature_data_dirty_processed)

data_list <- list(
  "Sensor Data" = temperature_data_dirty_processed
)
# Define the colors for each dataset
line_colors <- c('blue')
# Plot the combined graph
plot_temperature_combined(
  data_list,
  "Hourly Temperature Sensor Results With linear interpolation",
  line_colors
)

#==========================================================#
####               (4) Dataset Comparison               ####
#==========================================================#

round(t(describe(temperature_data_dirty$temperature)), 3)
round(t(describe(temperature_data_dirty_processed$temperature)), 3)
round(t(describe(temperature_data_clean$temperature)), 3)

data_list <- list(
  "Original Data" = temperature_data_clean,
  "Processed Data" = temperature_data_dirty_processed
)

# Define the colors for each dataset
line_colors <- c('red', 'blue')

# Plot the combined graph
plot_temperature_combined(
  data_list,
  "Original and Processed Temperature Data Comparison",
  line_colors
)

