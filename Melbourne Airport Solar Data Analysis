# -------------------------------------------------------------
# Project: Melbourne Airport Solar Data Analysis
# Author: Syed Anfas Ahmed
# Purpose: Exploratory Data Analysis and Clustering
# -------------------------------------------------------------

# 1.1 Loading the Data
# -------------------------------------------------------------
# Load the dataset containing solar and environmental data
the_data <- read.csv("MelbAirportSolarData.csv", header = TRUE, sep = ",")

# Sample the data to work with a smaller subset (5000 rows)
set.seed(123)  # Ensures reproducibility of sampling
sampled_indices <- sample(1:nrow(the_data), 5000)
my.data <- the_data[sampled_indices, ]

# Convert sampled data to a matrix
my.data_mat <- as.matrix(my.data)

# Save the sampled data to a text file for reference
write.table(my.data, "Syed-Anfas-Ahmed-MelbAirSolarMyData.txt")


# 1.2 Wind Speed Analysis
# -------------------------------------------------------------
# Extract the 'Wind speed' column (assumed to be in the 3rd column)
speed_wind <- as.numeric(my.data_mat[, 3])

# Plot a histogram to visualize the distribution of wind speed
hist(
  speed_wind,
  main = "Wind Speed Histogram",
  xlab = "Speed of Wind in (m/s)",
  col = "lightgreen",
  border = "white"
)

# Generate a box plot to summarize the spread of wind speed data
boxplot(
  speed_wind,
  horizontal = TRUE,
  main = "Box Plot of Wind Speed",
  col = "lightgrey"
)

# Calculate and print the five-number summary (min, 1st quartile, median, 3rd quartile, max)
five_num_summary <- fivenum(speed_wind)
print(five_num_summary)

# Generate a full summary of wind speed data (mean, quartiles, etc.)
summary_windspeed <- summary(speed_wind)
print(summary_windspeed)


# 1.3 Temperature vs Humidity Analysis
# -------------------------------------------------------------
# Extract 'Temperature' (assumed 2nd column) and 'Humidity' (assumed 4th column)
temp_humidity <- my.data[, c(2, 4)]

# Plot a scatter plot to show the relationship between temperature and humidity
plot(
  temp_humidity[, 1], temp_humidity[, 2],
  xlab = "Temperature (°C)", ylab = "Humidity (%)",
  main = "Temperature vs Humidity"
)

# Fit a linear regression model: Humidity ~ Temperature
regression_model <- lm(temp_humidity[, 2] ~ temp_humidity[, 1])

# Add the regression line to the scatter plot
abline(regression_model, col = "red")

# Print the linear regression equation
intercept <- coef(regression_model)[1]
slope <- coef(regression_model)[2]
cat("Linear regression equation: Humidity = ", round(intercept, 2), " + ", round(slope, 2), "*Temperature\n")

# Calculate and print the correlation coefficient between temperature and humidity
correlation <- cor(temp_humidity[, 1], temp_humidity[, 2])
cat("Correlation coefficient: ", round(correlation, 2), "\n")

# Calculate and print the R-squared value for the regression model (goodness of fit)
r_squared <- summary(regression_model)$r.squared
cat("Coefficient of Determination (R²): ", round(r_squared, 2), "\n")


# 2.0 Categorical Data Binning and Probability Calculations
# -------------------------------------------------------------
wind_speed_cat <- cut(speed_wind, breaks = c(0, 2, 5, 10, 15), labels = c("Low", "Moderate", "High", "Very High"))
temp_cat <- cut(my.data[, 2], breaks = c(-10, 10, 20, 30, 40), labels = c("Cold", "Mild", "Warm", "Hot"))
irradiance_cat <- cut(my.data[, 5], breaks = c(0, 300, 600, 900), labels = c("Low", "Medium", "High"))

# Create a contingency table to count occurrences of each combination of categories
contingency_table <- table(wind_speed_cat, temp_cat, irradiance_cat)
print(contingency_table)

# Calculate conditional probabilities and mutual exclusivity
# Conditional probability: P(wind speed is High | temperature is Warm)
prob_wind_given_temp <- prop.table(contingency_table, margin = 2)  # Margin 2 conditions on temperature
cat("P(High wind speed | Warm temperature):", prob_wind_given_temp["High", "Warm"], "\n")


# 3.0 Bayesian Analysis and Prior/Posterior Distributions
# -------------------------------------------------------------
# Define prior distribution for some height data
prior_mean <- 170  # Mean of prior
prior_sd <- 15     # Standard deviation of prior

# Generate a sequence of height values for plotting
height_values <- seq(140, 200, by = 1)

# Compute prior density values for each height
prior_density <- dnorm(height_values, mean = prior_mean, sd = prior_sd)

# Plot prior distribution using ggplot2
library(ggplot2)
ggplot(data.frame(height = height_values, Prior = prior_density), aes(x = height, y = Prior)) +
  geom_line(color = "orange") +
  labs(title = "Prior Distribution of Height", x = expression(theta), y = "Density") +
  theme_minimal()

# Define likelihood function based on observed data
observed_mean <- 160  # Mean of observed data
likelihood_sd <- 10    # Standard deviation of observed data
likelihood_density <- function(height) {
  dnorm(height, mean = observed_mean, sd = likelihood_sd)
}

# Compute likelihood values
likelihood_values <- likelihood_density(height_values)

# Plot likelihood distribution
ggplot(data.frame(height = height_values, Likelihood = likelihood_values), aes(x = height, y = Likelihood)) +
  geom_line(color = "lightgreen") +
  labs(title = "Likelihood Distribution of Height", x = expression(theta), y = "Density") +
  theme_minimal()


# 5.0 Clustering Analysis
# -------------------------------------------------------------
# Load data from 'lettersdata.txt' 
zz <- read.table("lettersdata.txt")
zz <- as.matrix(zz)

# Perform K-means clustering with 4 clusters
set.seed(42)
k_val <- 4
km_result <- kmeans(zz, centers = k_val)

# Plot the K-means clustering results
plot(zz, col = km_result$cluster, main = "K-Means Clustering", xlab = "Vertical Dimension", ylab = "Horizontal Dimension", pch = 19)
points(km_result$centers, col = 3:k_val, pch = 8, cex = 2)

# Elbow Method: Plot TOTWSS (Total Within-Cluster Sum of Squares) vs Number of Clusters
k_values <- 2:20
get_totwss <- function(k) {
  set.seed(42)
  km_result <- kmeans(zz, centers = k)
  return(km_result$tot.withinss)
}
totwss_values <- sapply(k_values, get_totwss)

plot(k_values, totwss_values, type = "b", col = "red", main = "TOTWSS vs Number of Clusters", xlab = "Number of Clusters", ylab = "TOTWSS", pch = 19)

# Perform spectral clustering
library(kernlab)
set.seed(42)
spcial <- specc(zz, centers = 4)
spcl_clusters <- as.numeric(spcial)

# Plot the spectral clustering results
plot(zz, col = spcl_clusters, main = "Spectral Clustering Results", xlab = "Dimension 1", ylab = "Dimension 2", pch = 19)

# End of project
