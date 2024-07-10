
# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, corrplot, plotly, caTools,
              hrbrthemes)
#GGally

# Set working directory
setwd("~/Documents/GitHub/ocquarto_portfolio/posts/house_market_analysis")

# Import data files
rawdf <- list.files(pattern = "*.csv") %>% 
  lapply(read_csv) %>% 
  bind_rows()

# Data Pre-processing
df <- rawdf %>%
  as_tibble() %>%
  fill(total_bedrooms, .direction = "updown") %>%
  mutate(ocean_proximity = as.factor(ocean_proximity))

# Descriptive Statistics
summary(df)
str(df)

# Visualize Median House Value Density
ggplotly(
  df %>%
  ggplot(aes(median_house_value)) +
  stat_density(color = "darkblue", fill = "lightgray") +
  theme_minimal() +
  labs(title = "California House Value Density", 
       subtitle = "Median Housing Price",
       x = "House Value",
       y = "Density")
  )

# Correlation Analysis
corrplot(cor(select(df, -ocean_proximity)), method = "circle", na.label = "--")

# Split data into training and test sets
set.seed(123)
split <- sample.split(df$median_house_value, SplitRatio = 0.7)
housing_train <- subset(df, split == TRUE)
housing_test <- subset(df, split == FALSE)

# Build Linear Regression Model
model <- lm(median_house_value ~ ., data = housing_train)
summary(model)

# Predict on test data
housing_test$predicted_medv <- predict(model, housing_test)

# Plot Predicted vs Actual Values
ggplotly(housing_test %>%
  ggplot(aes(median_house_value, predicted_medv)) +
  geom_point(alpha = 0.1, color = "darkblue") +
  stat_smooth(color = "red") +
  labs(title = "California House Value Predicted vs. Actual", 
       subtitle = "Median Housing Price at 95% CI",
       x = "Actual Value", 
       y = "Predicted Value") +
  theme_ipsum()
)

# Calculate Root Mean Square Error (RMSE)
rmse <- sqrt(mean((housing_test$median_house_value - housing_test$predicted_medv)^2))
print(paste("Root Mean Square Error (RMSE):", rmse))

# Export the cleaned dataset
# write_csv(df, "cleaned_housing_data.csv")
# Unload all packages and clear the workspace
# pacman::p_unload(all)
# rm(list = ls())
