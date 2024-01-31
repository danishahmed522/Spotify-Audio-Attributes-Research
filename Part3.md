---
title: "STA302 Project part 3"
author: "Danish Ahmed Bombal"
date: "12/1/2023"
output: pdf_document
---

```
knitr::opts_chunk$set(echo = TRUE)
```


```
library(dplyr)

# Load the dataset
spotify_df <- read.csv("spotify.csv")

# Check the first few rows of the dataset
head(spotify_df)
```


```
# Display the structure of your dataset
str(spotify_df)
```

### Data Cleaning


```
# Define your variable cleaned_data based on your dataset
cleaned_data <- spotify_df

cleaned_data <- cleaned_data %>% 
  distinct(track_name, .keep_all = TRUE)

# Select the columns you want to keep
cleaned_data <- cleaned_data %>%
  select(track_name, popularity, danceability, energy, tempo, time_signature, valence, acousticness, loudness, mode, key)

# Remove observations with missing data
cleaned_data <- na.omit(cleaned_data)

# Summary Statistics and Interpretations - Danish
# Display summary statistics for your cleaned dataset
summary(cleaned_data)
```


```
# Convert the "key" column to a factor
cleaned_data$key <- as.factor(cleaned_data$key)

# Define a vector of key names
key_names <- c("C", "C♯/D♭", "D", "D♯/E♭", "E", "F", "F♯/G♭", "G", "G♯/A♭", "A", "A♯/B♭", "B")

# Create dummy variables using model.matrix
dummy_vars <- model.matrix(~ key - 1, data = cleaned_data)

# Rename the columns based on key names
colnames(dummy_vars) <- key_names

# Add the dummy variables to the original data frame
cleaned_data <- cbind(cleaned_data, dummy_vars)

# Drop the "key" column from the data frame
cleaned_data <- subset(cleaned_data, select = -key)

# View the updated data frame
head(cleaned_data)

```


```
column_names <- names(cleaned_data)
print(column_names)
```


```
library(corrplot)
corr_data <- cleaned_data %>% select(popularity, danceability, energy, tempo, time_signature, valence, acousticness, loudness, mode)
cor_matrix <- cor(corr_data)
# Plot the correlation matrix as a heatmap
corrplot(cor_matrix, method = "color")
```


```
colnames(cleaned_data)
```


```
model <- lm(popularity ~ danceability + energy + tempo + time_signature + 
              valence + acousticness + loudness + mode, data = cleaned_data)
summary(model)
```


```
model <- lm(popularity ~ . , data = cleaned_data)
summary(model)
```


```
# Fit a linear regression model
model <- lm(popularity ~ danceability + energy + tempo + time_signature + 
              valence + acousticness + loudness + mode, data = cleaned_data)

# Summarize the model
summary(model)
```


```
# Fit a linear regression model
model <- lm(popularity ~ danceability + energy + time_signature + 
              valence + acousticness + loudness + key, data = cleaned_data)

# Summarize the model
summary(model)
```

### Residual Analysis and Data Checking


```
# We use the 'model' variable above to check for the residual analysis after running the regression.

# Obtain values for fitted and residuals
fitted_values <- fitted(model)
residual_values <- resid(model)

# Creating the scatter plot for fitted vs. residuals
plot(fitted_values, residual_values, 
     main = "Spotify Tracks: Fitted Values versus Residual Values", 
     xlab = "Fitted Values", 
     ylab = "Residuals")

# Add the line y = 0 for reference
abline(h = 0, col = "blue")
```


```
# Check for normality of residuals using a QQ plot
qqnorm(residual_values)
qqline(residual_values)
```


```
# Creating the scatter plot of fitted values vs. observed popularity
plot(fitted_values, cleaned_data$popularity,
     main = "Popularity vs. Fitted Values",
     xlab = "Fitted Values",
     ylab = "Popularity")

# Add a 45-degree reference line
abline(a = 0, b = 1, col = "blue")
```

## F Tests both overall


```
model <- lm(popularity ~ as.factor(mode), data = cleaned_data)
anova_result <- anova(model)

print(anova_result)
```


```
full_model <- lm(popularity ~ as.factor(mode), data = cleaned_data)
reduced_model <- lm(popularity ~ , data = cleaned_data)  # Model with only intercept

# Perform partial F-test
partial_f_test <- anova(reduced_model, full_model)
```
