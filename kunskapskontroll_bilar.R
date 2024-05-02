# Installing packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("MASS")
install.packages("leaps")
install.packages("car")
install.packages("Metrics")
install.packages("rsample")
install.packages("corrplot")
install.packages("caret")
install.packages("httr2")

update.packages()

# Loading packages
library("dplyr")
library("tidyr")
library("ggplot2")
library("readxl")
library("MASS")  
library("leaps") 
library("car")   
library("Metrics")
library("rsample")
library("corrplot")
library("caret")
library("httr2")


# EDA--------------------------------------------------------------------

cars <- read.csv("/Users/natalied/Desktop/EC/06.R_programmering/Kunskapskontroll/skrapning_v3_cleaned.csv")

# Exploring the data

dim(cars)
head(cars)
str(cars)
summary(cars)


# Choosing only electric cars from data set (Fuel = EL)
# Dropping some columns that won´t be used
# Factorizing categorical data 

elcars <- cars %>%
  filter(Fuel == "El") %>%
  dplyr::select(-`X`, -`Company`, -`Fuel`, -`Gear`, -`Name`, -`Engine.Volume`, -`Horsepower`, -`Model`) %>%
  mutate(
    Brand = as.factor(Brand),
    Location = as.factor(Location)
  )

# Checking if it went well

head(elcars)
str(elcars)
nrow(elcars)
summary(elcars)


# Checking all of brands in Brand-column and the total quantity of each

brand_chart <- elcars %>%
  count(Brand, sort = TRUE)


print(brand_chart)

# Creating a diagram over the results

ggplot(brand_chart, aes(x = Brand, y = n, fill = Brand)) +
  geom_col(show.legend = FALSE) +  
  labs(title = "Frequence of unique values in column",
       x = "Unique values", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Sorting the results from biggest amount to lowest 
# choosing top 10 brands
# and creating a diagram over results

top10_brands <- brand_chart %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

ggplot(top10_brands, aes(x = reorder(Brand, -n), y = n, fill = Brand)) +
  geom_col(show.legend = FALSE) +  
  labs(title = "Frequency of Unique Values in Column",
       x = "Unique Values", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Plotting top 10 brands to see relations


# Extracting the top 10 brand names
top10_brand_names <- top10_brands$Brand  

# Filtering elcars with only top 10 brands

elcars_top10 <- elcars %>%
  filter(Brand %in% top10_brand_names)

head(elcars_top10)
str(elcars_top10)
nrow(elcars_top10)
summary(elcars_top10)

# Histogram of top 10 brands and year of manufacture

ggplot(elcars_top10, aes(x = Year)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  facet_wrap(~ Brand) +
  labs(title = "Distribution of Year by Top 10 Brands",
       x = "Year", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram of top 10 brands and year of manufacture

ggplot(elcars_top10, aes(x = Year)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  labs(title = "Distribution of Manufacturing Year for Top 10 Brands",
       x = "Manufacturing Year", y = "Frequency") +
  scale_x_continuous(breaks = seq(min(elcars_top10$Year), max(elcars_top10$Year), by = 1),
                     labels = seq(min(elcars_top10$Year), max(elcars_top10$Year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot showing relation between Miles and Price for top 10 brands

ggplot(elcars_top10, aes(x = Miles, y = Price)) +
  geom_point(aes(color = Brand), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relationship Between Mileage and Price",
       x = "Mileage (Miles)", y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot showing relation between Year and Price for top 10 brands

ggplot(elcars_top10, aes(x = Year, y = Price, color = Brand)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_continuous(breaks = seq(min(elcars_top10$Year), max(elcars_top10$Year), by = 1),
                     labels = seq(min(elcars_top10$Year), max(elcars_top10$Year), by = 1)) +
  labs(title = "Relationship Between Year and Price for Top 10 Brands",
       x = "Manufacturing Year", y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Boxgram over brands and prices

ggplot(elcars_top10, aes(x = Brand, y = Price, fill = Brand)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Brand for Top 10 Most Frequent Brands",
       x = "Brand", y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Scatter plot

ggplot(elcars_top10, aes(x = Brand, y = Price, color = Brand)) +
  geom_point(alpha = 0.6) +
  labs(title = "Price vs. Brand for Top 10 Most Frequent Brands",
       x = "Brand", y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Looks like we have outliers
# Calculating the first and third quartiles

Q1 <- quantile(elcars_top10$Price, 0.25)
Q3 <- quantile(elcars_top10$Price, 0.75)
IQR <- Q3 - Q1

# Defining the limits for outliers

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filtering out outliers

elcars_top10_cleaned <- elcars_top10 %>% 
  filter(Price >= lower_bound & Price <= upper_bound)

# Creating a new boxplot to display the cleaned data

ggplot(elcars_top10_cleaned, aes(x = Brand, y = Price, fill = Brand)) +
  geom_boxplot() +
  labs(x = "Brand", y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Cleaning data from lower bound outliers/extreme values
# Choosing to set a minimum acceptable price threshold 
# based on domain knowledge or observation

price_threshold <- 10000

# Filtering out cars priced below the threshold
# and checking if it looks good

elcars_top10_newcleaned <- elcars_top10_cleaned %>%
  filter(Price >= price_threshold)

summary(elcars_top10_newcleaned$Price)


ggplot(elcars_top10_newcleaned, aes(x = Brand, y = Price, fill = Brand)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Histogram of selling prices. 

hist(elcars_top10_newcleaned$Price, col="darkblue")

ggplot(elcars_top10, aes(x = Price)) + geom_histogram(bins = 30, fill = "blue")
ggplot(elcars_top10_cleaned, aes(x = Price)) + geom_histogram(bins = 30, fill = "blue")
ggplot(elcars_top10_newcleaned, aes(x = Price)) + geom_histogram(bins = 30, fill = "blue")


# Fixing logarithmic transformation for stabilizing
# the variance and normalize the distribution of a variable

#elcars_top10_newcleaned$log_Price <- log(elcars_top10_newcleaned)

summary(elcars_top10_newcleaned)


###### Multiple linear regression analysis

install.packages("caret")

library(caret)

# Spitting the data into test and train set

set.seed(123)
split <- createDataPartition(elcars_top10_newcleaned$Price, p = 0.80, list = FALSE)
training_set <- elcars_top10_newcleaned[split,]
test_set <- elcars_top10_newcleaned[-split,]


# Training multiple linear regressionmodel

model <- lm(Price ~ Year + Miles + Brand + Location, data = training_set)
summary(model)


# Model evaluation

# Using the test data to make predictions

predictions <- predict(model, newdata = test_set)

# (exponentiation to the predicted logarithmic values)
# so the RMSE and R-squared can show relevant results

predicted_prices <- predict(model, newdata = test_set)
#predicted_prices <- exp(predicted_log_prices)

#predicted_log_prices <- predict(model, newdata = test_set)
#predicted_prices <- exp(predicted_log_prices)

rmse_value <- rmse(test_set$Price, predicted_prices)
r2_value <- R2(test_set$Price, predicted_prices)

print(paste("RMSE:", rmse_value))
print(paste("R-squared:", r2_value))

summary(elcars_top10_newcleaned)

# Testing for Multicollinearity

vif(model)

# Residuals vs predicted values to look for patterns

plot(predict(model), residuals(model), main="Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")

#Scale-Location plot to see heteroskedasticity

plot(predict(model), sqrt(abs(residuals(model))), main="Scale-Location", xlab="Fitted values", ylab="sqrt(|Residuals|)")
abline(h=0, col="red")

# QQ-plot to see outliers

qqnorm(residuals(model))
qqline(residuals(model), col="red")

# Cleaning outliers
# Determining quantile limits for (log_price)/Price

lower_bound <- quantile(elcars_top10_newcleaned$Price, probs = 0.05)
upper_bound <- quantile(elcars_top10_newcleaned$Price, probs = 0.95)


trimmed_data <- subset(elcars_top10_newcleaned, Price >= lower_bound & Price <= upper_bound)


# (Handling heteroskedasticity by using square root transformation of log_Price)
# trimmed data$sqrt (log_) Price <- sqrt(trimmed_data$log_Price)

# Splitting up trimmed data into training and test sets

set.seed(123)
split <- createDataPartition(trimmed_data$Price, p = 0.80, list = FALSE)
training_set_trimmed <- trimmed_data[split, ]
test_set_trimmed <- trimmed_data[-split, ]

# Training a new regression model on the trimmed training data 
# (with the transformed variable)

model_trimmed <- lm(Price ~ Year + Miles + Brand + Location, data = training_set_trimmed)

# Summarizing the model to see if there is improvement

summary(model_trimmed)

# Making predictions on the new test set and 
# (transforming the predictions back to the original price)

predicted_prices <- predict(model_trimmed, newdata = test_set_trimmed)
#predicted_log_prices <- predicted_sqrt_log_prices^2  
#predicted_prices <- exp(predicted_log_prices)


# Calculating the RMSE and R^2 of the back-transformed prices

rmse_value <- rmse(test_set_trimmed$Price, predicted_prices)
r2_value <- R2(test_set_trimmed$Price, predicted_prices)

# Printing the new performance metrics

print(paste("RMSE after back-transformation:", rmse_value))
print(paste("R-squared after back-transformation:", r2_value))

# Testing for Multicollinearity

vif(model_trimmed)



#### Best Subset Regression

# Set seed for reproducibility

set.seed(123)

# Splitting data into training and testing sets

split <- createDataPartition(elcars_top10_newcleaned$Price, p = 0.80, list = FALSE)
training_set <- elcars_top10_newcleaned[split,]
test_set <- elcars_top10_newcleaned[-split,]

# Performing best subset regression 

best_subset_model <- regsubsets(Price ~ Year + Miles + Brand + Location, data = training_set, nvmax = 13, really.big = TRUE)
best_model_summary <- summary(best_subset_model)

# Selecting the best model based on adjusted R-squared

best_vars <- names(coef(best_subset_model, which.max(best_model_summary$adjr2)))
available_vars <- intersect(best_vars, names(training_set))

# Creating the model formula from the best variables

formula <- as.formula(paste("Price ~", paste(available_vars[-1], collapse=" + ")))

# Fitting the final model on training data

final_model <- lm(formula, data = training_set)

# Diagnostic plots

par(mfrow = c(2, 2))
plot(final_model)

# Predicting on test set and transforming predictions back

predictions <- predict(final_model, newdata = test_set)
#predictions <- exp(predictions_log)  # Transform back from log


rmse_value <- sqrt(mean((test_set$Price - predictions)^2))
test_r_squared <- cor(test_set$Price, predictions)^2


results <- data.frame(
  Model = "Final Model",
  RMSE = rmse_value,
  Adj_R_squared = summary(final_model)$adj.r.squared,
  Test_R_squared = test_r_squared,
  BIC = BIC(final_model)
)


print(results)

# Exploring problems with model prediction because of the weaker results

# Calculating variance inflation factors (VIF)

vif_values <- vif(final_model)


print(vif_values)


#### Lasso model

install.packages("glmnet")
library(glmnet)


X <- model.matrix(Price ~ Year + Miles + Brand + Location, data = elcars_top10_newcleaned)[,-1] # Första kolumnen undantas eftersom den är intercept
y <- elcars_top10_newcleaned$Price


lasso_model <- glmnet(X, y, alpha = 1)

# Choosing lambda value that minimizes the cross-validation error

cv_model <- cv.glmnet(X, y, alpha = 1)
plot(cv_model)
best_lambda <- cv_model$lambda.min

print(paste("Best lambda:", best_lambda))


predicted_prices <- predict(lasso_model, s = best_lambda, newx = model.matrix(Price ~ Year + Miles + Brand + Location, data = elcars_top10_newcleaned)[,-1])


rmse_value <- sqrt(mean((elcars_top10_newcleaned$Price - predicted_prices)^2))
r2_value <- 1 - sum((elcars_top10_newcleaned$Price - predicted_prices)^2) / sum((elcars_top10_newcleaned$Price - mean(elcars_top10_newcleaned$Price))^2)


print(paste("RMSE:", rmse_value))
print(paste("R-squared:", r2_value))

