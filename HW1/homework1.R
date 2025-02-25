# Import dataset and packages
  
library(tidyverse)
library(ggplot2)
df <- read.csv("data/diabetes.csv")
print(df)

# Statistics

## Check null or na values

#Проверка на наличие пропущенных значений
na_count_df <- colSums(is.na(df))
print(na_count_df)

## Get summary statistics
  
glimpse(df)
summary(df)

## Group by 'OUTCOME' column and calculate summary statistics

result <- df %>%
  group_by(Outcome) %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        median = ~ median(.x, na.rm = TRUE),
                        min = ~ min(.x, na.rm = TRUE),
                        max = ~ max(.x, na.rm = TRUE))))
print(result)

# EDA
## Count plot for the 'Outcome' column

ggplot(df, aes(x = factor(Outcome))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Count of people having Diabete",
       x = "Outcome (0 = No, 1 = Yes)",
       y = "Count") +
  theme_minimal()

## Boxplot of Glucose levels by Outcome

ggplot(df, aes(x = factor(Outcome), y = Glucose, fill = factor(Outcome))) +
  geom_boxplot() +
  labs(title = "Boxplot of Glucose Levels by Diabetes Outcome",
       x = "Diabetes Outcome (0 = Non-Diabetic, 1 = Diabetic)",
       y = "Glucose Level") +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme_minimal()

## Boxplot of Age by Outcome
ggplot(df, aes(x = factor(Outcome), y = Age, fill = factor(Outcome))) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Diabetes Outcome",
       x = "Diabetes Outcome (0 = Non-Diabetic, 1 = Diabetic)",
       y = "Age") +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme_minimal()

## Histogram of BMI

ggplot(df, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of BMI",
       x = "Body Mass Index (BMI)",
       y = "Frequency") +
  theme_minimal()

##Scatter plot of BMI vs. Glucose

#Scatter plot of BMI vs. Glucose
ggplot(df, aes(x = BMI, y = Glucose, color = Outcome)) +
  geom_point() +
  labs(title = "Scatter Plot of BMI vs. Glucose",
       x = "\nBody Mass Index (BMI)\n\nOutcome: (1= have diabete, 0= doesn't have diabete)",
       y = "Glucose Level") +
  theme_minimal()

# Scatter plot of Insulin vs. Glucose
ggplot(df, aes(x = Insulin, y = Glucose, color = Outcome)) +
  geom_point() +
  labs(title = "Scatter Plot of Insulin  vs. Glucose",
       x = "\nInsulin \n\nOutcome: (1= have diabete, 0= doesn't have diabete)",
       y = "Glucose Level") +
  theme_minimal()

## Pair plot of selected numeric variables

ggpairs(df[, c("Glucose", "BMI", "Insulin")])

ggpairs(df[, c("BloodPressure", "DiabetesPedigreeFunction", "Pregnancies")])

