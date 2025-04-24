# ggplot.R - Data visualization using ggplot2

library(ggplot2)
library(gridExtra)

# Load data
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

# 1. Histograms for continuous variables
plot_hist <- function(data, var) {
  ggplot(data, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bins = 15, fill = "skyblue", color = "black") +
    geom_density(color = "red", size = 1) +
    ggtitle(paste("Distribution of", var)) +
    theme_minimal()
}

# Create histograms for all numeric variables
numeric_vars <- c("mpg", "disp", "hp", "drat", "wt", "qsec")
hist_plots <- lapply(numeric_vars, function(x) plot_hist(mtcars, x))
grid.arrange(grobs = hist_plots, ncol = 2)

# 2. Bar plots for categorical variables
plot_bar <- function(data, var) {
  ggplot(data, aes_string(x = var)) +
    geom_bar(fill = "lightgreen", color = "black") +
    ggtitle(paste("Distribution of", var)) +
    theme_minimal()
}

# Create bar plots for categorical variables
cat_vars <- c("cyl", "vs", "am", "gear", "carb")
bar_plots <- lapply(cat_vars, function(x) plot_bar(mtcars, x))
grid.arrange(grobs = bar_plots, ncol = 2)

# 3. Boxplot for mpg by transmission type
ggplot(mtcars, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot() +
  labs(title = "MPG by Transmission Type",
       x = "Transmission (0 = Automatic, 1 = Manual)",
       y = "Miles per Gallon") +
  theme_minimal()

# 4. Boxplot for horsepower by cylinders
ggplot(mtcars, aes(x = cyl, y = hp, fill = cyl)) +
  geom_boxplot() +
  labs(title = "Horsepower by Number of Cylinders",
       x = "Number of Cylinders",
       y = "Horsepower") +
  theme_minimal()

# 5. Correlation plot
library(corrplot)
cor_matrix <- cor(mtcars[, numeric_vars])
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7) 