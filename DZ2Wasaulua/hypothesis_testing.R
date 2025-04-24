# hypothesis_testing.R - Statistical hypothesis testing

library(car)

# Load data
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# 1. Test normality of mpg for automatic vs manual transmission
shapiro_auto <- shapiro.test(mtcars$mpg[mtcars$am == 0])
shapiro_manual <- shapiro.test(mtcars$mpg[mtcars$am == 1])

cat("Shapiro-Wilk test for automatic transmission (am=0): p-value =", shapiro_auto$p.value, "\n")
cat("Shapiro-Wilk test for manual transmission (am=1): p-value =", shapiro_manual$p.value, "\n")

# 2. T-test for mpg by transmission type
t_test <- t.test(mpg ~ am, data = mtcars, var.equal = FALSE)
print(t_test)

# 3. Test normality of hp by cylinders
normality <- by(mtcars$hp, mtcars$cyl, shapiro.test)
print("Normality tests by cylinder group:")
print(normality)

# 4. Test homogeneity of variances
levene_test <- leveneTest(hp ~ cyl, data = mtcars)
print(levene_test)

# 5. Kruskal-Wallis test for hp by cylinders
kruskal_test <- kruskal.test(hp ~ cyl, data = mtcars)
print(kruskal_test)

# 6. Post-hoc Dunn test
library(DescTools)
dunn_test <- DunnTest(hp ~ cyl, data = mtcars, method = "bonferroni")
print(dunn_test)

# 7. Correlation test between mpg and weight
cor_test <- cor.test(mtcars$mpg, mtcars$wt)
print(cor_test)

# 8. Chi-square test for transmission type by cylinders
contingency_table <- table(mtcars$am, mtcars$cyl)
chi_test <- chisq.test(contingency_table)
print(chi_test)