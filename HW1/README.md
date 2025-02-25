---
title: "readme homework1"
author: "Brunotte Wasaulue Tazi"
date: "`r Sys.Date()`"
output:
  pdf_document: default
  html_document: default
format:
  html:
    toc: true
    footer: ''
editor: visual
execute:
  echo: true
  error: true
  warning: true
  message: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Data description

Данный набор данных содержит информацию о пациентах с диабетом. Он включает в себя различные медицинские показатели, такие как уровень глюкозы(Glucose), индекс массы тела (BMI), уровень инсулина(Insulin) и другие. **Целевая переменная — Outcome**, которая указывает, есть ли у пациента диабет (1) или нет (0).

## Attribute list - Cписок атрибутов набора данных:

| Атрибут | Описание |
|-------------------------|----------------------------------------------|
| Беременности (Pregnancies) | Для выражения количества беременностей |
| Глюкоза (Glucose) | Для выражения уровня глюкозы в крови |
| Артериальное давление (BloodPressure) | Для выражения измерения артериального давления |
| Толщина кожи (SkinThickness) | Для выражения толщины кожи |
| Инсулин (Insulin) | Для выражения уровня инсулина в крови |
| ИМТ (BMI) | Для выражения индекса массы тела |
| Функция определения диабета | Для выражения процента диабета |
| Возраст (Age) | Для выражения возраста |
| Результат (Outcome) | Для выражения конечного результата (1 означает "Да", 0 - "Нет") |

# Import data set and packages

```{r}
library(tidyverse)
library(ggplot2)
df <- read.csv("data/diabetes.csv")
```

```{r}
print(df)
```

# Statistics

## Check null or Na values

```{r}
#Проверка на наличие пропущенных значений
na_count_df <- colSums(is.na(df))
print(na_count_df)
```

Здесь мы подсчитываем количество пропущенных значений в каждом столбце. Это поможет нам понять, есть ли необходимость в обработке пропусков. **в датасете нет пропущенных значений**

## Get summary statistics

```{r}
glimpse(df)
```

```{r}
summary(df)
```

Функция `glimpse` предоставляет краткий обзор структуры данных, включая типы переменных и их значения. Функция `summary` выводит основные статистические характеристики для каждого столбца.

## Group by 'OUTCOME' column and calculate summary statistics

```{r}
result <- df %>%
  group_by(Outcome) %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        median = ~ median(.x, na.rm = TRUE),
                        min = ~ min(.x, na.rm = TRUE),
                        max = ~ max(.x, na.rm = TRUE))))
print(result)
```

Здесь мы группируем данные по переменной `Outcome` и вычисляем `среднее, медиану, минимум и максимум для всех числовых переменных`. Это позволяет сравнить статистические характеристики между группами.

# EDA

## Count plot for the 'Outcome' column

```{r}
ggplot(df, aes(x = factor(Outcome))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Count of people having Diabete",
       x = "Outcome (0 = No, 1 = Yes)",
       y = "Count") +
  theme_minimal()
```
![unnamed-chunk-7-1](https://github.com/user-attachments/assets/57871132-2caa-4981-8b06-79e5db9e1d44)

Этот график показывает количество людей с диабетом и без него, что позволяет визуально оценить распределение. Здесь мы видим, что в нашем наборе данных у многих людей нет диабета.Но эти два класса показывают некоторый дисбаланс в данных. Для дальнейших манипуляций необходимо обработать этот дисбаланс, чтобы избежать ошибочных результатов.

## Boxplot of Glucose levels by Outcome

```{r}
ggplot(df, aes(x = factor(Outcome), y = Glucose, fill = factor(Outcome))) +
  geom_boxplot() +
  labs(title = "Boxplot of Glucose Levels by Diabetes Outcome",
       x = "Diabetes Outcome (0 = Non-Diabetic, 1 = Diabetic)",
       y = "Glucose Level") +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme_minimal()
```

Ящик с усами позволяет увидеть распределение уровней глюкозы для диабетиков и не диабетиков, а также выявить выбросы.Здесь мы можем сделать вывод, что для людей, не страдающих сахарным диабетом, среднее значение немного превышает 100 (точно 109,9800, значение из функции summary), а для диабетиков - меньше 150 (точно 141,2575, значение из функции summary).

```{r}
ggplot(df, aes(x = factor(Outcome), y = Age, fill = factor(Outcome))) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Diabetes Outcome",
       x = "Diabetes Outcome (0 = Non-Diabetic, 1 = Diabetic)",
       y = "Age") +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  theme_minimal()
```
![unnamed-chunk-9-1](https://github.com/user-attachments/assets/f082d297-d59f-4e07-8392-6b4151041e8b)

Этот график показывает распределение возраста пациентов в зависимости от наличия диабета.

## Histogram of BMI

```{r}
ggplot(df, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of BMI",
       x = "Body Mass Index (BMI)",
       y = "Frequency") +
  theme_minimal()
```
![unnamed-chunk-10-1](https://github.com/user-attachments/assets/fcc3460f-7307-45ae-b851-9d4e3dbbe240)

Гистограмма показывает распределение индекса массы тела (ИМТ) среди пациентов. Это позволяет увидеть, как много людей находятся в различных диапазонах ИМТ. Что касается нашего набора данных, то у большинства людей индекс массы тела от 30 до 37 (приблизительно)

## Scatter plot of BMI vs. Glucose

```{r}
#Scatter plot of BMI vs. Glucose
ggplot(df, aes(x = BMI, y = Glucose, color = Outcome)) +
  geom_point() +
  labs(title = "Scatter Plot of BMI vs. Glucose",
       x = "\nBody Mass Index (BMI)\n\nOutcome: (1= have diabete, 0= doesn't have diabete)",
       y = "Glucose Level") +
  theme_minimal()
```
![unnamed-chunk-11-1](https://github.com/user-attachments/assets/cac35642-176d-4a83-a991-f351be6e461f)

Диаграмма рассеяния показывает взаимосвязь между ИМТ и уровнем глюкозы в крови. Цвет точек указывает на наличие диабета (1 = диабетик, 0 = не диабетик). Это позволяет визуально оценить, как ИМТ влияет на уровень глюкозы. Мы можем ясно видеть, что `наиболее высоким является индекс массы тела, уровень глюкозы также имеет тенденцию быть высоким`, что приводит к высокой вероятности развития диабета. Тот же результат можно наблюдать на следующем графике, используя соотношение между глюкозой и инсулином.

```{r}
# Scatter plot of Insulin vs. Glucose
ggplot(df, aes(x = Insulin, y = Glucose, color = Outcome)) +
  geom_point() +
  labs(title = "Scatter Plot of Insulin  vs. Glucose",
       x = "\nInsulin \n\nOutcome: (1= have diabete, 0= doesn't have diabete)",
       y = "Glucose Level") +
  theme_minimal()
```
![unnamed-chunk-12-1](https://github.com/user-attachments/assets/3ff0fc11-87e6-49ee-9f77-30f09644b53c)

## Pair plot of selected numeric variables

Парная диаграмма позволяет визуализировать взаимосвязи между несколькими числовыми переменными. Это может помочь выявить корреляции между этими переменными.

```{r}
ggpairs(df[, c("Glucose", "BMI", "Insulin")])
```

```{r}
ggpairs(df[, c("BloodPressure", "DiabetesPedigreeFunction", "Pregnancies")])
```
