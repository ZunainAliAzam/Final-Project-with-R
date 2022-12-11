---
title: "BSCS_306_Practical"
author: "Zunain Ali Azam"
date: "21/01/2022"
output: word_document
---

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this

## 1. Input data into R #
# --------------------
```{r}
library(datasets)

data("cars")

# First several rows of dataset. 
head(cars)
# Last several rows of dataset. 
tail(cars)

# Dimension of dataset.
dim(cars) 
# Total number of rows. 
nrow(cars)
# Variables are Gender, Height, Weight and Index.
ncol(cars)
```

## 2. Data Summaries/ Description
# ------------------------------

```{r}
# Giving a summarized view of dataset.
summary(cars)              

library(psych)
# Describes whole dataset.
describe(cars)
# Describing one quantitative variable.
describe(cars$speed)

```

## 3. Data Plots/ Visualization
# ----------------------------

```{r}
# BOX PLOTS
boxplot(dist~speed, data = cars, col  = "thistle4",
        main = "Box Plot Cars: Distance and Speed",
        xlab = "Distance", ylab="Speed")

# SCATTER PLOTS
plot(cars)
plot(cars$dist, cars$speed,
     col  = "#140332",
     pch  = 20,
     cex  = 1.5,
     main = "Scatter plot Cars: Distance and Speed", 
     xlab = "Distance",
     ylab = "Speed")

# HISTOGRAMS

hist(cars$speed,
     col    = "#ffd966",              
     xlim   = c(0,30),
     breaks = 20,
     main   = "Histogram: Speed of Cars",
     xlab   = "Speed)")
hist(cars$dist ,
     col    = "#ffd966", 
     xlim   = c(0,100),
     breaks = 20,
     main   = "Histogram: Distance attained by Cars",
     xlab   = "Distance")

# TABLE CREATION OF SPEED OF CARS 
Speed_tab <- table(cars$speed)                      

# Bar Charts
barplot(Speed_tab,
        main = "Bar Chart: Speed of Cars", 
        col= "#134f5c", 
        xlab = "Speed", ylab = "Frequency")          # Draws a bar chart.

# Line Charts
plot(Speed_tab, main = "Line Chart: Speed of Cars", 
        col= "#134f5c",
        xlab = "Speed", ylab = "Frequency")          # Draws a line chart.

# TABLE CREATION OF DISTANCE OF CARS 
Distance_tab <- table(cars$dist)                      

# Bar Charts
barplot(Distance_tab,
        main = "Bar Chart: Distance of Cars", 
        col= "#134f5c", 
        xlab = "Distance", ylab = "Frequency")          # Draws a bar chart.

# Line Charts
plot(Distance_tab, main = "Line Chart: Distance of Cars", 
        col= "#134f5c",
        xlab = "Distance", ylab = "Frequency")          # Draws a line chart.
```

## 4. Correlation
# --------------

```{r}
cor(cars)

library(corrplot)
cor.mat.cars <- cor(cars)
corrplot(cor.mat.cars, order = "AOE")
# From the plot, it is visible that there is a positive correlation and from the table it can be seen that speed and distance are close to each other.
cor(cars$speed, cars$dist)
# Since sign is positive so if one get large other gets larger and since it is more than 0.7. Therefore, we can say it is strongly correlated. 
```
# 5. Confidence Interval
# ----------------------

```{r}
library(Rmisc)

# Calculating the confidence interval of a vector of data

CI(cars$speed, ci = 0.95)
# The mean is significantly greater than zero.

CI(cars$dist, ci = 0.95)
# The mean is significantly greater than zero.

```
# 6. Hypothesis Testing:
# ---------------------

```{r}
library(stats)

# One sample t-test
t.test(cars$speed, mu = 200)
# To Test: Is the mean value of Murder differ from 20 or not?
# Answer: This is a two tail test. Mean value of speed differs from 200 and p value is less than 0.05. Therefore, null hypothesis is rejected.

t.test(cars$speed, mu = 200, alternative = "greater")
# To Test: If you want to test on sided alternative.
# Answer: This is a one tail test. Mean value of speed is greater than 200 and  p value is greater than 0.05. Therefore, we fail to reject the null hypothesis.

# Two sample t-test for two independent samples
# ----------------------------------------------
x <- rnorm(cars$speed)
y <- rnorm(cars$dist)
t.test(x,y, var.equal = TRUE)   # Addition of var.equal = TRUE
# Answer: This is a two tail test. Mean value of x and y is not equal to zero and p value is greater than 0.05. Therefore, we reject the null hypothesis.
```

# 7. Chi-Square test:
# ------------------

```{r}

# Apply the Chi-Square test to see test of association/ independence.
chisq.test(cars$speed, cars$dist)

# We have x-squared = 636.94, Since we get a p-Value greater than the significance level of 0.05, we accept the null hypothesis and conclude that the two variables are independent.
```

# 8. Analysis Of Variance:
# -----------------------
```{r}
# ONE WAY ANOVA
# Question: Do the speed of cars affect the distance? 
boxplot(cars$speed ~ cars$dist, col= "#88C4C9", 
        main = "Box Plot: Affect of Speed on Distance of cars")
model1 <- aov(cars$speed ~ cars$dist)
summary(model1)

#ANSWER: It is observed that the F-statistic value is 89.57 and it is more significant as the corresponding p-value is  greater. Thus, it is wise to reject the null hypothesis of speed. In other words, the speed does not affect distance.

library(gplots)
plotmeans(cars$speed ~ cars$dist, main="Mean Plot with 95% Confidence Interval", ylab = "Speed", xlab = "Distance")

```

## 9. Linear and Multiple Regression Models:
```{r}
# Linear Regression line formula:
# -------------------------------

# This will give details of the model including the 
# correlation, parameters (intercept and slope) along with P-value and Mean sum of squares.  
# You need to write comments on these three at least.

attach(cars)

fit.LR <- lm(speed ~ dist, data = cars)
summary(fit.LR)
# Answer: Firstly,P value is less than 0.05 which mean intercept is significant while in assault we see that p value is greater than 0.05 so it will not be accepted so in other word we can say that assault has not much significance impact on the Speed. Secondly, signs are positive which shows if one increases other greatly increases as well ,In Multiple R-squared is 0.6511 so for correlation we do square root of it so answer 0.4239 is which is positive so there is some correlation with distance and speed. Thirdly, the p-value in dist and after f statistic is same which indicate we have a linear regression model i.e one x only i.e one intercept.

# PLOTS:
scatter.smooth(x=speed, y = dist, main="Speed ~ Distance")

require(ggplot2)
ggplot(cars,aes(y=speed,x=dist))+geom_point()+geom_smooth(method="lm")

# Plots show us:
#1) Linearity:
            #The relationship between the independent and dependent variable must be linear. We can test this visually with a scatter plot to see if the distribution of data points could be described with a straight line.

#2) Independence of observations:
            #Because we only have one independent variable and one dependent variable from the given data result so, we don’t need to test for any hidden relationships among variables.

#3) Normality:
            #using the hist function we find from the above data the whether dependent variable follows normal distribution

# Multiple Regression line formula:
# --------------------------------

fit.MR <- lm(speed~ dist, data = cars)
summary(fit.MR)

# Answer: P-value is very low in intercept and it has a positive sign, so there is a positive correlation in it. Taking the square root of Multiple R-squared: 0.6511, we get 0.423. Also, the value of P in distance is least significant, i.e. 0.01, than the intercept p value, i.e. 0

# beta0 = intercept of the regression line is 8.28391.
# beta1 = slope of the first independent variable 0.16557.

# Plot:
attach(cars)
library(dplyr)
library(ggplot2)

cars %>%
  filter(speed < 50)%>%
  ggplot(aes(x=speed, y= dist, col = speed))+ geom_point(alpha = 1)+
  geom_smooth(method = lm)

cars %>%
  filter(dist < 100)%>%
  ggplot(aes(x=dist, y= speed, col = dist))+ geom_point(alpha = 1)+
  geom_smooth(method = lm)

## Applying test on the coefficient
# -------------------------------
# t-test Formula for Intercept:

# t-test Formula for Slope:

# Is the overall regression model suitable?
# ---------------------------------------
anova(fit.LR)  # Test difference in slopes (joint F-test)
# Answer: Since F-statistic value is greater than the p-value. Therefore, we can say that by judging f-value result is good.
```

