# Online Academic Data Analysis Bootcamp Using Open-Access Program R: Essentials. Session 5, Parametric Statistics: Exploring Assumptions
# Patrick Njage
# March 17, 2022
# Exercise 1 Read in R exam data.
# Set the variable uni to be a factor:
  # Transform the variable numeracy using log, square root and reciprocal transformation.
# Plot histograms of numeracy, log numeracy, square root numeracy and reciprocal transformed numeracy scores and add a normal density line. Interpret the assumption of normality based on the histograms
# Plot Q-Q plots of the transformations above and interpret.
# Conduct and interpret Shapiro – Wilk Tests for numeracy, log numeracy, square root numeracy and reciprocal transformed numeracy scores
library(ggplot2)
library(car)
exam_data <- read_excel("RExam.xlsx")
# variable uni to be a factor
exam_data$uni <- as.factor(exam_data$uni)

# Transform the variable numeracy using log, square root, and reciprocal transformation
exam_data$log_numeracy <- log(exam_data$numeracy)
exam_data$sqrt_numeracy <- sqrt(exam_data$numeracy)
exam_data$reciprocal_numeracy <- 1/exam_data$numeracy

# Histograms of the transformed variables
ggplot(exam_data, aes(x = numeracy)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  stat_function(fun = dnorm, color = "red") +
  ggtitle("Histogram of Numeracy Scores")

ggplot(exam_data, aes(x = log_numeracy)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  stat_function(fun = dnorm, color = "red") +
  ggtitle("Histogram of Log-transformed Numeracy Scores")

ggplot(exam_data, aes(x = sqrt_numeracy)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  stat_function(fun = dnorm, color = "red") +
  ggtitle("Histogram of Square Root-transformed Numeracy Scores")

ggplot(exam_data, aes(x = reciprocal_numeracy)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  stat_function(fun = dnorm, color = "red") +
  ggtitle("Histogram of Reciprocal-transformed Numeracy Scores")

# Q-Q plots
qqPlot(exam_data$log_numeracy, main = "Q-Q Plot of Log-transformed Numeracy Scores")
qqPlot(exam_data$sqrt_numeracy, main = "Q-Q Plot of Square Root-transformed Numeracy Scores")
qqPlot(exam_data$reciprocal_numeracy, main = "Q-Q Plot of Reciprocal-transformed Numeracy Scores")

# Shapiro-Wilk test
shapiro.test(exam_data$numeracy)
shapiro.test(exam_data$log_numeracy)
shapiro.test(exam_data$sqrt_numeracy)
shapiro.test(exam_data$reciprocal_numeracy)





# Exercise 2
# Test the assumption of homogeneity of variance for numeracy, log numeracy, square root numeracy and reciprocal transformed numeracy variables in Exercise 1.

# Exercise 2
# Test the assumption of homogeneity of variance
leveneTest(log_numeracy ~ uni, data = exam_data)
leveneTest(log_numeracy ~ uni, data = exam_data)
leveneTest(sqrt_numeracy ~ uni, data = exam_data)
leveneTest(reciprocal_numeracy ~ uni, data = exam_data)
