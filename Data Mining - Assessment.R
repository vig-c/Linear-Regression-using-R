#__________Assessment__________#

#Data mining multiple linear regression assignment

#__________Importing packages and libraries__________#

library(tidyverse)
library(gridExtra)
library(ggplot2)
library(lubridate)
library(dplyr)
library(Hmisc)
library(cowplot)
library(WVPlots)

#__________Importing primary data set__________#

insurance = read.csv("C:/Users/91978/Desktop/Vignesh Files/University of Edinburgh/Courses/Data Mining - 1/Project/insurance.csv")
View(insurance)

#__________Understanding the imported data__________#

head(insurance) #To display first n rows present in the input data frame
str(insurance) #To display internal structure of any R object in a compact way
summary(insurance) #To display summary of the results of various model fitting functions

#__________Plotting graphs to understand primary data better__________#

plot_age = ggplot(insurance, aes(age, charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "Age" , y = "Insurance Charges")

plot_bmi = ggplot(insurance, aes(bmi, charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "BMI" , y = "Insurance Charges")

grid_1 = plot_grid(plot_age, plot_bmi) 
title = ggdraw()
plot_grid(title, grid_1, ncol=1, rel_heights=c(0.1, 1))

ggsave("C:/Users/91978/Desktop/graph1.png", grid_1, width = 6, height = 4, dpi = 300)

plot_sex = ggplot(data = insurance, mapping = aes(x = as.factor(sex), y = charges, fill = sex)) +
  geom_boxplot() +
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "Gender" , y = "Insurance Charges") +
  scale_fill_manual("Gender",values=c("red","blue"))

plot_child = ggplot(data = insurance, mapping = aes(x = as.factor(children), y = charges, color = children)) +
  geom_boxplot() +
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "Children" , y = "Insurance Charges")

grid_2 = plot_grid(plot_sex, plot_child) 
title = ggdraw()
plot_grid(title, grid_2, ncol=1, rel_heights=c(0.1, 1))

ggsave("C:/Users/91978/Desktop/graph2.png", grid_2, width = 6, height = 4, dpi = 300)

plot_smoker = ggplot(data = insurance, mapping = aes(x = as.factor(smoker), y = charges, color = smoker)) +
  geom_boxplot()+
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "Smoker" , y = "Insurance Charges") +
  scale_colour_manual("Smoker",values=c("green","red"))

plot_region = ggplot(data = insurance, mapping = aes(x = as.factor(region), y = log(charges), fill = region)) +
  geom_boxplot()+
  coord_flip() +
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "Region" , y = "(ln) Charges") +
  scale_fill_manual("Region",values=c("green","blue","purple","pink"))

grid_3 = plot_grid(plot_smoker, plot_region) 
title = ggdraw()
plot_grid(title, grid_3, ncol=1, rel_heights=c(0.1, 1))

ggsave("C:/Users/91978/Desktop/graph3.png", grid_3, width = 6, height = 4, dpi = 300)

insurance$BMI_Category = "Underweight"
insurance$BMI_Category[insurance$bmi >= 18.5 & insurance$bmi <= 24.9] <- "Normal"
insurance$BMI_Category[insurance$bmi >= 25 & insurance$bmi <= 29.9] <- "Overweight"
insurance$BMI_Category[insurance$bmi > 30] <- "Obese"

n_train = round(0.8 * nrow(insurance))
train_indices = sample(1:nrow(insurance), n_train)
insurance_train = insurance[train_indices, ]
insurance_test = insurance[-train_indices, ]

lm.fit = lm(charges ~ age + BMI_Category + children + smoker + region, data = insurance_train)
round(summary(lm.fit)$coefficients, 2)
cat('Adjusted R^2:', round(summary(lm.fit)$adj.r.sq, 2),'   ', 'F-Test:', round(summary(lm.fit)$f, 2))

lm.fit = lm(charges ~ age + BMI_Category + children + smoker + region +smoker*BMI_Category + smoker*age + smoker*region, data = insurance_train)
round(summary(lm.fit)$coefficients, 2)
cat('Adjusted R^2:', round(summary(lm.fit)$adj.r.sq, 2),'   ', 'F-Test:', round(summary(lm.fit)$f, 2))

lm.fit = lm(charges ~ age + BMI_Category + children + smoker + region +smoker*BMI_Category, data = insurance_train)
round(summary(lm.fit)$coefficients, 2)
cat('Adjusted R^2:', round(summary(lm.fit)$adj.r.sq, 2),'   ', 'F-Test:', round(summary(lm.fit)$f, 2))

insurance_test$prediction = predict(lm.fit, newdata = insurance_test)
graph1 = ggplot(insurance_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "green", alpha = 0.7) + 
  geom_abline(color = "red") +
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "Prediction" , y = "Insurance Charges") 
cat('Adjusted R^2:', round(summary(lm.fit)$adj.r.sq, 2),'   ', 'F-Test:', round(summary(lm.fit)$f, 2))

ggsave("C:/Users/91978/Desktop/graph4.png", graph1, width = 6, height = 4, dpi = 300)

insurance_test$residuals = insurance_test$charges - insurance_test$prediction

graph2 = ggplot(data = insurance_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "green", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "Prediction" , y = "Residuals") 

ggsave("C:/Users/91978/Desktop/graph5.png", graph2, width = 6, height = 4, dpi = 300)

graph3 = ggplot(insurance_test, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "green") +
  theme_light() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs (x = "Residuals" , y = "Count") 

ggsave("C:/Users/91978/Desktop/graph6.png", graph3, width = 6, height = 4, dpi = 300)


