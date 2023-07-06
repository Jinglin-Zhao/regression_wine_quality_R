setwd("C:\\Users\\Lenovo\\Desktop\\regression\\Project")
redDat = read.csv("winequality_red.csv", sep = ";", header = T)
redDat
# Check missing value
install.packages("tidyverse")
library(tidyverse)
redDat %>% is.na() %>% colSums()
sum(is.na(redDat))
# Outliers
boxplot(redDat)
#summary all the variables
summary(redDat)
#summary the dependent variables
summary(redDat$quality)
table(redDat$quality)
install.packages("GGally")
library(GGally)
# plot wine quality
#Distribution of red wine quality ratings
ggplot(redDat,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()
#plot variables
plot(redDat)
# plot variables correlation
#Correlation Heatmap of Variables
install.packages("corrplot")
library("corrplot")
corrplot(cor(redDat))
library(factoextra)
library (clValid)
library (caret)
library(dplyr)
library(ggplot2)
library(MASS)
library(plotly)
library(corrplot)
library(gridExtra)
library(FactoMineR)
library(factoextra)
library(corrgram)
redDat$quality=as.factor(redDat$quality)
ggplot(redDat, aes(x=quality, fill = quality)) +
  geom_bar(stat="count") +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  labs(y="Num of Observations", x="Wine Quality") +
  labs(title= "Distribution of Red Wine Quality Ratings")
redDat%>%
  gather(-quality, key = "var", value = "value") %>% 
  ggplot(aes(x = quality, y = value, color = quality)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free", ncol = 3)+
  theme(legend.position="none")
#correlation description
b = cor(redDat[,-12])
b
a = cor(redDat[,-12], method="spearman")
a
install.packages("ggcorrplot")
str(redDat)
library(ggcorrplot)
corr <- cor(redDat)
ggcorrplot(corr, hc.order = FALSE, type  = "lower",
           lab = TRUE)
# modeling
full <- lm(quality ~ ., redDat)
summary(full)

###########   PACKAGE ######################

library(mixlm)
forward_model <- forward(full, alpha = 0.05, full = TRUE)
forward_model
#forward(full, alpha = 0.10, full = TRUE)
backward_model <- backward(full, alpha = 0.05, full = TRUE, hierarchy = TRUE)
#backward(full, alpha = 0.15, full = TRUE, hierarchy = TRUE)
stepwise_model <- stepWise(full, alpha.enter = 0.05, alpha.remove = 0.10, full = TRUE)
#choose linear model
stepAIC(full,direction="both")


# working model
reduced_linear_model <- lm(formula = quality ~ alcohol + volatile.acidity + sulphates + 
            total.sulfur.dioxide + chlorides + pH + free.sulfur.dioxide, data=redDat)
summary(reduced_linear_model)
vif(reduced_linear_model)
# the residual
plot(reduced_linear_model$residuals)
# plot qq-pp
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(reduced_linear_model)
# Check linearity
vif(reduced_linear_model)
mean(vif(reduced_linear_model))

# qq plot try
# Load the necessary packages
library(car)
library(MASS)

# Create a QQ plot of the residuals
qqplot(x = fitted(reduced_linear_model),y= resid(reduced_linear_model), main = "QQ Plot of Residuals")

# Create a spread-level plot to check for non-constant variance
spreadLevelPlot(reduced_linear_model, main = "Spread-Level Plot")

# Calculate the studentized residuals
student_resid <- studres(reduced_linear_model)

# Print the studentized residuals to the console
print(student_resid)