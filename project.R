setwd("C:\\Users\\Lenovo\\Desktop\\regression\\Project")
whiteDat = read.csv("winequality_white.csv", sep = ";", header = T)
whiteDat
# Check missing value
install.packages("tidyverse")
library(tidyverse)
whiteDat %>% is.na() %>% colSums()
sum(is.na(whiteDat))
# Outliers
boxplot(whiteDat)
#summary all the variables
summary(whiteDat)
#summary the dependent variables
summary(whiteDat$quality)
table(whiteDat$quality)
install.packages("GGally")
library(GGally)
# plot wine quality
#Distribution of red wine quality ratings
ggplot(whiteDat,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()
#plot variables
plot(whiteDat)
# plot variables correlation
#Correlation Heatmap of Variables
install.packages("corrplot")
library("corrplot")
corrplot(cor(whiteDat))
# bar plot variables based on quality
install.packages("FactoMineR")
install.packages("factoextra")
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
install.packages("corrgram")
whiteDat$quality=as.factor(whiteDat$quality)
ggplot(whiteDat, aes(x=quality, fill = quality)) +
  geom_bar(stat="count") +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  labs(y="Num of Observations", x="Wine Quality") +
  labs(title= "Distribution of Red Wine Quality Ratings")
whiteDat%>%
  gather(-quality, key = "var", value = "value") %>% 
  ggplot(aes(x = quality, y = value, color = quality)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free", ncol = 3)+
  theme(legend.position="none")
warning()
#CORRELATION description
b = cor(whiteDat[,-12])
b
a = cor(whiteDat[,-12], method="spearman")
a
library(ggcorrplot)
corr <- cor(whiteDat)

# modelling
full_linear_model <- lm(data = redDat , formula = quality ~.)
Qfit1 <- lm(quality ~ ., data = whiteDat)
summary(Qfit1)
library(car)
vif(full_linear_model)

