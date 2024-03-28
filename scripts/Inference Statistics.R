---
title: "Data Mining using R"
subtitle: "CE1"
authors: "Silvan. H, Kerim. L, Vishal. A"
date: "29.03.2024"
output: 
  html_document:
  toc: true
toc_float: true
theme: united
---

#1 Clear all previous data and commands 
rm(list = ls())
cat("\014")

#2 Check working directory
getwd()

#3 Setup libraries
```{r setup, include = FALSE}
knitr::opts_chunk$set(echo = TRUE)

#install.packages("tidyverse")
#install.packages("haven")
#install.packages("car")
#install.packages("stargazer")
library(tidyverse)
library(haven)
library(car)
library(stargazer)
``` 

#4 Load dataset
dat <- load("data_processed/dat_trust.Rdata")

# Create "Age Groups"
data_trust <- data_trust %>% 
  mutate(age_group = cut(agea, 
                         breaks = c(14, 24, 44, 64, 90),
                         labels = c("Youth", "Young_Adult", "Middle", "Senior")))


#5 Inference Statistic
#5.1 Convert domicil and gndr into factors, add labels
data_trust$domicil <- factor(data_trust$domicil, levels = c(1,2,3,4,5), labels = c("Big City", "Suburbs", "Town/Small City", "Country Village","Farm/Countryside"))
data_trust$gndr<- factor(data_trust$gndr, levels = c(1,2), labels = c("Male", "Female"))

#5.2 Run individual linear regressions for each independent variable
#5.2.1 Model 1: Independent Variable: domicil
model_1 <- lm(average_trust ~ domicil, data_trust)

#5.2.2 Model 2: Independent Variable: gndr
model_2 <- lm(average_trust ~ gndr, data_trust)

#5.2.3 Model 3: Independent Variable: agea
model_3 <- lm(average_trust ~ agea, data_trust)

#5.3 Create Tables
#5.3.1 Table for model 1(domicil)
stargazer(model_1, type = "text")

#5.3.2 Table for model 2(gndr)
stargazer(model_2, type = "text")

#5.3.3 Table for model 3(agea)
stargazer(model_3, type = "text")


#5.4 Plot models
#5.4.1 Plot Model 1 (domicil)

#5.4.2 Plot Model 2 (gndr)

#5.4.3 Plot model 3 (agea)
ggplot(data_trust, aes(x=agea, y=average_trust)) + geom_jitter(color="blue",alpha = 0.1) +
  geom_smooth(method ="lm", formula =y ~x, se =TRUE, color = "red") + theme_minimal() +
  scale_y_continuous(limits = c(0,10),breaks=c(0,1,2,3,4,5, 6, 7, 8, 9,10)) +
  scale_x_continuous(limits = c(15, 90), breaks=seq(15,90, by=5)) 


      