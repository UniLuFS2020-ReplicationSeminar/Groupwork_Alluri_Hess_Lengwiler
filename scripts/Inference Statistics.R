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

knitr::opts_chunk$set(echo = TRUE)

#install.packages("tidyverse")
#install.packages("haven")
#install.packages("car")
#install.packages("stargazer")
library(tidyverse)
library(haven)
library(car)
library(stargazer)
library(ggplot2)


#4 Load dataset
load("data_processed/data_trust.Rdata")

#5 Data manipulation

# Create "Age Groups"
dat <- data_trust %>% 
  mutate(age_group = cut(agea, 
                         breaks = c(14, 24, 44, 64, 90),
                         labels = c("Youth", "Young_Adult", "Middle", "Senior")))

# Convert domicil, gndr and rlgdnm(religion) into factors
dat$domicil <- factor(data_trust$domicil, 
                      levels = c(1,2,3,4,5),
                      labels = c("Big_City", "Suburb", "Small_City", 
                      "Country_Village","Farm_Countryside"))

dat$gndr<- factor(data_trust$gndr, 
                  levels = c(1,2), 
                  labels = c("Male", "Female"))

dat$rlgdnm <- factor(data_trust$rlgdnm,
                      levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels = c("Roman_Catholic", "Protestant", "Eastern_Orthodox",
                                 "Other_Christian denomination","Jewish", "Islam",
                                 "Eastern religions", "Other Non-Christian religions"))


#5 Inference Statistic
#5.1
#5.2 Run individual linear regressions for each independent variable
#5.2.1 Model 1: Independent Variable: domicil
model_1 <- lm(t_score ~ domicil, dat)

#5.2.2 Model 2: Independent Variable: gndr
model_2 <- lm(t_score ~ gndr, dat)

#5.2.3 Model 3: Independent Variable: agea
model_3 <- lm(t_score ~ agea, dat)

#5.3 Create Tables
#5.3.1 Table for model 1(domicil)
stargazer(model_1, type = "text")

#5.3.2 Table for model 2(gndr)
stargazer(model_2, type = "text")

#5.3.3 Table for model 3(agea)
stargazer(model_3, type = "text")


#5.4 Plot models

#5.4.1 Plot Model 1 (Domicile vs Trust Score)

ggplot(dat, aes(domicil, t_score, fill = domicil))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Trust level comparison based on living location",
       x = "Domicile",
       y = "Trust Score")




#5.4.3 Plot model 2 (Gender vs Trust Score)

dat2 <- dat %>% 
  group_by(gndr) %>% 
  summarise(med_tscore = median(t_score))
            
ggplot(dat2, aes(gndr, med_tscore, fill = gndr))+
    geom_col()+
    labs(title = "Median trust score across genders",
         x = "Gender",
         y = "Trust Score")




#5.4.3 Plot model 3 (Age vs Trust Score)


ggplot(dat, aes(agea, t_score))+
  geom_jitter(color = "blue", alpha = 0.1)+
  geom_smooth(method = "lm", color = "red")+
  labs(title = "Trust Score vs Age",
       x = "Age",
       y = "Trust Score")+
  theme_bw()



#5.4.4 Plot model 4 (Religion vs Trust Score)

ggplot(dat, aes(rlgdnm, t_score, fill = rlgdnm))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  labs(title = "Trust level comparison based on Religion",
       x = "Religion",
       y = "Trust Score")

