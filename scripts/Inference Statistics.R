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
library(ggplot2)
```

#4 Load dataset
load("data_processed/data_trust.Rdata")

#5 Data manipulation
```{r}
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
                      labels = c("Roman_Catholic", "Protestant","Eastern_Orthodox",
                                 "Other_Christian denomination","Jewish", "Islam",
                                 "Eastern religions", "Other Non-Christian religions"))
```


#5 Inference Statistic
#5.1
#5.2 Run individual linear regressions for each independent variable
```{r}
#5.2.1 Model 1: Independent Variable: domicil
model_1 <- lm(t_score ~ domicil, dat)
summary(model_1)

#5.2.2 Model 2: Independent Variable: gndr
model_2 <- lm(t_score ~ gndr, dat)
summary(model_2)

#5.2.3 Model 3: Independent Variable: agea
model_3 <- lm(t_score ~ agea, dat)
summary(model_3)

#5.2.4 Model 4: Multiple linear regression
model_4 <- lm(t_score ~ domicil + gndr + agea + rlgdnm, dat)
summary(model_4)

#5.3 Create Tables
#5.3.1 Table for model 1(domicil)
stargazer(model_1, type = "text")

#5.3.2 Table for model 2(gndr)
stargazer(model_2, type = "text")

#5.3.3 Table for model 3(agea)
stargazer(model_3, type = "text")


```

#5.4 Plots

```{r}
#5.4.1 Plot1 - Distribution of Trust Score

plot1 <- ggplot(dat, aes(t_score))+
  geom_histogram(fill = "lightblue", binwidth = 0.5)+
  geom_vline(aes(xintercept = mean(t_score)), color = "red", linetype = "dashed")+
  geom_vline(aes(xintercept = median(t_score)), color = "purple")+
  labs(title = "Distribution of Trust Score",
       x = "Trust Score",
       y = "Frequency")+
  theme_bw()

```


```{r}
#5.4.2 Plot2 - Domicile vs Trust Score

plot2 <- ggplot(dat, aes(domicil, t_score, fill = domicil))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  labs(title = "Trust level comparison based on living location",
       x = "Domicile",
       y = "Trust Score")
```

```{r}
#5.4.3 Plot3 - Gender vs Trust Score

#Getting median trust score for each gender
dat2 <- dat %>% 
  group_by(gndr) %>% 
  summarise(med_tscore = median(t_score))
            
plot3 <- ggplot(dat2, aes(gndr, med_tscore, fill = gndr))+
    geom_col()+
    labs(title = "Median trust score across genders",
         x = "Gender",
         y = "Trust Score")
```

```{r}
#5.4.4 Plot4 - Age vs Trust Score
plot4 <- ggplot(dat, aes(agea, t_score))+
  geom_jitter(color = "blue", alpha = 0.1)+
  geom_smooth(method = "lm", color = "red")+
  labs(title = "Trust Score vs Age",
       x = "Age",
       y = "Trust Score")+
  theme_bw()
```


```{r}
#5.4.5 Plot5 - Religion vs Trust Score
plot5 <- ggplot(dat, aes(rlgdnm, t_score, fill = rlgdnm))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  labs(title = "Trust level comparison based on Religion",
       x = "Religion",
       y = "Trust Score")
```

#6 Save Plots

ggsave("trust_score_distribution.png", plot = plot1, path = "plots", dpi = 300)
ggsave("domicile_tscore.png", plot = plot2, path = "plots", dpi = 600)
ggsave("gender_tscore.png", plot = plot3, path = "plots", dpi = 300)
ggsave("age_tscore.png", plot = plot4, path = "plots", dpi = 400)
ggsave("relgion_tscore.png", plot = plot5, path = "plots", dpi = 600)




