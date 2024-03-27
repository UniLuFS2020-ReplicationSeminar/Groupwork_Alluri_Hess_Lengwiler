#1 Clear all previous data and commands 
rm(list = ls())
cat("\014")

#2 Check working directory
getwd()

#3 Get needed libraries
install.packages("tidyverse")
install.packages("haven")
intall.packages("dplyr")
library(tidyverse)
library(haven)
library(dplyr)

#4 Get dataset
data <- read_dta("ESS10.dta")

#5 Subset only needed data
data_trust <- data %>% select(idno,agea,gndr, domicil,trstprl,trstlgl, trstplc, trstplt, trstprt, trstep, trstun)

#6 Add variable "average trust in government"
data_trust$average_trust <- rowMeans(data[, c("trstprl","trstlgl", "trstplc", "trstplt", "trstprt", "trstep", "trstun")], na.rm=TRUE)

#7 Manual control
View(data_trust)

