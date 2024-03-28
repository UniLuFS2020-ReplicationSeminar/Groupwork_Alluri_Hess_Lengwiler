#1 Clear all previous data and commands 
rm(list = ls())
cat("\014")

#2 Check working directory
getwd()

#3 Get needed libraries
#install.packages("tidyverse")
#install.packages("haven")
#intall.packages("dplyr")
library(tidyverse)
library(haven)
library(dplyr)

#4 Get dataset
data <- read_dta("data_original/ESS10.dta")

#5 Subset only needed data and remove NA's
data_trust <- data %>% 
  select(idno, agea, gndr, domicil,
         trstprl, trstlgl, trstplc, 
         trstplt, trstprt, trstep, trstun)


#6 Add variable "average trust in government"
data_trust$average_trust <- 
  rowMeans(data[, c("trstprl","trstlgl", "trstplc", "trstplt", "trstprt", "trstep", "trstun")], na.rm=TRUE)

#7 Custom "Trust Score" creation based on assumed weightage of trust variables
data_trust$t_score <-  with(data_trust,
                          trstprl * 0.25 + 
                          trstlgl * 0.2 + 
                          trstplc * 0.15 + 
                          trstplt * 0.05 + 
                          trstprt * 0.1 + 
                          trstep * 0.15 + 
                          trstun * 0.1 ) 


# Manual control
View(data_trust)

#Save processed data
save(data_trust, file = "data_processed/data_trust.RData")

