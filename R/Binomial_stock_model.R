#==================================================================================================
# Binomial stock model
# Date: March 25, 2021
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#
#
#==================================================================================================
#NOTES: 
#==================================================================================================

# Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)
library(visreg)
library(MASS)

# Set working directory ####
setwd("Q:/RESEARCH/Tagging/Github/data")

# Import tagging data
mcode <- read.csv("M_code_list.csv")

# Filter for final fates which allow for stock assignment (i.e. FF 1 and 2)
ff12 <- mcode %>% filter(mcode$Final.fate..num. %in% c(1:2))

# Convert response factor to binomial (0/1)
bi <- vector(length = nrow(ff12))

for(i in 1:length(bi)) {
  if(ff12[i,9] == ff12[i,16]) {
    bi[i] <- 1
  } else {
    bi[i] <- 0
  }
}

ff12$binomial <- bi

# Convert date and time of capture to POSIXct object
ff12$ymd_hms <- paste(ff12$Date, ff12$Time.in, sep = " ")

ff12$ymd_hms <- parse_date_time(ff12$ymd_hms, orders = 'mdy HM', tz = 'US/Alaska') 

ff12$ymd_hms_numeric <- as.numeric(ymd_hms(ff12$ymd_hms))

# Build binomial model 
bi_model_full <- glm(binomial ~ ymd_hms*Lat, data = ff12,
                     family = binomial(link = "logit"))

bi_model_no_int <- glm(binomial ~ ymd_hms + Lat, data = ff12,
                     family = binomial(link = "logit"))
summary(bi_model_no_int)

# Try splitting into separate SD5 and SD6 models
  #SD 5
ff125 <- ff12 %>% filter(ff12$Fishing.loc == 5)

bi_model_5_full <- glm(binomial ~ ymd_hms_numeric + Lat, data = ff125,
                       family = binomial(link = "logit"))

summary(bi_model_5_full)

visreg(bi_model_5_full, scale = "response")

  # SD6
ff126 <- ff12 %>% filter(ff12$Fishing.loc == 6)

bi_model_6_full <- glm(binomial ~ ymd_hms_numeric + Lat, data = ff126,
                       family = binomial(link = "logit"))

summary(bi_model_6_full)

visreg(bi_model_6_full, scale = "response")

# Try 6a, 6b, 6c... 
