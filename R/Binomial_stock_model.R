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

# Plot effect of Lat on terminal fish
mod <- glm(binomial ~ Lat, data = ff125, family = binomial(link = "logit"))

glmplot <- ggplot(ff125, aes(Lat, binomial)) +
  geom_rug() +
  geom_smooth(size = 0.5, color = "black", method = "glm", se = T, 
              method.args = list(family = "binomial")) +
  scale_x_continuous(name = "Latitude", expand = c(0,0), 
                     breaks = seq(64.15, 64.35, .05)) +
  scale_y_continuous(name = "Likelihood", 
                     expand = c(0, 0), 
                     limits = c(0, 1), 
                     breaks = seq(0, 1, .2)) +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         text = element_text(family = "Times New Roman"),
         legend.position = c(0.75,0.75),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.key.size = unit(.5, "cm"),
         plot.margin = unit(c(0.2, 0.1, 0.1, 0.1), "cm"),
         axis.ticks.y = element_line(size = 0.5),
         axis.ticks.x = element_line(size = 0.5),
         axis.ticks.length = unit(0.2,"cm"),
         axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         axis.text.x = element_text(colour = "black", size = 14, angle = 90, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.border = element_rect(color = "black", fill = "transparent", size = 0.5))

dev.new(width = 2.75, height = 1.83, units = "in", noRStudioGD = TRUE) %>% 
ggsave(glmplot, file = "GLM_1.tiff", width = 2.75, height = 1.83, units = "in",dpi = 300)

loadfonts(device = "win")
library(tidyverse)
library(ggplot2)
library(extrafont)
library(ggpubr)
library(forcats)
library(ggforce)
library(ggridges)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(readr)
# Load fonts
loadfonts(device = "win")
