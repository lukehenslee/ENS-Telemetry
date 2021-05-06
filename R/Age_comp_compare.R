#==================================================================================================
# Compare age distributions between tagged and commercially harvested coho
# Date: May 5, 2021
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#
#
#==================================================================================================
#NOTES: 
#==================================================================================================

library(ggpubr)
library(forcats)
library(ggforce)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(tidyverse)
library(ggplot2)
library(bbmle)
library(dplyr)
library(manipulate)
library(visreg) 
library(ggridges)
library(reshape2)
library(cowplot)
library(ggmap)
library(ggspatial)
library(sf)
library(spData)
library(ggsn)
library(extrafont)
library(knitr)
loadfonts(device = "win")
library(parzer)
library(readxl)

# Set wd
setwd("Q:/RESEARCH/Tagging")

# Import data
mcode <- read_csv("M-code list/M_code_list_V12.csv")
mcode <- mcode %>% 
  filter(mcode$Age == 1.1 | mcode$Age == 2.1 | mcode$Age == 3.1)

SD5 <- read_csv("Selectivity/SHK_comm_coho_2019.csv")
SD5 <- SD5 %>% 
  drop_na(totalAge)
SD5 <- SD5 %>% 
  filter(SD5$totalAge != 0.1)

SD6 <- read_csv("Selectivity/UNK_comm_coho_2019.csv")
SD6 <- SD6 %>% 
  drop_na(totalAge)

# Combine ages into dataframe
tagage <- mcode$Age
taglabel <- rep("Tag", 322)

ageSD5 <- SD5$totalAge
SD5label <- rep("SD5", 111)

ageSD6 <- SD6$totalAge
SD6label <- rep("SD6", 176)

Age <- append(tagage, ageSD5)
Age <- append(Age, ageSD6)

Sample <- append(taglabel, SD5label)
Sample <- append(Sample, SD6label)

agedist <- data.frame(Age, as.factor(Sample))

agedist <- agedist %>% 
  rename(Age = Age,
         Sample = as.factor.Sample.)
# Plot
ggplot(agedist, aes(fill = Sample)) +
  geom_bar(aes(x = as.factor(Age), y = ..prop.., group = Sample), col = "black", position = "dodge") +
  xlab("Age") +
  ylab("Proportion") +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         text = element_text(family = "Times New Roman"),
         legend.position = c(0.8,0.75),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         axis.ticks.y = element_line(size = 0.5),
         axis.ticks.x = element_line(size = 0.5),
         axis.ticks.length = unit(0.2,"cm"),
         axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)),
         axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
                                    margin = margin(t = 5, r = 0, b = 0, l = 0)),
         axis.line = element_line(colour = "black", size = 0.5, lineend = "square"),
         panel.border = element_rect(color = "black", fill = "transparent", size = 0.5))

ggplot(agedist, aes(fill = Sample)) +
  geom_col(aes(as.factor(Age)), position = "dodge")

# Perform ANOVA
aov <- aov(Age ~ Sample, data = agedist)
summary(aov)
plot(TukeyHSD(aov))
