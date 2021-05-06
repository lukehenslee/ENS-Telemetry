#==================================================================================================
# Compare sex distributions between tagged and commercially harvested coho
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

sexID <- vector()
  
for(i in 1:nrow(mcode)) {
  if(mcode[i,9] == "M") {
  sexID[i] <- 1
  } else {
    sexID[i] <- 2
  }
}

mcode$sexID <- sexID

SD5 <- read_csv("Selectivity/SHK_comm_coho_2019.csv")
SD5 <- SD5 %>% 
  drop_na(sexID)


SD6 <- read_csv("Selectivity/UNK_comm_coho_2019.csv")
SD6 <- SD6 %>% 
  drop_na(sexID)

# Combine ages into dataframe
tagsex <- mcode$sexID
taglabel <- rep("Tag", 400)

sexSD5 <- SD5$sexID
SD5label <- rep("SD5", 120)

sexSD6 <- SD6$sexID
SD6label <- rep("SD6", 200)

Sex <- append(tagsex, sexSD5)
Sex <- append(Sex, sexSD6)

Sample <- append(taglabel, SD5label)
Sample <- append(Sample, SD6label)

sexdist <- data.frame(Sex, as.factor(Sample))

sexdist <- sexdist %>% 
  rename(Sex = Sex,
         Sample = as.factor.Sample.)
# Plot
  # Sampler was alternating M/F in 2019
ggplot(sexdist, aes(fill = Sample)) +
  geom_bar(aes(x = as.factor(Sex), y = ..prop.., group = Sample), col = "black", position = "dodge") +
  xlab("Age") +
  ylab("Proportion") +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         text = element_text(family = "Times New Roman"),
         legend.position = c(0.75,0.25),
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

# Perform ANOVA
aov <- aov(Sex ~ Sample, data = sexdist)
summary(aov)
plot(TukeyHSD(aov))
