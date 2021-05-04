#==================================================================================================
# Compare length distributions between tagged and commercially harvested coho
# Date: May 4, 2021
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
mcode <- read_csv("Github/data/M_code_list_V10.csv")
SD5 <- read_csv("Selectivity/SHK_comm_coho_2019.csv")
SD6 <- read_csv("Selectivity/UNK_comm_coho_2019.csv")

# Combine lengths into dataframe
ld <- mcode$Length
lds <- rep("Tag", 400)

ldSD5 <- SD5$length
ldSD5s <- rep("SD5", 120)

ldSD6 <- SD6$length
ldSD6s <- rep("SD6", 200)

LD <- append(ld, ldSD5)
LD <- append(LD, ldSD6)

LDS <- append(lds, ldSD5s)
LDS <- append(LDS, ldSD6s)

lengthdist <- data.frame(LD, as.factor(LDS))

lengthdist <- lengthdist %>% 
  rename(Length = LD,
         Sample = as.factor.LDS.)
# Plot
ggplot(lengthdist) +
  geom_boxplot(aes(x = Sample, y = Length)) 

# Plot
ggplot(lengthdist) +
  geom_density_line(aes(x = Length, fill = fct_infreq(Sample)), alpha = 0.5) +
  scale_fill_brewer(type = "qual", guide = "legend",
                    palette = "Set2", name = "Sample", breaks = c("Tag", "SD5", "SD6"), 
                    labels = c("Tagged", "SD5 comm", "SD6 comm")) +
  xlab("Length (mm)") +
  ylab("Density") +
  theme_classic() +
  theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
         axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
         text = element_text(family = "Times New Roman"),
         legend.position = c(0.2,0.75),
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

ggplot(mcode) +
  geom_boxplot(aes(y = Length)) +
  geom_boxplot(data = SD5, aes(y = length))

# Perform ANOVA
aov <- aov(Length ~ Sample, data = lengthdist)
summary(aov)
plot(TukeyHSD(aov))
