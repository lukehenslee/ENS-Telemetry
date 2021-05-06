#==================================================================================================
# Estimate receiver detection probability 
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

# Blueberry creek array 

# First we subset all fish tagged north of the array with final fates south of the array 
blueberry <- mcode %>% 
  filter(`FF Loc` == "Unalakleet" | `FF Loc` == "Golsovia" | `FF Loc` == "S") %>% 
  filter(`Lat` >=  63.954056)

blueberry <- data.frame(rbind(blueberry, mcode %>% 
                               filter(`FF Loc` == "Egavik" |`FF Loc` == "Tagoomenik" | `FF Loc` == "Shaktoolik" | `FF Loc` == "Ungalik"
                                      | `FF Loc` == "Inglutalik" | `FF Loc` == "Koyuk" | `FF Loc` == "N") %>% 
                               filter(`Lat` <= 63.954056)))

# Okay, now we have to figure out which of those fish were detected at the Blueberry creek array
  # Import Blueberry creek detections
arrays <- read_csv("R/output/Previous versions/Marine passage/Array_all_passage_final.csv")
array5 <- filter(arrays, arrays$array_ID == 5)

# Filter mcodes which were not detected by array 5
notdetected5 <- anti_join(blueberry, array5, by = c("M.code" = "Tag.ID"))

r5 <- nrow(blueberry) - nrow(notdetected5)
z5 <- nrow(notdetected5)

phat5 <- r5/(r5 + z5)
phat5

# Junction creek array
  # First we subset all fish tagged north of the array with final fates south of the array 
junction <- mcode %>% 
  filter(`FF Loc` == "Egavik" | `FF Loc` == "Unalakleet" | `FF Loc` == "Golsovia" | `FF Loc` == "S") %>% 
  filter(`Lat` >= 64.131518)

  # Then all fish tagged south of the array with final fates north
junction <- data.frame(rbind(junction, mcode %>% 
                     filter(`FF Loc` == "Tagoomenik" | `FF Loc` == "Shaktoolik" | `FF Loc` == "Ungalik"
                            | `FF Loc` == "Inglutalik" | `FF Loc` == "Koyuk" | `FF Loc` == "N") %>% 
                       filter(`Lat` <= 64.131518)))

# Import Junction array detections
array4 <- filter(arrays, arrays$array_ID == 4)

# Filter mcodes which were not detected by array 4
notdetected4 <- anti_join(junction, array4, by = c("M.code" = "Tag.ID"))

r4 <- nrow(junction) - nrow(notdetected4)
z4 <- nrow(notdetected4)

phat4 <- r4/(r4 + z4)
phat4
