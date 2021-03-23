#==================================================================================================
# Importing and filtering marine array receiver data
# Date: February 24, 2021
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#Purpose: To import marine array acoustic receiver data, output acoustic 
# detection histories for all tags
#
#==================================================================================================
#NOTES: Array 5 contained receivers 15 and 35, which are also Tag ID numbers. 
# Since each receiver sends a beacon signal to ensure proper function, it obscures
# detection of tags 15 and 35. Therefore, we filter these Tag IDs out of array 5.
# A similar situation arises in array 7, which contains receiver 55. We filter Tag ID
# 55 out of array 7 detection history. Lotek is aware of the issue and working on
# fixing it for 2021.
#
# No tags detected in Kwiniuk or Tubutulik rivers and receiver data is ommited 
#==================================================================================================

# Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)

# Set working directory ####
setwd("Q:/RESEARCH/Tagging/R/data/detections")

# Import receiver data and combine ####
  # Make list of file names in detection folder
temp <- list.files(pattern = "*.csv")

  # Read file content
det <- lapply(temp, read_csv, 
        col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
        Power = col_double(), TOA = col_skip(), 
        `Tag ID` = col_character(), Time = col_time(format = "%H:%M:%S"), 
        Type = col_skip(), Value = col_skip()), skip = 45)

  # Read file names
filenames <- temp %>% basename() %>% as.list()

  # Combine file content list with file name list and remove ".csv"
det_named <- mapply(c, det, substr(filenames, 1, 3), SIMPLIFY = F)

  # Combine lists and lable receiver ID column
det_all <- rbindlist(det_named, fill = T)
names(det_all)[5] <- "receiver.ID"

# Filter detections ####
  # Omitting tags 15, 35, and 55 (see note). 
det_all_tags <- filter(det_all, between(Date, as.Date("2020-7-29"), as.Date("2020-9-15")), 
       `Tag ID` %in% seq(75, 7995, by = 20)) %>% 
       group_by(`Tag ID`) 

# Modify date/time column ####
det_all_tags$ymd_hms <- paste(det_all_tags$Date, det_all_tags$Time, sep = " ") 

det_all_tags$ymd_hms_numeric <- as.numeric(ymd_hms(det_all_tags$ymd_hms, tz = 'US/Alaska'))

det_all_tags <- subset(det_all_tags, select = -c(Date, Time))

# Write .csv files ####

# Make .csv with all tag detections on one sheet
TAGS <- createWorkbook()

addWorksheet(TAGS, sheetName = "All_detections")

writeData(TAGS, "All_detections", det_all_tags)

saveWorkbook(TAGS, "Q:/RESEARCH/Tagging/R/output/List_all_tag_detections.xlsx")

# Make .csv with a new sheet for each Tag ID
  # Make list of Tag ID dataframes

det_all_tags <- det_all_tags[order(det_all_tags$`Tag ID`),]

det_all_tags_split <- setNames(split(det_all_tags, det_all_tags$`Tag ID`),
         paste(unique(det_all_tags$`Tag ID`)))

  # Create a blank workbook
TAG <- createWorkbook()

for(i in seq(75, 7995, by = 20)) {
  wsName <- paste(i)  
  if(!(wsName %in% det_all_tags$`Tag ID`)) next
  addWorksheet(TAG, sheetName = wsName)
  writeData(TAG, sheet = wsName, x = get(wsName, envir = as.environment(det_all_tags_split)))
}

saveWorkbook(TAG, "Q:/RESEARCH/Tagging/R/output/All_tag_detections.xlsx")

  