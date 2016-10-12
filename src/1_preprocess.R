#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Data preprocessing script
# Filename:     test.R
# Description:  Opens data sets and preprocesses them for the analysis
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-10-12 09:24:16
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
rm(list=ls())

# Functions --------------------------------------------------------------------
# Subset strings from right
right_substring <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Loading Data Sets ------------------------------------------------------------

temp  <- list.files(path='data', pattern="*.xlsx", full.names=T)
files <- lapply(temp, read_excel)

# TV debates
# Deb_RC <- read_excel('data/DEBATE_CAN_24_09_2015_TelevisedDebateData.xlsx')
# Deb_TVA <- read_excel('data/DEBATE_CAN_02_10_2015_TelevisedDebateData.xlsx')
# Deb_MCL <- read_excel('data/DEBATE_CAN_06_08_2015_TelevisedDebateData.xlsx')
# Deb_MCL[,12] <- '20:00 pm'

# TV-ads
# Ads_TV <- read_excel('data/TVAds/TVADS_CAN_19_10_2015_TelevisionCommercialsData.xls')

# Expert Surveys
# Experts <- readstata13::read.dta13(
#   'data/2015_medw_charles/ExpertSurvey/Exp_data_2015r.dta',
#   fromEncoding= "macintosh", encoding= "UTF-8"
# )

# TODO: Social Media Data (Fb&Tw)
# Fb  <-
# Tw  <-

## Preprocessing the Data -----------------------------------------------------

# Creating a list of the data to be used filtered by UpperCase first letter
# files <- lapply(ls(pattern="^[A-Z]"), get)
# names(files) <- ls(pattern="^[A-Z]")

# Convert all variable names to lower case for merge
files <- lapply(files,
  function(i) {
    y <- data.frame(i)
    names(y) <- gsub(" ", "", names(y), fixed = TRUE)
    names(y) <- tolower(names(y))
    return(y)
  }
)

# Convert files to a global data.frame with all data types
Data <- data.table::rbindlist(files, fill=T)
dim(Data)
# Create type of data variable in Global data.frame
Data$type_source <- rep(names(files), sapply(files, nrow))
names(Data) <- gsub(".", "", names(Data), fixed = TRUE)

# Filter only sentences made by parties
# Data <- subset(
#   Data, actorparty %in%
#   c(62100, 62300, 62400, 62700, 62600)
# )

# Data <- subset(Data, actorparty!=99)
# Data <- subset(Data, objectparty!=99)

# Recode Party Codes
Data[Data==62100] <- 'GPC'
Data[Data==62300] <- 'NDP'
Data[Data==62400] <- 'LPC'
Data[Data==62700] <- 'BQ'
Data[Data==62600] <- 'CPC'

Data[Data=="ndp"] <- "NDP"
Data[Data=="bloc"] <- "BQ"
Data[Data=="lpc"] <- "LPC"
Data[Data=="cpc"] <- "CPC"
Data[Data=="green"] <- "GPC"

Data[Data=="NPD"] <- "NDP"
Data[Data=="PLC"] <- "LPC"
Data[Data=="PCC"] <- "CPC"

# Recode dates in TV Ads
# Create 'post' event variables based on specific date
Data$year <- as.numeric(right_substring(Data$date, 4))
Data$month <- right_substring(Data$date, 6)
Data$month <- as.numeric(substr(Data$month, 1,2))
Data$day <- as.numeric(substr(Data$date, 1,nchar(Data$date)-6))

# Dummy Date
Data$post <- 0
Data$post[Data$day>=18 & Data$month>=9] <- 1

# Reordering variables
Data <- Data %>% select(noquote(order(colnames(Data))))

# Recode Direction
Data$direction[Data$direction == 0] <- 'Negative'
Data$direction[Data$direction == 1] <- 'Positive'

# Clean workspace
rm(list=setdiff(ls(), "Data"))
gc()
