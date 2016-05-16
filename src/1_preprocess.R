#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-05-15 20:42:03
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

# Loading Data Sets ------------------------------------------------------------
# TV debates
Deb_RC <- read_excel('data/2015_medw/Debates/RC/DEBATE_CAN_24_09_2015_TelevisedDebateData.xlsx')
Deb_TVA <- read_excel('data/2015_medw/Debates/TVA/DEBATE_CAN_02_10_2015_TelevisedDebateData.xlsx')

# TV-ads
Ads_TV <- read_excel('data/2015_medw/TVAds/TVADS_CAN_19_10_2015_TelevisionCommercialsData.xls')

# Expert Surveys
Experts <- readstata13::read.dta13(
  'data/2015_medw/ExpertSurvey/Exp_data_2015r.dta',
  fromEncoding= "macintosh", encoding= "UTF-8"
)

# TODO: Social Media Data (Fb&Tw)
# Fb  <-
# Tw  <-

## Preprocessing the Data -----------------------------------------------------
# Creating a list the data to be used filtered by UpperCase first letter
df_list <- lapply(ls(pattern="^[A-Z]"), get)
names(df_list) <- ls(pattern="^[A-Z]")

# Convert all variable names to lower case for merge
df_list <- lapply(df_list,
  function(i) {
    y <- data.frame(i)
    names(y) <- gsub(" ", "", names(y), fixed = TRUE)
    names(y) <- tolower(names(y))
    return(y)
  }
)

# Convert df_list to a global data.frame with all data types
Data <- data.table::rbindlist(df_list, fill=T)
# Create type of data variable in Global data.frame
Data$type_source <- rep(names(df_list), sapply(df_list, nrow))
names(Data) <- gsub(".", "", names(Data), fixed = TRUE)

# Filter only sentences made by parties
# Data <- subset(
#   Data, actorparty %in%
#   c(62100, 62300, 62400, 62700, 62600)
# )

# Data <- subset(Data, actorparty!=99)
# Data <- subset(Data, objectparty!=99)

# Recode Party Codes
Data[Data==62100] <- 'PV'
Data[Data==62300] <- 'NPD'
Data[Data==62400] <- 'PLC'
Data[Data==62700] <- 'BQ'
Data[Data==62600] <- 'PCC'

## Recode dates
right_substring <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

Data$year <- as.numeric(right_substring(Data$date, 4))
Data$month <- right_substring(Data$date, 6)
Data$month <- as.numeric(substr(Data$month, 1,2))
Data$day <- as.numeric(substr(Data$date, 1,nchar(Data$date)-6))

# Dummy Date
Data$post <- 0
Data$post[Data$day>=18 & Data$month>=9] <- 1

# Reordering variables
Data <- Data %>% select(noquote(order(colnames(Data))))

# Clean workspace
rm(list=setdiff(ls(), "Data"))
gc()
