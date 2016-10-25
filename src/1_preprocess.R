#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Data preprocessing script
# Filename:     test.R
# Description:  Opens data sets and preprocesses them for the analysis
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-10-25 06:59:43
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

# Expert Surveys
# Experts <- readstata13::read.dta13(
#   'data/2015_medw_charles/ExpertSurvey/Exp_data_2015r.dta',
#   fromEncoding= "macintosh", encoding= "UTF-8"
# )

# TODO: Social Media Data (FB&TW)
# Fb  <-
# Tw  <-

## Preprocessing the Data -----------------------------------------------------

# Creating a list of the data to be used filtered by UpperCase first letter
names(files) <- str_extract(temp, "[A-Z]+_[A-Z]+")

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
# Create type of data variable in Global data.frame
data_type <- str_extract(names(files), "[A-Z]+")
Data$type_source <- rep(data_type, sapply(files, nrow))
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

# Dummy Date
Data$post <- 0
Data$post[Data$day>=18 & Data$month>=9] <- 1

# Reordering variables
Data <- Data %>% select(order(colnames(Data)))

# Recode Direction
Data$direction[Data$direction == 0] <- 'Negative'
Data$direction[Data$direction == 1] <- 'Positive'
Data <- data.frame(Data)

# Clean workspace
rm(list=setdiff(ls(), "Data"))
gc()
