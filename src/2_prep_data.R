#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-05-14 11:36:33
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

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

# Clean workspace
rm(list=setdiff(ls(), "Data"))
gc()

# Recode Party Codes
# 62100: PV
# 62300: NPD
# 62400: PLC
# 62700: BQ
# 62600: PCC

## Positive negative debate by party
Data <- subset(Data, direction!=99 &
  actorparty %in% c(62100, 62300, 62400, 62700, 62600))

## Recode party numbers to chars
Data[Data==62100] <- 'PV'
Data[Data==62300] <- 'NPD'
Data[Data==62400] <- 'PLC'
Data[Data==62700] <- 'BQ'
Data[Data==62600] <- 'PCC'

## Recode dates
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

Data$year <- as.numeric(substrRight(Data$date, 4))
Data$month <- substrRight(Data$date, 6)
Data$month <- as.numeric(substr(Data$month, 1,2))
Data$day <- as.numeric(substr(Data$date, 1,nchar(Data$date)-6))

# Dummy Date
Data$post <- 0
Data$post[Data$day>=18 & Data$month>=9] <- 1
Data <- subset(Data, objectparty!=99)

pre <- subset(Data, post==0)
post <- subset(Data, post == 1)


