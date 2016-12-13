#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Make MEDW Dataset
# Filename:     test.R
# Description:  Preprocesses MEDW Datasets
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-12-10 11:46:48
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
source('settings.R')


#### Loading data Sets --------------------------------
temp  <- list.files(path=data_path, pattern="TVADS", full.names=T)
#temp  <- list.files(path='data', pattern="*.xlsx", full.names=T)
#files <- lapply(temp, readxl::read_excel)

d1 <- getData(temp[1])
d2 <- getData(temp[2])

# Creating a unique ID
d1$unique_id <- paste0('TVAds1_', d1[,which(names(d1)=='adid')])
d2$unique_id <- paste0('TVAds2_', d2[,which(names(d1)=='adid')])

# Handling Dates
d2$date <- gsub("2016", "2015", d2$date)
d2$date <- as.numeric(gsub("[^0-9]", "", d2$date))
d2$date <- as.Date(as.character(d2$date), "%Y%m%d")
d1$date <- ifelse(nchar(d1$date) == 7, paste0('0', d1$date), d1$date)
d1$date <- as.Date(as.character(d1$date), "%d%m%Y")

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
#names(files) <- stringr::str_extract(temp, "[A-Z]+_[A-Z]+")

# # Convert all variable names to lower case for merge
# files <- lapply(files,
#   function(i) {
#     y <- data.frame(i)
#     names(y) <- gsub(" ", "", names(y), fixed = TRUE)
#     names(y) <- tolower(names(y))
#     return(y)
#   }
# )

# Convert files to a global data.frame with all data types
#data <- plyr::rbind.fill(files)

# Create type of data variable in Global data.frame
#data_type <- stringr::str_extract(names(files), "[A-Z]+")
#data$type_source <- rep(data_type, sapply(files, nrow))
d1$type_source <- 'TV_Ads'
d2$type_source <- 'TV_Ads'

data <- plyr::rbind.fill(d1,d2)
data <- as.data.frame(data)


# Recode Party Codes
data[data==62100] <- 'GPC'
data[data==62300] <- 'NDP'
data[data==62400] <- 'LPC'
data[data==62700] <- 'BQ'
data[data==62600] <- 'CPC'

# Dummy date
# data$post <- 0
# data$post[data$day>=18 & data$month>=9] <- 1

# Recode Variables
# data$direction[data$direction==1] <- 'positive'
# data$direction[data$direction==0] <- 'negative'

data$direction[data$direction==1] <- 1
data$direction[data$direction==0] <- -1

data[data==99] <- NA
data$language[data$language==1] <- 'en'
data$language[data$language==3] <- 'fr'

# Clean workspace
rm(list=setdiff(ls(), "data"))
invisible(gc())
