#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Data preprocessing script
# Filename:     test.R
# Description:  Opens data sets and preprocesses them for the analysis
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-12-07 08:36:31
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
source('settings.R')

#### FUNS --------------------------------
getData <- function (x) {
  require(dplyr)
  d <- data.frame(readxl::read_excel(x))
  names(d) <- gsub(".", "", names(d), fixed = TRUE)
  names(d) <- tolower(names(d))
  d <- d %>%
    select(date,
           actorparty,
           objectparty,
           direction,
           language,
           tone,
           spotlength,
           adid
    ) %>% return
}

#### Loading Data Sets --------------------------------
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
#Data <- plyr::rbind.fill(files)

# Create type of data variable in Global data.frame
#data_type <- stringr::str_extract(names(files), "[A-Z]+")
#Data$type_source <- rep(data_type, sapply(files, nrow))
d1$type_source <- 'TV_Ads'
d2$type_source <- 'TV_Ads'

Data <- plyr::rbind.fill(d1,d2)
Data <- as.data.frame(Data)


# Recode Party Codes
Data[Data==62100] <- 'GPC'
Data[Data==62300] <- 'NDP'
Data[Data==62400] <- 'LPC'
Data[Data==62700] <- 'BQ'
Data[Data==62600] <- 'CPC'

# Dummy date
# Data$post <- 0
# Data$post[Data$day>=18 & Data$month>=9] <- 1

# Recode Variables
# Data$direction[Data$direction==1] <- 'positive'
# Data$direction[Data$direction==0] <- 'negative'

Data$direction[Data$direction==1] <- 1
Data$direction[Data$direction==0] <- -1

Data[Data==99] <- NA
Data$language[Data$language==1] <- 'en'
Data$language[Data$language==3] <- 'fr'

# Clean workspace
rm(list=setdiff(ls(), "Data"))
invisible(gc())
