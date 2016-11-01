#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Data preprocessing script
# Filename:     test.R
# Description:  Opens data sets and preprocesses them for the analysis
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-11-01 14:31:34
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
# Loading Data Sets ------------------------------------------------------------
temp  <- list.files(path='data', pattern="TVADS", full.names=T)
#temp  <- list.files(path='data', pattern="*.xlsx", full.names=T)
#files <- lapply(temp, readxl::read_excel)
d1 <- data.frame(readxl::read_excel(temp[1]))
d2 <- data.frame(readxl::read_excel(temp[2]))

d1$unique_id <- paste0(d1[,which(names(d1)=='Coder.ID')],'_', d1[,which(names(d1)=='Ad.ID')])
d2$unique_id <- paste0(d2[,which(names(d1)=='Coder.ID')],'_', d2[,which(names(d1)=='Ad.ID')])
#d1$Date
#d2$Data <-

files <- list(d1, d2)

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
names(files) <- stringr::str_extract(temp, "[A-Z]+_[A-Z]+")

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
Data <- plyr::rbind.fill(files)

# Create type of data variable in Global data.frame
data_type <- stringr::str_extract(names(files), "[A-Z]+")
Data$type_source <- rep(data_type, sapply(files, nrow))
names(Data) <- gsub(".", "", names(Data), fixed = TRUE)
names(Data) <- tolower(names(Data))

Data <- Data %>% select(type_source, unique_id, actorparty, objectparty, direction, language, tone, spotlength)

# Recode Party Codes
Data[Data==62100] <- 'GPC'
Data[Data==62300] <- 'NDP'
Data[Data==62400] <- 'LPC'
Data[Data==62700] <- 'BQ'
Data[Data==62600] <- 'CPC'

Data[Data=="ndp"]   <- "NDP"
Data[Data=="bloc"]  <- "BQ"
Data[Data=="lpc"]   <- "LPC"
Data[Data=="cpc"]   <- "CPC"
Data[Data=="green"] <- "GPC"

Data[Data=="NPD"] <- "NDP"
Data[Data=="PLC"] <- "LPC"
Data[Data=="PCC"] <- "CPC"

# Dummy Date
# Data$post <- 0
# Data$post[Data$day>=18 & Data$month>=9] <- 1

# Recode Variables
Data$direction[Data$direction==1] <- 'positive'
Data$direction[Data$direction==0] <- 'negative'
Data[Data==99] <- NA
Data$language[Data$language==1] <- 'en'
Data$language[Data$language==3] <- 'fr'

# Clean workspace
rm(list=setdiff(ls(), "Data"))
gc()
