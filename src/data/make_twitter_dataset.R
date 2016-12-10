#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Make Twitter Dataset
# Filename:     TwitterTopics.r
# Description:  Preprocesses Twitter Datasets
# Version:      0.0.0.000
# Created:      2016-10-17 20:15:03
# Modified:     2016-12-10 11:51:25
# Author:       Mickael Temporão
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
source('settings.R')


#### Loading data Sets --------------------------------
data <- openxlsx::read.xlsx(paste0(data_path, 'twitter_feeds_can2016.xlsx'))
## MAKE DATE FIELD
data$date  <- openxlsx::convertToDate(data$date)
data$day   <- NULL
data$month <- NULL
## Subset tweets in campaign period
data <- subset(data, date > as.Date("2015-08-3") & date < as.Date("2015-10-20"))

## Clean tweets
account_names <- gsub("([A-Za-z]+).*", "\\1", data$message) #  Extract first word of sentence (account name)
tweets        <- data$message
tweets        <- gsub("pbs\\.twimg.+","",tweets)         #  Remove source in tweest
tweets        <- iconv(tweets, to = "ASCII", sub = " ")  #  Convert to basic ASCII text to avoid silly characters
tweets        <- tolower(tweets)                         #  Make everything consistently lower case
tweets        <- gsub("^(\\w+)", "", tweets)             #  Remove first word of sentence (account name here)
tweets        <- gsub("^rt", " ", tweets)                #  Remove the "RT" (retweet) so duplicates are duplicates
tweets        <- gsub("@\\w+", " ", tweets)              #  Remove user names (all proper names if you're wise!)
tweets        <- gsub("http.+ |http.+$", " ", tweets)    #  Remove links
tweets        <- gsub("[[:punct:]]", " ", tweets)        #  Remove punctuation
tweets        <- gsub("[ |\t]{2,}", " ", tweets)         #  Remove tabs
tweets        <- gsub("amp", " ", tweets)                #  "&" is "&amp" in HTML, so after punctuation removed ...
tweets        <- gsub("^ ", "", tweets)                  #  Leading blanks
tweets        <- gsub(" $", "", tweets)                  #  Lagging blanks
tweets        <- gsub(" +", " ", tweets)                 #  General spaces (should just do all whitespaces no?)

## Replace original message by clean tweets
data$message <- tweets
rm(tweets)
