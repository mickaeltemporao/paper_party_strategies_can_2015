#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     build_twitter_features.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-12-10 11:47:56
# Modified:     2016-12-10 16:25:38
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
source('src/data/make_twitter_dataset.R')


## Get language of the tweets
data$lang <- textcat::textcat(data$message)
data$lang[data$lang %in% c('scots')] <- 'english'
prop <- round(prop.table(table(ifelse(data$lang %in% c('english', 'french'), 1,0)))[1],2)
sprintf("Proportion of non english-french tweets : %.2f", prop)

## Filter only english tweets
data <- subset(data, lang=='english')
