#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-09 11:06:35
# Modified:     2016-05-09 14:50:02
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

src = list.files('src/', pattern="*.R")
sapply(paste0('src/',src),source,.GlobalEnv)

# Analysis of Debate Data ------------------------------------------------------
str(Deb)

### 62400: PLQ
### 62700: PQ
### 62401: CAQ
### 62300: QS

## 1. Positive negative debate by party
test <- subset(Deb, direction!=99 &
  actorparty %in% c(62400, 62700, 62401, 62300))

table(test$direction, test$actorparty)

ggplot(test, aes(x=factor(actorparty), fill=factor(direction))) +
  geom_bar(position='dodge') +
  scale_fill_grey()

# 2. Most common topics
## - Most negative topics
## - Most positive topics
# 3. Evolution of topics during campaign
# 4. Evolution of topic sentiment campaign
