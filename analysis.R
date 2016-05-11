#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-09 11:06:35
# Modified:     2016-05-11 16:13:37
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

src = list.files('src/', pattern="*.R")
sapply(paste0('src/',src),source,.GlobalEnv)

# Analysis of Debate Data ------------------------------------------------------

## Party Codes
# 62300: QS
# 62400: PLQ
# 62401: CAQ
# 62700: PQ

## 1. Positive negative debate by party
test <- subset(Deb, direction!=99 &
  actorparty %in% c(62400, 62700, 62401, 62300))

counts <- table(test$direction, test$actorparty)
round(prop.table(counts, 2), 2)

pdf('/figs/pos_neg.pdf')
barplot(counts, main='Positive-Negative Sentences in TV Debates',
  names.arg=c("QS", "PLQ", "CAQ", "PQ"),
  beside=T, ylim=c(0,200)
)
legend("topleft", c("Positive","Negative"), fill=c(0,1), bty="n")

## Target of Attacks in TV Debates
table(Target=test$objectparty, Actors=test$actorparty)
round(prop.table(table(Target=test$objectparty, Actors=test$actorparty), 2),2)

# 2. Most common topics
## - Most negative topics
## - Most positive topics
# 3. Evolution of topics during campaign
# 4. Evolution of topic sentiment campaign
