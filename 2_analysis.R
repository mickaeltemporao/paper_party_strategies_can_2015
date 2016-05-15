#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-09 11:06:35
# Modified:     2016-05-14 11:39:18
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

src = list.files('src/', pattern="*.R")
sapply(paste0('src/',src),source,.GlobalEnv)

# Analysis of TV Data Data ------------------------------------------------------

## Positive negative debate by party

counts <- table(Data$direction, Data$actorparty)
counts <- round(prop.table(counts, 2), 2)

g <- ggplot(Data, aes(x=reorder(actorparty, direction), fill=factor(direction) ))
g + geom_bar( position='fill') +
  theme(axis.text=element_text(size=60),
        axis.title=element_text(size=60,face="bold")) +
  scale_fill_brewer(palette="Paired") +
  coord_flip()+
  theme_bw()

#TODO: by avant/après
pdf('figs/pos_neg.pdf')
barplot(reorder(counts, party), main='Positive-Negative Sentences in TV Debates', horiz=TRUE
  #ylim=c(0,200)
)
#legend("bottom", c("Positive","Negative"), fill=c(0,1), bty="n")
dev.off()


## 2 bar plot
# 1 pour actor (combien de fois le party parle + pos neg a lintérieur)
# 1 pour object (combien de x on parle du party + pos neg a lint)

# TABLEAU XXXX
Data <- subset(Data, direction==0)
Data <- subset(Data, objectparty!=99)
pre <- subset(pre, direction==0)
pre <- subset(pre, objectparty!=99)
post <- subset(post, direction==0)
post <- subset(post, objectparty!=99)

# Data <- subset(Data, objectparty!='BQ')
# pre <- subset(pre, objectparty!='BQ')
# post <- subset(post, objectparty!='BQ')

# Negative sentences targetting other parties
Data$test <- 0
Data$test[Data$objectparty==Data$actorparty] <- 1
table1 <- subset(Data,test !=1)
table(Target=table1$objectparty, Actors=table1$actorparty)
round(prop.table(table(Actors=table1$actorparty, Target=table1$objectparty ), 1),2)

# TODO:Proportions par + prop direction + value
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
round(prop.table(table(Data$objectparty)), 2)
barplot(round(prop.table(table(Data$objectparty)), 2))

round(prop.table(table(Actors=Data$actorparty, Target=Data$objectparty), 1), 2)
round(prop.table(table(Actors=pre$actorparty, Target=pre$objectparty), 1), 2)
round(prop.table(table(Actors=post$actorparty, Target=post$objectparty), 1), 2)

#TODO: Order by negativePositive-Negative Sentences in all TV Data
c_all <- round(prop.table(table(Data$direction, Data$actorparty), 2), 2)
c_pre <- round(prop.table(table(pre$direction, pre$actorparty), 2), 2)
c_post <- round(prop.table(table(post$direction, post$actorparty), 2), 2)
c_post[2,] <- sort(c_post[2,])
par(mfrow = c(3, 1))
layout(matrix(c(0,0,0,1,1,1,1,1,0,0,0,2,2,2,2,2,0,3,3,3,3,3), 11, 2))

barplot(c_all, main='All Campaign', horiz=TRUE)
barplot(c_pre, main='Pre', horiz=TRUE)
barplot(c_post, main='Post', horiz=TRUE)

# Most common topics
table(Data$actorparty, Data$traittype)

# Expert survey-----------------

round(prop.table(table(Actors=Exp$party, Target=Exp$q2), 1), 2)

## - Most negative topics
## - Most positive topics
# 3. Evolution of topics during campaign
# 4. Evolution of topic sentiment campaign

# A
