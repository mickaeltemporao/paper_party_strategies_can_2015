#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-09 11:06:35
# Modified:     2016-05-15 23:45:06
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

src = list.files('src/', pattern="*.R")
sapply(paste0('src/',src),source,.GlobalEnv)

# 1. Analysis of Advertisements -----------------------------------------------
## TV Ads

### Actor Party
type    <- c( "Ads_TV", "Deb_RC", "Deb_TVA", "Experts")
figure  <- 'pos_neg'
object  <- c('actorparty', 'objectparty')

#### General Plot
for (i in object) {
  for (j in type) {
    plot_data <- filter(Data, type_source==j, direction!=99) %>%
      rename_(key=i) %>% filter(key %in% c("BQ", "CPC", "GPC", "LPC", "NDP")) %>%
      group_by(key, direction) %>% summarise (n = n()) %>%
      arrange(key, direction) %>%
      mutate(df_sum = round(cumsum(n)-0.5*n,0)) %>% ungroup %>% group_by(key) %>%
      mutate(freq = paste0(round(n / sum(n)*100, 0),' %')) %>%
      mutate(pos_plot = ifelse(direction == 'Negative', n, 0))
    plot_data
    p <- ggplot(plot_data,
      aes(x=reorder(key, -pos_plot), y=n, fill=direction)) +
      geom_bar(stat="identity")+
      geom_text(aes(y=df_sum, label=freq), vjust=1,
        color="white", size=3.5)+
      scale_fill_grey(name='')+
      xlab('')+ ylab('')+
      theme_wsj()+
      theme(legend.position='top',
        legend.background= element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))
    ggsave(paste0('figs/', j, '_', figure, '_', i, '.pdf'), width = 7, height = 7)
  }
}

# Tables

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

# Expert survey-----------------

round(prop.table(table(Actors=Exp$party, Target=Exp$q2), 1), 2)

## - Most negative topics
## - Most positive topics
# 3. Evolution of topics during campaign
# 4. Evolution of topic sentiment campaign

# A
