#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-09 11:06:35
# Modified:     2016-05-16 12:01:57
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

src = list.files('src/', pattern="*.R")
sapply(paste0('src/',src),source,.GlobalEnv)

# 1. Analysis of all data sets available ---------------------------------------

### Actor Party
object  <- c('actorparty', 'objectparty')
type    <- unique(Data$type_source)
figure  <- 'pos_neg'

#### General Plot for actorparty, objectparty with counts
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
      ggsave(paste0('figs/counts_', j, '_', figure, '_', i, '.pdf'), width = 7, height = 7)
  }
}

#### General Plot for actorparty, objectparty with proportions
for (i in object) {
  for (j in type) {
      plot_data <- filter(Data, type_source==j, direction!=99) %>%
        rename_(key=i) %>% filter(key %in% c("BQ", "CPC", "GPC", "LPC", "NDP")) %>%
        group_by(key, direction) %>% summarise (n = n()) %>%
        arrange(key, direction) %>%
        mutate(prop_freq = round(n / sum(n), 2)) %>%
        ungroup %>% group_by(key) %>%
        mutate(freq = paste0(round(n / sum(n)*100, 0),' %')) %>%
        mutate(pos_plot = ifelse(direction == 'Negative', prop_freq, 0))
      plot_data
      p <- ggplot(plot_data,
        aes(x=reorder(key, n), y=n, fill=direction)) +
        geom_bar(stat="identity", position ='fill') +
        geom_text(aes(y=pos_plot, label=freq), vjust=2,
          color="white", size=3.5)+
        scale_fill_grey(name='') +
        xlab('')+ ylab('') +
        theme_wsj() +
        theme(legend.position='top',
          legend.background= element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"))

      ggsave(paste0('figs/prop_', j, '_', figure, '_', i, '.pdf'), width = 7, height = 7)
  }
}

# Tables

# Negative sentences targetting other parties
table_data <- filter(Data,type_source=='Ads_TV', direction=='Negative', actorparty!=99, objectparty!=99) %>%
  mutate(targeting_others = ifelse(objectparty == actorparty, 0, 1)) %>%
  filter(targeting_others == 1)

table(Target=table_data$objectparty, Actors=table_data$actorparty)
test_table <- round(prop.table(table(Actors=table_data$actorparty, Target=table_data$objectparty ), 1),2)

table_data <- filter(Data,type_source=='Ads_TV', direction=='Negative', actorparty!=99, objectparty!=99) %>%
  mutate(targeting_others = ifelse(objectparty == actorparty, 0, 1)) %>%
  filter(targeting_others == 1)

table(Target=table_data$objectparty, Actors=table_data$actorparty)
test_table <- round(prop.table(table(Actors=table_data$actorparty, Target=table_data$objectparty ), 1),2)

table_data <- filter(Data,type_source=='Ads_TV', direction=='Negative', actorparty!=99, objectparty!=99) %>%
  mutate(targeting_others = ifelse(objectparty == actorparty, 0, 1)) %>%
  filter(targeting_others == 1)

table(Target=table_data$objectparty, Actors=table_data$actorparty)
test_table <- round(prop.table(table(Actors=table_data$actorparty, Target=table_data$objectparty ), 1),2)

table(Target=table_data$objectparty, Actors=table_data$actorparty)
test_table <- round(prop.table(table(Actors=table_data$actorparty, Target=table_data$objectparty ), 1),2)
write.csv(test_table, paste0('figs/tables/', 'test_name', '.csv'))

round(prop.table(table(Actors=Data$actorparty, Target=Data$objectparty), 1), 2)
round(prop.table(table(Actors=pre$actorparty, Target=pre$objectparty), 1), 2)
round(prop.table(table(Actors=post$actorparty, Target=post$objectparty), 1), 2)

# Expert survey-----------------
test <- subset(Data, type_source=='Experts')
round(prop.table(table(Actors=Exp$party, Target=Exp$q2), 1), 2)

test_data <- Experts %>% group_by(party)

p <- ggplot(Experts, aes(party, q1))
p + geom_boxplot() +
  xlab('')+ ylab('') +
  theme_wsj()+
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))+
  scale_x_discrete(labels=c("ndp"="NDP","bloc"="BQ","lpc"="LPC","cpc"="CPC", "green"="GPC"))+
  coord_flip()

