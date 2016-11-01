#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Party Strategies Canada 2015
# Filename:     analysis.R
# Description:  Descriptive statistics of party strategies in Canada 2015
# Version:      0.0.0.000
# Created:      2016-05-09 11:06:35
# Modified:     2016-11-01 15:25:00
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
rm(list=ls())
src = list.files('src/features', pattern="*.R")
sapply(paste0('src/features/',src),source,.GlobalEnv)

# 1. Analysis of all data sets available ---------------------------------------
# voir s'il y a une tendance dans le temps
# est-ce que cette tendance est distincte entre les francais et anglais

### Actor Party
object  <- c('actorparty', 'objectparty') # i <- object[1]
type    <- unique(Data$type_source) # j <- type[1]
figure  <- 'pos_neg'

Data <- Data[!is.na(Data$direction),]
Data <- filter(Data, actorparty %in% c('CPC', 'NDP', 'LPC'))

# Direction of quasi sentences
ggplot(Data, aes(actorparty)) +
  geom_bar(aes(fill=direction))
ggsave(paste0('reports/figures/tvads_sent_count_dir.png'), width = 7, height = 7)

ggplot(Data, aes(actorparty)) +
  geom_bar(aes(fill=direction), position='fill')
ggsave(paste0('reports/figures/tvads_sent_prop_dir.png'), width = 7, height = 7)

# Language of quasi sentences
ggplot(Data, aes(actorparty)) +
  geom_bar(aes(fill=language))
ggsave(paste0('reports/figures/tvads_sent_count_lang.png'), width = 7, height = 7)

ggplot(Data, aes(actorparty)) +
  geom_bar(aes(fill=language), position='fill') +
ggsave(paste0('reports/figures/tvads_sent_prop_lang.png'), width = 7, height = 7)

#
ggplot(filter(Data, language=='en'), aes(actorparty)) +
  geom_bar(aes(fill=direction))
ggsave(paste0('reports/figures/tvads_sent_count_dir_en.png'), width = 7, height = 7)

ggplot(filter(Data, language=='fr'), aes(actorparty)) +
  geom_bar(aes(fill=direction))
ggsave(paste0('reports/figures/tvads_sent_count_dir_fr.png'), width = 7, height = 7)

#
ggplot(filter(Data, language=='fr'), aes(actorparty)) +
  geom_bar(aes(fill=direction), position='fill')
ggsave(paste0('reports/figures/tvads_sent_prop_dir_fr.png'), width = 7, height = 7)

ggplot(filter(Data, language=='en'), aes(actorparty)) +
  geom_bar(aes(fill=direction), position='fill')
ggsave(paste0('reports/figures/tvads_sent_prop_dir_en.png'), width = 7, height = 7)

#### General plots for actorparty, objectparty with counts
for (i in object) {
  for (j in type) {
      plot_data <- Data %>%
        rename_(key=i) %>%
        filter(key %in% c("BQ", "CPC", "GPC", "LPC", "NDP")) %>%
        group_by(unique_id, key, language, direction) %>%
        summarise (n = n()) %>%
        arrange(key, language) %>%
        mutate(df_sum = round(cumsum(n)-0.5*n,0)) %>%
        ungroup %>% group_by(key) %>%
        mutate(freq = paste0(round(n / sum(n)*100, 0),' %')) %>%
        mutate(pos_plot = ifelse(language == 'Negative', n, 0))
      plot_data
      p <- ggplot(plot_data,
        aes(x=reorder(key, -pos_plot), y=n, fill=language)) +
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

#### General plots for actorparty, objectparty with proportions
for (i in object) {
  for (j in type) {
      plot_data <- filter(Data, type_source==j, language!=99) %>%
        rename_(key=i) %>% filter(key %in% c("BQ", "CPC", "GPC", "LPC", "NDP")) %>%
        group_by(key, language) %>% summarise (n = n()) %>%
        arrange(key, language) %>%
        mutate(prop_freq = round(n / sum(n), 2)) %>%
        mutate(df_sum = round(cumsum(prop_freq)-0.5*prop_freq,0)) %>%
        ungroup %>% group_by(key) %>%
        mutate(freq = paste0(round(n / sum(n)*100, 0),' %')) %>%
        mutate(pos_plot = ifelse(language == 'Negative', prop_freq, 0))
      plot_data
      p <- ggplot(plot_data,
        aes(x=reorder(key, -pos_plot), y=n, fill=language)) +
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
      ggsave(paste0('figs/prop_', format(Sys.time(), "%Y%m%d"),'_', j, '_', figure, '_', i, '.pdf'), width = 7, height = 7)
  }
}


#### Box plot and table for expert surveys
j <- 'Experts'
plot_data <- subset(Data, type_source==j) %>% group_by(party)

output <- round(prop.table(table(Actors=plot_data$party, Target=plot_data$q2), 1), 2)
print(output)
write.csv(output, paste0('figs/tables/', j,'_xtarget_yactors', '.csv'))

p <- ggplot(plot_data, aes(party, q1))
p + geom_boxplot() +
  xlab('')+ ylab('') +
  theme_wsj()+
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))+
  coord_flip()
ggsave(paste0('figs/boxplot', '_experts_q1', '_', '.pdf'), width = 7, height = 7)

#### Tables
# Negative sentences targetting other parties
for (j in type) {
  table_data <- filter(Data,type_source==j, language=='Negative', actorparty!=99, objectparty!=99) %>%
    mutate(targeting_others = ifelse(objectparty == actorparty, 0, 1)) %>%
    filter(targeting_others == 1)
  output <- round(prop.table(table(Actors=table_data$actorparty, Target=table_data$objectparty ), 1),2)
  print(output)
  write.csv(output, paste0('figs/tables/', j,'_xtarget_yactors', '.csv'))
}

j <- 'Ads_TV'
for (i in unique(Data$post)) {
  table_data <- filter(Data,type_source==j, post ==i, language=='Negative', actorparty!=99, objectparty!=99) %>%
    mutate(targeting_others = ifelse(objectparty == actorparty, 0, 1)) %>%
    filter(targeting_others == 1)
  output <- round(prop.table(table(Actors=table_data$actorparty, Target=table_data$objectparty ), 1),2)
  print(output)
  write.csv(output, paste0('figs/tables/', j,'_xtarget_yactors', ifelse(i==1, 'post', 'pre'), '.csv'))
}

# Data summaries sentences by ads by party
Data <- tbl_df(Data)

# TV Ads summaries
output <- Data %>% filter(type_source=='Ads_TV') %>%
  select(actorparty, unique_id) %>%
  group_by(actorparty, unique_id) %>%
  summarise(count = n()) %>%
  summarise_each(funs(n(), sum, mean, sd)) %>%
  select(-contains('unique_id'))
names(output) <- c('actorparty', 'ads_count', 'sent_count', 'avg_sent', 'sd_sent')
print(output)
write.csv(output, paste0('figs/tables/summaries_', 'Ads_TV', '.csv'))

# Debates summaries
debs <- type[2:4]
for (j in debs) {
  output <- Data %>% filter(type_source == j) %>% select(actorparty, language) %>%
    group_by(actorparty, language) %>%
    summarise(count = n())
  print(output)
  write.csv(output, paste0('figs/tables/summaries_', j, '.csv'))
}
