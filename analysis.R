#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-09 11:06:35
# Modified:     2016-05-16 13:13:55
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

src = list.files('src/', pattern="*.R")
sapply(paste0('src/',src),source,.GlobalEnv)

# 1. Analysis of all data sets available ---------------------------------------

### Actor Party
object  <- c('actorparty', 'objectparty') # i <- object[1]
type    <- unique(Data$type_source) # j <- type[2]
figure  <- 'pos_neg'

#### General plots for actorparty, objectparty with counts
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

#### General plots for actorparty, objectparty with proportions
for (i in object) {
  for (j in type) {
      plot_data <- filter(Data, type_source==j, direction!=99) %>%
        rename_(key=i) %>% filter(key %in% c("BQ", "CPC", "GPC", "LPC", "NDP")) %>%
        group_by(key, direction) %>% summarise (n = n()) %>%
        arrange(key, direction) %>%
        mutate(prop_freq = round(n / sum(n), 2)) %>%
        mutate(df_sum = round(cumsum(prop_freq)-0.5*prop_freq,0)) %>%
        ungroup %>% group_by(key) %>%
        mutate(freq = paste0(round(n / sum(n)*100, 0),' %')) %>%
        mutate(pos_plot = ifelse(direction == 'Negative', prop_freq, 0))
      plot_data
      p <- ggplot(plot_data,
        aes(x=reorder(key, -pos_plot), y=n, fill=direction)) +
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
  table_data <- filter(Data,type_source==j, direction=='Negative', actorparty!=99, objectparty!=99) %>%
    mutate(targeting_others = ifelse(objectparty == actorparty, 0, 1)) %>%
    filter(targeting_others == 1)
  output <- round(prop.table(table(Actors=table_data$actorparty, Target=table_data$objectparty ), 1),2)
  print(output)
  write.csv(output, paste0('figs/tables/', j,'_xtarget_yactors', '.csv'))
}

j <- 'Ads_TV'
for (i in unique(Data$post)) {
  table_data <- filter(Data,type_source==j, post ==i, direction=='Negative', actorparty!=99, objectparty!=99) %>%
    mutate(targeting_others = ifelse(objectparty == actorparty, 0, 1)) %>%
    filter(targeting_others == 1)
  output <- round(prop.table(table(Actors=table_data$actorparty, Target=table_data$objectparty ), 1),2)
  print(output)
  write.csv(output, paste0('figs/tables/', j,'_xtarget_yactors', ifelse(i==1, 'post', 'pre'), '.csv'))
}
