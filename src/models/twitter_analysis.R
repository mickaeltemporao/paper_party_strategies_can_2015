#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     twitter_analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-12-10 11:22:02
# Modified:     2016-12-13 15:23:46
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

#### Load the datasets/dictionaries --------------------------------
library(quanteda)
source('src/features/build_twitter_features.R')
source('src/dictionaries/removed_words.R')
policy_agendas <- dictionary(file = "src/dictionaries/policy_agendas.lc3")


#### Create Corpus --------------------------------
tw_corpus <- corpus(data, text_field= "message")
summary(tw_corpus, 5)


#### Top Features --------------------------------
wc_dfm <- dfm(tw_corpus, stem = F, remove = c(rm_words, stopwords("english")))
topfeatures(wc_dfm, 50)

# Visualize the corpus
library(RColorBrewer)
png(paste0('reports/figures/', today, '_wordcloud.png'))
plot(wc_dfm,
     min.freq=2,
     max.words = 100,
     col = brewer.pal(8, "Dark2"))
dev.off()


#### Extracting Topics --------------------------------
topics_dfm <- dfm(tw_corpus,
              dictionary = policy_agendas,
              remove = c(stopwords("english")),
              stem = F)

# Convert to data frame and recode to dummies
tw_topics <- as.data.frame(topics_dfm)
tw_topics <- as.data.frame(ifelse(tw_topics == 0, 0, 1))
names(tw_topics) <- paste0("topic_",names(tw_topics))
# bind topics to original data
data <- cbind(data, tw_topics)


#### Plot Topics over time --------------------------------
library(ggplot2)
library(ggthemes)
library(tidyr)

## Prepare data to tidy format ----------------
plot_data <- data %>%
  dplyr::select(id, source, date, contains("topic")) %>%
  gather(key=topic, value=value, -id, -source, -date) %>%
  dplyr::filter(value == 1)
plot_data$topic <- gsub("topic_", "", plot_data$topic)
plot_data$topic <- gsub("_|-", "", plot_data$topic)

## Bar Plots ----------------
# Average topics
ggplot(plot_data, aes(x=topic)) +
  geom_bar(col='black', size=0.3) +
  theme_fivethirtyeight() +
  ggtitle('Topics per tweet') +
  scale_x_discrete(label=abbreviate) +
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_twitter_topics.png'), width = 10, height = 3)

# Average topics by party
ggplot(plot_data, aes(x=topic)) +
  geom_bar(col='black', size=0.3) +
  theme_fivethirtyeight() +
  ggtitle('Topics per tweet') +
  scale_x_discrete(label=abbreviate) +
  facet_grid(source ~ .) +
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_twitter_topics_by_party.png'), width = 11, height = 7)

## Time Series ----------------
# All topics over time
ggplot(plot_data, aes(x=date, fill=topic)) +
  geom_bar() +
  theme_fivethirtyeight() +
  ggtitle('Topics per tweet during campaign') +
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_twitter_topics_all_ts_byparty.png'))

# Filter top 5 topics
temp <- sort(table(plot_data$topic), decreasing=T)[1:5]
top <- names(temp)

# All topics over time by party
ggplot(dplyr::filter(plot_data, topic %in% top), aes(x=date, fill=topic)) +
  geom_bar(width=1, col='black', size=0.15) +
  theme_fivethirtyeight() +
  ggtitle('Topics per tweet during campaign by party') +
  facet_grid(source ~ .) +
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_twitter_topics_all_ts_by_party.png'))

# All topics over time by party
ggplot(dplyr::filter(plot_data, topic %in% top), aes(x=date, fill=topic)) +
  geom_bar(width=1, col='black', size=0.15, position="fill") +
  theme_fivethirtyeight() +
  ggtitle('Proportion of Topics per tweet during campaign by party') +
  facet_grid(source ~ .) +
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_twitter_topics_all_ts_by_party_prop.png'))

# XXX
# Select number of topics for LDA model
library(ldatuning)
library(SnowballC)
library(topicmodels)
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
png(paste0('reports/figures/', today, '_topics_diagnostic.png'))
FindTopicsNumber_plot(result)
dev.off()
# Set the optimal number of topics
SEED = 1 # Pick a random seed for replication
k    = 7 # Let's start with 10 topics
# This might take a minute!
models <- list(
    CTM       = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
    VEM       = LDA(dtm, k = k, control = list(seed = SEED)),
    VEM_Fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
    Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                 thin = 100,    iter = 1000))
)
# There you have it. Models now holds 4 topics. See the topicmodels API documentation for details
lapply(models, terms, 10)
write.csv(terms(models[[1]], 10), paste0('reports/', today, '_topics_top10.csv'), row.names=F)
# matrix of tweet assignments to predominate topic on that tweet
# for each of the models, in case you wanted to categorize them
assignments <- sapply(models, topics)

doc_leng
d$message <- tweets # Create corpus object
d$lda_gibs <- NA

dim(d[d$lang=='english' & is.na(d$lang),])
cbind(d[d$lang=='english' & !is.na(d$lang),], as.data.frame(assignments)$Gibbs)

dim(d[d$lang=='english',])
