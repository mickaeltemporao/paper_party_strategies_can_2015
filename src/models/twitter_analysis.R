#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     twitter_analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-12-10 11:22:02
# Modified:     2016-12-20 09:25:30
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
sentiment      <- dictionary(file = "src/dictionaries/sentiment.lc3")
civi_rights    <- dictionary(file = "src/dictionaries/civil_rights.lc3")
span           <- 1


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

#### Extracting Sentiments --------------------------------
sentiment_dfm <- dfm(tw_corpus,
              dictionary = sentiment,
              remove = c(stopwords("english")),
              stem = F)

# Convert to data frame and recode to dummies
tw_sentiment        <- as.data.frame(sentiment_dfm)
#TODO: Code sentiment
#tw_sentiment <- as.data.frame(ifelse(tw_sentiment == 0, 0, 1))
names(tw_sentiment) <- c("sent_negative", "sent_positive")
# bind topics to original data
data                <- cbind(data, tw_sentiment)
data$sent_negative  <- -data$sent_negative
data$sent_dir        <- data$sent_positive + data$sent_negative

#### Plot Topics over time --------------------------------
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)

## Prepare data to tidy format ----------------
plot_data <- data %>%
  dplyr::select(id, source, date, contains("topic")) %>%
  gather(key=topic, value=value, -id, -source, -date) %>%
  dplyr::filter(value == 1)
plot_data$topic <- gsub("topic_", "", plot_data$topic)
plot_data$topic <- gsub("_|-", "", plot_data$topic)
plot_data$sent_negative <- -plot_data$sent_negative

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
ggsave(
  paste0('reports/figures/twitter/',
         format(Sys.time(), "%Y%m%d"),
         '_twitter_topics_all_ts_by_party_prop.png')
  )

# Top 5 Tweets by party per day
for (i in unique(data$source)) {
  temp_data <- dplyr::filter(plot_data, source==i)
  temp <- sort(table(temp_data$topic), decreasing=T)[1:5]
  top <- names(temp)
  pl<-ggplot(dplyr::filter(temp_data, topic %in% top), aes(x=date, fill=topic)) +
    geom_bar(width=1, col='black', size=0.15) +
    theme_fivethirtyeight() +
    ggtitle(paste0(i, ' Top 5 topics in tweets during campaign by party')) +
    theme(
      legend.background= element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"))
    print(pl)
 ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_', i, '_twitter_top5_ts.png'))
}

#### Sentiment --------------------------------
plot_data <- data %>%
  dplyr::select(id, source, date, sent_dir)

# Negative Count per ad per day
d <- data %>%
  group_by(date, source) %>%
  dplyr::filter(sent_dir<0) %>%
  summarize(target = n())

ggplot(d, aes(x=date, y=target, col=source)) +
  geom_jitter(size = 2, alpha=0.4) +
  geom_smooth(se=F, alpha=.5) +
  theme_fivethirtyeight() +
  ggtitle('Count of Negative Tweets by day per Party') +
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_', 'tw_neg_counts_by_day.png'), width=7, height=7)

#TODO: Negative proportion per ad per day
temp <- data
temp$simp_dir <- ifelse(d$sent_dir > 0, 1,
                ifelse(d$sent_dir < 0, -1, 0)
              )
d <- temp %>%
  group_by(date, source, simp_dir) %>%
  summarize(n = n()) %>%
  mutate(target = round(n / sum(n),2)) %>%
  ungroup %>%
  dplyr::filter(simp_dir <= -1) %>% print

ggplot(d, aes(x=date, y=target)) +
  geom_jitter(aes(color=source), alpha=0.4)+
  geom_smooth(aes(color=source), se=F) +
  theme_fivethirtyeight() +
  ggtitle('Proportion of Negative tweets per party by day') +
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_', 'tw_neg_prop_by_day.png'), width=7, height=7)


# Avg Direction per day
d <- data %>%
  group_by(date, source) %>%
  arrange(date) %>%
  summarize(direction=mean(sent_dir)) %>%
  print
ggplot(d, aes(x=date, y=direction)) +
  geom_jitter(aes(color=source), alpha=0.2) +
  geom_smooth(aes(color=source), se=F) +
  theme_fivethirtyeight() +
  ggtitle('Average Direction per day by Party') +
  theme(
    legend.background= element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"))
ggsave(paste0('reports/figures/twitter/', format(Sys.time(), "%Y%m%d"), '_', 'tw_avg_dir_by_party_by_day.png'), width=7, height=7)


# XXX ------------------------------
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
