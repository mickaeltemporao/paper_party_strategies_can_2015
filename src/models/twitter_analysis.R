#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     twitter_analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-12-10 11:22:02
# Modified:     2016-12-10 19:03:40
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(RColorBrewer)
library(SnowballC)
library(quanteda)
library(topicmodels)


#### Load the datasets/dictionaries --------------------------------
source('src/features/build_twitter_features.R')
source('src/dictionaries/removed_words.R')
policy_agendas <- dictionary(file = "src/dictionaries/policy_agendas_english.ykd")


#### Create Corpus --------------------------------
twitter_corpus <- corpus(data, text_field= "message")
summary(twitter_corpus, 5)


#### Top Features --------------------------------
wc_dfm <- dfm(twitter_corpus, stem = F, remove = c(rm_words, stopwords("english")))
topfeatures(wc_dfm, 50)

# Visualize the corpus
png(paste0('reports/figures/', today, '_wordcloud.png'))
plot(wc_dfm,
     min.freq=2,
     max.words = 100,
     col = brewer.pal(8, "Set1"))
dev.off()


# XXX
# Select number of topics for LDA model
library("ldatuning")
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
