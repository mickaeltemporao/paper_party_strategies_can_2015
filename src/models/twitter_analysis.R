#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     twitter_analysis.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-12-10 11:22:02
# Modified:     2016-12-10 12:06:12
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(quanteda)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(SnowballC)


#### Load the datasets and dictionaries
source('src/features/build_twitter_features.R')
dict_fr <- dictionary(file = "frlsd.cat", format = "wordstat")

# Convert to tm corpus and use its API for some additional fun
corpus <- Corpus(VectorSource(d$message[d$lang=='english']))  # Create corpus object
# Remove stop words. This could be greatly expanded!
corpus <- tm_map(corpus, removeWords, stopwords("en"), mc.cores=1)
# Remove numbers. This could have been done earlier, of course.
corpus <- tm_map(corpus, removeNumbers, mc.cores=1)
# Stem the words. Google if you don't understand
corpus <- tm_map(corpus, stemDocument, mc.cores=1)

# Remove the stems associated with our search terms!
account_names <- unique(c(account_names, unique(d$source)))
temp_words <- c("elxn42", "cdnpoli", "retweet", "elxn", "aug", "sep", "oct",
                "client", "www", "twitter", "blackberri", "iphone", "hootsuit",
                "web", "tweetdeck", "iphon", "com", "green", "ndp", "lpc",
                "canadiangreen", "liber", "realchang", "android", "elizabethmay",
                "trudeau", "mulcair", "readychang", "twimg", "tom", "ipad",
                "temptrudeau", "chang", "justin", "harper", "stephen", "justin",
                "canadian", "will", "harper", "candid", "yyj", "today", "canada",
                "gpc", "votegreen", "ago", "hour", "just", "want", "one", "make",
                "get", "neet", "can", "say", "tpp", "last", "may", "qpgpc", "see",
                "time", "teamtrudeau", "tmpm", "take", "tonight", "now", "watch",
                "live", "day", "vancouv", "faceafacetva", "need", "let", "look",
                "ready4chang", "conservative", "munkdeb", "parti", "elizabeth",
                "greenparty", "greenparti", "year", "macdeb", unique(d$source))
corpus <- tm_map(corpus, removeWords, temp_words, mc.cores=1)

# Visualize the corpus
pal <- brewer.pal(8, "Set1")
png(paste0('reports/figures/', today, '_wordcloud.png'))
wordcloud(corpus, min.freq=2, max.words = 150, random.order = TRUE, col = pal)
dev.off()

# Get the lengths and make sure we only create a DTM for tweets with
# some actual content
#TODO: add meta information
doc_leng <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc_leng > 0])
# model <- LDA(dtm, 10)  # Go ahead and test a simple model if you want

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
