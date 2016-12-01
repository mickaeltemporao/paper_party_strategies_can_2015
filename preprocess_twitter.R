#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     TwitterTopics.r
# Description:  Twitter Topic Modeling Using R
# Version:      0.0.0.000
# Created:      2016-10-17 20:15:03
# Modified:     2016-12-01 16:41:04
# Author:       Mickael Temporão, based on Bryan Goodrich TwitterTopics.R
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
library(twitteR)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(SnowballC)
library(textcat)

d <- openxlsx::read.xlsx('data/twitter_feeds_can2016.xlsx')

#### Preprocess Tweets --------------------------------
acounts_names <- gsub("([A-Za-z]+).*", "\\1", d$message) # Extract first word of sentence (account name)

tweets <- d$message
tweets <- gsub("pbs\\.twimg.+","",tweets)        # Remove source in tweest
tweets <- iconv(tweets, to = "ASCII", sub = " ") # Convert to basic ASCII text to avoid silly characters
tweets <- tolower(tweets)                        # Make everything consistently lower case
tweets <- gsub("^(\\w+)", "", tweets)            # Remove first word of sentence (account name here)
tweets <- gsub("rt", " ", tweets)                # Remove the "RT" (retweet) so duplicates are duplicates
tweets <- gsub("@\\w+", " ", tweets)             # Remove user names (all proper names if you're wise!)
tweets <- gsub("http.+ |http.+$", " ", tweets)   # Remove links
tweets <- gsub("[[:punct:]]", " ", tweets)       # Remove punctuation
tweets <- gsub("[ |\t]{2,}", " ", tweets)        # Remove tabs
tweets <- gsub("amp", " ", tweets)               # "&" is "&amp" in HTML, so after punctuation removed ...
tweets <- gsub("^ ", "", tweets)                 # Leading blanks
tweets <- gsub(" $", "", tweets)                 # Lagging blanks
tweets <- gsub(" +", " ", tweets)                # General spaces (should just do all whitespaces no?)
                                                 # tweets <- unique(tweets)  # Now get rid of duplicates!

## Get language
d$lang <- textcat(tweets)
d$lang[d$lang %in% c('scots')] <- 'english'
x <- 8
prop <- round(prop.table(table(ifelse(d$lang %in% c('english', 'french'), 1,0)))[1],2)
sprintf("Proports of non english-french tweets : %.3f", prop)

# Convert to tm corpus and use its API for some additional fun
corpus <- Corpus(VectorSource(tweets[d$lang=='english']))  # Create corpus object

# Remove stop words. This could be greatly expanded!
# Don't forget the mc.cores thing
corpus <- tm_map(corpus, removeWords, stopwords("en"), mc.cores=1)

# Remove numbers. This could have been done earlier, of course.
corpus <- tm_map(corpus, removeNumbers, mc.cores=1)

# Stem the words. Google if you don't understand
corpus <- tm_map(corpus, stemDocument, mc.cores=1)

# Remove the stems associated with our search terms!
account_names <- c(account_names, unique(d$source))
temp_words <- c("elxn42", "cdnpoli", "retweet", "elxn", "aug", "sep", "oct",
                "client", "www", "twitter", "blackberri", "iphone", "hootsuit",
                "web", "tweetdeck", "iphon", "com", "green", "ndp", "lpc",
                "canadiangreen", "liber", "realchang", "android", "elizabethmay",
                "trudeau", "mulcair", "readychang", "twimg", "tom", "ipad",
                "temptrudeau", "chang", "justin", "harper", "stephen", "justin",
                "canadian", "will", "harper", "candid", account_names)

corpus <- tm_map(corpus, removeWords, temp_words, mc.cores=1)

# Visualize the corpus
pal <- brewer.pal(8, "Dark2")
wordcloud(corpus, min.freq=2, max.words = 150, random.order = TRUE, col = pal)


# Get the lengths and make sure we only create a DTM for tweets with
# some actual content
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

FindTopicsNumber_plot(result)

# Set the optimal number of topics
SEED = 1 # Pick a random seed for replication
k    = 6 # Let's start with 10 topics

# This might take a minute!
models <- list(
    CTM       = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
    VEM       = LDA(dtm, k = k, control = list(seed = SEED)),
    VEM_Fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
    Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                 thin = 100,    iter = 1000))
)

# There you have it. Models now holds 4 topics. See the topicmodels API documentation for details

# Top 10 terms of each topic for each model
# Do you see any themes you can label to these "topics" (lists of words)?
lapply(models, terms, 10)

# matrix of tweet assignments to predominate topic on that tweet
# for each of the models, in case you wanted to categorize them
assignments <- sapply(models, topics)
