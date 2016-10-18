# Twitter Topic Modeling Using R
# Author: Bryan Goodrich
# Date Created: February 13, 2015
# Last Modified: April 3, 2015
#
# Use twitteR API to query Twitter, parse the search result, and
# perform a series of topic models for identifying potentially
# useful topics from your query content. This has applications for
# social media, research, or general curiosity
#
# Reference
# http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
# https://blog.credera.com/business-intelligence/twitter-analytics-using-r-part-2-create-word-cloud/


# Load relevant libraries
# Use install.packages("package") to install any you don't have
# If, like me, you're on Linux and cannot install the topicmodels
# package because of something about missing a GSL component. You'll
# need to install the library and development library. If, like me,
# you're on a Debian based system, it's pretty easy
#   sudo apt-get install libgsl0ldbl libgsl0-dev
library(twitteR)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(SnowballC)


# Populate with your https://apps.twitter.com application information
# to use my_setup() to register your session. No, you cannot see mine!
my_setup <- function() {
    ckey = "Your Consumer Key Here"
    csec = "Your Consumer Secret Here"
    akey = "Your Access Token Here"
    asec = "Your Access Token Secret Here"

    setup_twitter_oauth(ckey, csec, akey, asec)
}

# Convenience function for accessing the text part of a tweet
# returned by the twitteR API. Is used below.
tweet_text <- function(x) x$getText()

# Submit a search query (terms separated by "+") and get a return
# set of data (corpus).
tweet_corpus <- function(search, n = 5000, ...) {
    payload <- searchTwitter(search, n = n, ...)
    sapply(payload, tweet_text)
}



# Search for some key terms, try to grab a lot if you want. Twitter will
# limit you as it sees fit (can find). Also has spatial options.
# Try these Sacramento coordinates: '38.630404,-121.293535,50mi'
my_setup()  # Setup the login for this session
tweets <- tweet_corpus("energy+electricity", n = 10000, lang = 'en')

# Save your corpus (because you're limited in how often you can do this for free!)
saveRDS(tweets, file = "Workspace/energy_themes/tweets.Rds", compress = 'xz')

# Okay, read that corpus back in from disk. I'm sure you have a
# different save location, right?
tweets <- readRDS("Workspace/energy_themes/tweets.Rds")



# Okay, here's where things get tricky. See references for examples.
# Problems? Depends if your system is using parallel processing. If
# it is, you'll need to use mc.cores parameters as shown later. That
# took me awhile to get! Thanks to the references for clearing that up.

# Here we pre-process the data in some standard ways. I'll post-define each step
tweets <- iconv(tweets, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
tweets <- tolower(tweets)  # Make everything consistently lower case
tweets <- gsub("rt", " ", tweets)  # Remove the "RT" (retweet) so duplicates are duplicates
tweets <- gsub("@\\w+", " ", tweets)  # Remove user names (all proper names if you're wise!)
tweets <- gsub("http.+ |http.+$", " ", tweets)  # Remove links
tweets <- gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets <- gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets <- gsub("amp", " ", tweets)  # "&" is "&amp" in HTML, so after punctuation removed ...
tweets <- gsub("^ ", "", tweets)  # Leading blanks
tweets <- gsub(" $", "", tweets)  # Lagging blanks
tweets <- gsub(" +", " ", tweets) # General spaces (should just do all whitespaces no?)
tweets <- unique(tweets)  # Now get rid of duplicates!



# Convert to tm corpus and use its API for some additional fun
corpus <- Corpus(VectorSource(tweets))  # Create corpus object

# Remove English stop words. This could be greatly expanded!
# Don't forget the mc.cores thing
corpus <- tm_map(corpus, removeWords, stopwords("en"), mc.cores=1)

# Remove numbers. This could have been done earlier, of course.
corpus <- tm_map(corpus, removeNumbers, mc.cores=1)

# Stem the words. Google if you don't understand
corpus <- tm_map(corpus, stemDocument, mc.cores=1)

# Remove the stems associated with our search terms!
corpus <- tm_map(corpus, removeWords, c("energi", "electr"), mc.cores=1)



# Why not visualize the corpus now?
# Mine had a lot to do with {solar, power, renew, new, can, save, suppl, wind, price, use}
pal <- brewer.pal(8, "Dark2")
wordcloud(corpus, min.freq=2, max.words = 150, random.order = TRUE, col = pal)



# Now for Topic Modeling

# Get the lengths and make sure we only create a DTM for tweets with
# some actual content
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])
# model <- LDA(dtm, 10)  # Go ahead and test a simple model if you want



# Now for some topics
SEED = sample(1:1000000, 1)  # Pick a random seed for replication
k = 10  # Let's start with 10 topics

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
