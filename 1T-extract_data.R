# Load Requried Packages
library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")

# Authonitical keys
consumer_key <- 'tAyR9LyhATfD90aA7Ft1Zfj3I'
consumer_secret <- 'vX1RHqHHDpnmNOqrGPMVnmnQjQvG98X3xlB7T7zv4hKcvj7tVv'
access_token <- '2572842085-vExbB4HNvN57zmQhoQdbmutC16a4kdMdh1xVta5'
access_secret <- 'HtTHSeAOz1WPcUX8nfW5ddwZ1TbXZGFB4pSHU0IZ3agvA'

twitteR::setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- userTimeline("turkishairlines", n=200)

tweets.df <- twListToDF(tweets) 

retweets("964471378329075713", n = 20)
