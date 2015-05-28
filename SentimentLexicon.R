library(data.table)
library(qdap)
library(tm)

########## positive lexicons ###########

# import positive lexicons from your local directory defined in earlier step
# anonther lexican dictionary: http://mpqa.cs.pitt.edu/
pos<- scan("positive-words.txt", what='character', comment.char=';')
#save(pos, file = "pos.RData")

########## negative lexicons ############

# import negative lexicons from your local directory defined in earlier step
# http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
neg<- scan("negative-words.txt", what='character', comment.char=';')
save(neg, file = "neg.RData")

# manually adding posisive and negative words
pos.words<- c(pos, "sweet", "tight", "badass", "cool", "awesome", "funny", "fun",
  "hilarious", "lol")
  
neg.words<- c(neg, "suck", "sucked","sucking", "sucks", "blow", "blows", "blowing", "blowed", "shit", "shity", 
              "shits","shiting", "stink", "stinks", "coward", "cowards", "asshole", "jackass", "jerk", "jerks")

# rm(list = c('neg','neg_finance', 'neg_all', 'pos', 'pos_finance', 'pos_all'))

# check length of each lexicon
length(unique(pos.words))
length(unique(neg.words))

# remove duplicates for tiddiness 
pos.words<- unique(pos.words)
neg.words<- unique(neg.words)
# save(pos.words, file = "pos.words.RData")
# save(neg.words, file = "neg.words.RData")

# Create researcher defined sentiment.frame
POLKEY<- sentiment_frame(pos.words, neg.words)
    # POLKEY$x to lower
    POLKEY$x<- tolower(POLKEY$x)
    setkey(POLKEY,x)  

#save(POLKEY, file = "POLKEY.RData")
