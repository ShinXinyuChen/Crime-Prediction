library(data.table)
library(qdap)
library(qdapDictionaries)
library(tm)


############# Data cleanning ############
# replace emotico to english word
out.2013Mar.r$text1 <- NA
out.2013Mar.r$text1 <- mgsub(as.character(emoticon[[2]]), as.character(emoticon[[1]]), out.2013Mar.r$text)
#emoticon[[1]]
# clear punctuation 
out.2013Mar.r$text1<- gsub('[[:punct:]]', ' ', out.2013Mar.r$text1)
# take control charactor out, need or not? 
#out.2013Mar13pm.dt$text1<- gsub('[[:cntrl:]]', ' ', out.2013Mar13pm.dt$text1)
#
out.2013Mar.r$text1<- gsub('\\d+', ' ', out.2013Mar.r$text1)
out.2013Mar.r$text1<- tolower(out.2013Mar.r$text1)

twitter.clean <- function(doc){
  doc$text1 <- NA
  doc$text1 <- mgsub(as.character(emoticon[[2]]), as.character(emoticon[[1]]), doc$text)
  #emoticon[[1]]
  # clear punctuation 
  out.2013Mar.r$text1<- gsub('[[:punct:]]', ' ', out.2013Mar.r$text1)
  # take control charactor out, need or not? 
  #out.2013Mar13pm.dt$text1<- gsub('[[:cntrl:]]', ' ', out.2013Mar13pm.dt$text1)
  #
  out.2013Mar.r$text1<- gsub('\\d+', ' ', out.2013Mar.r$text1)
  out.2013Mar.r$text1<- tolower(out.2013Mar.r$text1)
}
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
    # something funky happens to the encoding of just one of the rows. easier to blow it away (6909:na\xefve -1)
    ##??POLKEY<- POLKEY[-5963,]
    # POLKEY$x to lower
    POLKEY$x<- tolower(POLKEY$x)
    setkey(POLKEY,x)  

#save(POLKEY, file = "POLKEY.RData")
