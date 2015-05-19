# LOAD TWITTER PACKAGE
require(twitteR)
require(ROAuth)
t.api.key = ""
t.api.secret = ""
t.api.accesstoken = "-"
t.api.accesssecret = ""
t.reqURL = "https://api.twitter.com/oauth/request_token"
t.accessURL = "https://api.twitter.com/oauth/access_token"
t.authURL = "https://api.twitter.com/oauth/authorize"

t.cred = 
  OAuthFactory$new(
  consumerKey = t.api.key, 
  consumerSecret = t.api.secret,
  requestURL = t.reqURL,
  accessURL = t.accessURL, 
  authURL = t.authURL
)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

t.cred$handshake(cainfo="cacert.pem")

save(t.cred, file="twitcred")
t.cred <- load("twitcred")

# NEW WAY TO SETUP TWITTER
setup_twitter_oauth(t.api.key, t.api.secret, t.api.accesstoken, t.api.accesssecret)

# A. GET TWEETS OF TWO STATES: BOSTON & PHILADELPHIA
twtBoston <- searchTwitter('#boston', n = 200)
twtPhila <- searchTwitter('#philadelphia', n = 200)

# B. CREATE TWO DATA CORPUS FOR THE TWEETS
getCorpus <- function (tweets) {
  tweets.text <- lapply(tweets, function(t) {t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return (data.corpus)
}

data.t.boston = getCorpus(twtBoston)
data.t.phila = getCorpus(twtPhila)

# C. PRE-PROCESS DATA
getTransCorpus <- function (data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  data.corpus <- tm_map(data.corpus, removeNumbers)
  english.stopwords <- stopwords("en")
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus,content_transformer(removeWords),english.stopwords)
  data.corpus <- tm_map(data.corpus,content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus,content_transformer(stripWhitespace))
  
  return (data.corpus)
}

tdata.t.boston = getTransCorpus(data.t.boston)
tdata.t.phila = getTransCorpus(data.t.phila)

# D. CREATE TERM DOCUMENT MATRIX FOR EACH TWEET
tdmBost <- TermDocumentMatrix(tdata.t.boston)
tdmPhil <- TermDocumentMatrix(tdata.t.phila)

# E. COMPARE FREQ TERMS IN EACH STATE
mBost <- as.matrix(tdmBost)
mPhil <- as.matrix(tdmPhil)

# calculate the frequency of words 
WFBost <- rowSums(mBost)
WFBost = sort(WFBost,decreasing = T)

WFPhil <- rowSums(mPhil)
WFPhil = sort(WFPhil,decreasing = T)

# EXAMINE TOP TEN WORDS
cbind(WFBost[1:10])
cbind(WFPhil[1:10])

# FREQ TERMS WITH LOWER FREQ BOUND OF 10
findFreqTerms(tdmBost, lowfreq=10)
findFreqTerms(tdmPhil, lowfreq=10)

# F. GRAPHICAL WORDCLOUD OF THE FREQ WORDS
require(wordcloud)
palette = brewer.pal(8,"Dark2")
par(mfrow=c(1,2))
wordcloud(words = names(WFBost),
          freq=WFBost,
          min.freq=10,
          random.order=F,
          colors=palette)

wordcloud(words = names(WFPhil),
          freq=WFPhil,
          min.freq=10,
          random.order=F,
          colors=palette)
par(mfrow=c(1,1))

# G. SENTIMENT ANALYSIS SCORE
sentiment.na <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '', text)
  text <- gsub('[[:cntrl:]]', '', text)
  text <- gsub('\\d+', '', text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0)
    return (NA)
  else
    return (p - n)
}

# FETCH TEXT FROM TWEETS
bost.texts <- 
  lapply(twtBoston, 
         function(t) {
           iconv(t$getText(), 
                 "latin1", "ASCII", sub="")
         })

phil.texts <- 
  lapply(twtPhila, 
         function(t) {
           iconv(t$getText(), 
                 "latin1", "ASCII", sub="")
         })


# GET SCORES
bost.scores.na <- sapply(bost.texts, 
                    sentiment.na, 
                    pos.words, neg.words)

phil.scores.na <- sapply(phil.texts, 
                         sentiment.na, 
                         pos.words, neg.words)


table(bost.scores.na)

table(phil.scores.na)

par(mfrow=c(1,2))

barplot(table(bost.scores.na), main="Sentiment Analysis for Boston",
        xlab="Score", ylab="Count",
        ylim=c(0,40), col="cyan")
grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

barplot(table(phil.scores.na), main="Sentiment Analysis for Philadelphia",
        xlab="Score", ylab="Count",
        ylim=c(0,40), col="cyan")
grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

par(mfrow=c(1,1))

# Data frame of scores and tweets

bost.vector <- sapply(bost.texts,function (t) {(t)})
boston <- data.frame(Score=bost.scores.na, Text=bost.vector)
View(boston)

phil.vector <- sapply(phil.texts,function (t) {(t)})
philadelphia <- data.frame(Score=phil.scores.na, Text=phil.vector)
View(philadelphia)
