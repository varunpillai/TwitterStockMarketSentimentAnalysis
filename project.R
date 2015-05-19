# LOAD TWITTER PACKAGE
require(tm)
require(twitteR)
require(ROAuth)

# LOAD TWITTER CREDENTIALS
t.api.key = ""
t.api.secret = ""
t.api.accesstoken = "-"
t.api.accesssecret = ""

t.cred <- load("twitcred")
setup_twitter_oauth(t.api.key, t.api.secret, t.api.accesstoken, t.api.accesssecret)

# A. GET TWEETS OF GAINERS
twtCNYD <- searchTwitter('$CNYD', n = 100)
twtBPTH <- searchTwitter('$BPTH', n = 100)
twtXPO <- searchTwitter('$XPO', n = 100)
save(twtCNYD, file="twtCNYD")
save(twtBPTH, file="twtBPTH")
save(twtXPO, file="twtXPO")
twtCNYD = load("twtCNYD")
twtBPTH = load("twtBPTH")
twtXPO = load("twtXPO")

gainers = c(twtCNYD,twtBPTH,twtXPO)

# A. GET TWEETS OF LOSERS
twtIG <- searchTwitter('$IG', n = 100)
twtSSYS <- searchTwitter('$SSYS', n = 100)
twtNVDQ <- searchTwitter('$NVDQ', n = 100)
save(twtIG, file="twtIG")
save(twtSSYS, file="twtSSYS")
save(twtNVDQ, file="twtNVDQ")
twtIG = load("twtIG")
twtSSYS = load("twtSSYS")
twtNVDQ = load("twtNVDQ")

losers = c(twtIG,twtSSYS,twtNVDQ)

# B. CREATE TWO DATA CORPUS FOR THE TWEETS
getCorpus <- function (tweets) {
  tweets.text <- lapply(tweets, function(t) {t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
  return (data.corpus)
}

gainers.corp = getCorpus(gainers)
gainers.df<-data.frame(text=unlist(sapply(gainers.corp, `[`, "content")),stringsAsFactors=F)
writeCorpus(gainers.corp,path=paste0(getwd(),"/data.corpus1",sep=""), filenames=NULL)

losers.corp = getCorpus(losers)
losers.df<-data.frame(text=unlist(sapply(losers.corp, `[`, "content")),stringsAsFactors=F)
writeCorpus(losers.corp,path=paste0(getwd(),"/data.corpus2",sep=""), filenames=NULL)

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

tgainers.corp = getTransCorpus(gainers.corp)
tlosers.corp = getTransCorpus(losers.corp)

# D. CREATE TERM DOCUMENT MATRIX FOR EACH TWEET
tdmGainers <- TermDocumentMatrix(tgainers.corp)
tdmLosers <- TermDocumentMatrix(tlosers.corp)

save(tdmGainers, file="tdmGainers")
save(tdmLosers, file="tdmLosers")

# E. COMPARE FREQ TERMS IN EACH STATE
mGain <- as.matrix(tdmGainers)
mLose <- as.matrix(tdmLosers)

# CALCULATE FREQUENCY OF WORDS
WFGain <- rowSums(mGain)
WFGain = sort(WFGain,decreasing = T)

WFLose <- rowSums(mLose)
WFLose = sort(WFLose,decreasing = T)

# FREQUENCIES OF ALL WORDS
cbind(WFGain[1:750])
cbind(WFLose[1:951])

# FREQ TERMS WITH LOWER FREQ BOUND OF 10
findFreqTerms(tdmGainers, lowfreq=10)
findFreqTerms(tdmLosers, lowfreq=10)

# GRAPHICAL WORDCLOUD OF THE FREQ WORDS
require(wordcloud)
palette = brewer.pal(8,"Dark2")
par(mfrow=c(1,2))
wordcloud(words = names(WFGain),
          freq=WFGain,
          min.freq=5,
          random.order=F,
          colors=palette)

wordcloud(words = names(WFLose),
          freq=WFLose,
          min.freq=5,
          random.order=F,
          colors=palette)
par(mfrow=c(1,1))

# F. SENTIMENT ANALYSIS SCORE
pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

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
gain.texts <- 
  lapply(gainers, 
         function(t) {
           iconv(t$getText(), 
                 "latin1", "ASCII", sub="")
         })

lose.texts <- 
  lapply(losers, 
         function(t) {
           iconv(t$getText(), 
                 "latin1", "ASCII", sub="")
         })


# GET SCORES
gain.scores.na <- sapply(gain.texts, 
                         sentiment.na, 
                         pos.words, neg.words)

lose.scores.na <- sapply(lose.texts, 
                         sentiment.na, 
                         pos.words, neg.words)

table(gain.scores.na)

table(lose.scores.na)

par(mfrow=c(1,2))

barplot(table(gain.scores.na), main="Sentiment Analysis for Gainers",
        xlab="Score", ylab="Count",
        ylim=c(0,50), col="cyan")
grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

barplot(table(lose.scores.na), main="Sentiment Analysis for Losers",
        xlab="Score", ylab="Count",
        ylim=c(0,50), col="cyan")
grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

par(mfrow=c(1,1))

# Data frame of scores and tweets

gain.vector <- sapply(gain.texts,function (t) {(t)})
gains <- data.frame(Score=gain.scores.na, Text=gain.vector)
View(gains)

lose.vector <- sapply(lose.texts,function (t) {(t)})
loses <- data.frame(Score=lose.scores.na, Text=lose.vector)
View(loses)

# EXTRA CREDITS
require(quantmod)
library("googleVis", lib.loc="~/R/win-library/3.1")

getSymbols("CNYD")  #highest gainer
getSymbols("RCKY")  #lowest gainer

# FETCH THE DATA FOR THE DAY 
cnyd.df = data.frame(CNYD['2015-04-29'])
rcky.df = data.frame(RCKY['2015-04-29'])

open = c(as.numeric(cnyd.df$CNYD.Open),as.numeric(rcky.df$RCKY.Open))
high = c(as.numeric(cnyd.df$CNYD.High),as.numeric(rcky.df$RCKY.High))
low = c(as.numeric(cnyd.df$CNYD.Low),as.numeric(rcky.df$RCKY.Low))
close = c(as.numeric(cnyd.df$CNYD.Close),as.numeric(rcky.df$RCKY.Close))
stocks = c("CNYD","RCKY")

mergedrows = data.frame(stocks,open,high,low,close)
str(mergedrows)

chartA = gvisBarChart(mergedrows,
                        xvar="stocks",
                        yvar=c("open","high","low","close"))
plot(chartA)
