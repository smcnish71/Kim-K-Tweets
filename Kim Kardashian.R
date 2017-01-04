setwd("~/Vizs/Kim/Kim-K-Tweets")
data<- read.csv('KimKardashianTweets.csv', header=TRUE, stringsAsFactors = FALSE)
library(plyr)
library(reshape)
library(tm)
library(SnowballC)
library(stringr)

#Sentiment Anlaysis 

pos= scan('positive-words.txt', what='character',comment.char=';')
neg = scan('negative-words.txt', what='character',comment.char=';')


score.sentiment = function(tweets, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of tweets plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(tweets, function(text, pos.words, neg.words) {
    
    #clean up variables with regex
    
    text <- gsub("[0-9]", "", text) #all the numeric values
    text <- gsub("RT*", "", text) #all the RT, if there are.
    text <- gsub("[^\x20-\x7E]", "", text) #all the unrelated characters
    text <- gsub(" ?(www|ht)tp(s?)://(.*)[.][a-z]+", "", text) #all the hyperlinks
    text <- gsub("+(pic)?[.]twitter[.]+(.*)", "", text) #the pics
    text <- gsub("[@#+$//-]", "", text) #hash's
    text <- gsub("[[:punct:]]", "", text) #punctuation.
    text = gsub("[[:cntrl:]]", "", text)   # control characters
    
    # and convert to lower case:
    text = tolower(text)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(text, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    neg.matches = match(words, neg.words)
    pos.matches = match(words, pos.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=tweets)
  return(scores.df)
}
scores = score.sentiment(data$text, pos, neg)

data_scores = cbind(data,scores$score)
data_scores = subset(data_scores, select = -c(id, retweet, author))
data_scores = rename(data_scores, c("X"= "ID", "scores$score" = "score"))


#Pull out family names to see who Kim tweets about the most

#Kardashian nicknames into vectors
Kim.names = c("Kim","Kimmie","Keeks","Keke","kimkardashian")
Khloe.names = c("Khloe","KoKo","Khloé","khloekardashian")
Kourtney.names = c("Kourtney","Kourt","kourtneykardash")
Kendall.names = c("Kendall","Kenny","kendalljenner")
Kylie.names = c("Kylie","Kyle","kyliejenner")
Kris.names = c("Kris","krisjenner", "mom")
Caitlyn.names = c("Caitlyn", "Bruce","Caitlyn_Jenner")
Kanye.names = c("Kanye","Yeezus","Yeezy", "kanyewest")
North.names = c("North", "North West")
Saint.names = c("Saint", "Saint West")

#tag tweets with names
data_names=data_scores 

data_names$Kim = ifelse(grepl( paste(Kim.names,collapse="|"),data_scores$text, ignore.case= TRUE),"Kim","nope")
data_names$Khloe = ifelse(grepl( paste(Khloe.names,collapse="|"),data_scores$text, ignore.case= TRUE),"Khloe","nope")
data_names$Kourtney = ifelse(grepl( paste(Kourtney.names,collapse="|"),data_scores$text, ignore.case= TRUE),"Kourtney","nope")
data_names$Kendall = ifelse(grepl( paste(Kendall.names,collapse="|"),data_scores$text, ignore.case= TRUE),"Kendall","nope")
data_names$Kylie = ifelse(grepl( paste(Kylie.names,collapse="|"),data_scores$text, ignore.case= TRUE),"Kylie","nope")
data_names$Kris = ifelse(grepl( paste(Kris.names,collapse="|"),data_scores$text, ignore.case= TRUE),"Kris","nope")
data_names$Caitlyn = ifelse(grepl( paste(Caitlyn.names,collapse="|"),data_scores$text, ignore.case= TRUE),"Caitlyn","nope")
data_names$Kanye = ifelse(grepl( paste(Kanye.names,collapse="|"),data_scores$text, ignore.case= TRUE),"Kanye","nope")
data_names$North = ifelse(grepl( paste(North.names,collapse="|"),data_scores$text, ignore.case= TRUE),"North","nope")
#removed 'Saints' from tagging with Saint name because Reggie Bush played for them
data_names$Saint = ifelse((grepl( paste(Saint.names,collapse="|"),data_scores$text, ignore.case= TRUE) & !grepl("Saints",data_scores$text, ignore.case= TRUE)),"Saint","nope")


#transform data for Tableau
data_names = melt(data_names, id=c("ID","date","link","text","score"))

data_names = data_names[!(data_names$value=="nope"),]
data_names = data_names[,-6]
data_names = merge( x= data_names , y= data_scores , by  = "ID" , all.y = TRUE)
data_names = rename(data_names, c("date.y"="date", "link.y" = "link", "text.y" = "text", "score.y"="score","value"="name"))
data_names = data_names[,-c(2:5)]


#how ! is Kim?
data_exclam = data_names
data_exclam$exclam = str_count(data_names$text, "!")

write.csv(data_exclam,file="Twitter Data with analysis.csv",row.names=F)


#What's kim mad about?
neg_tweets = data_scores[which(data_scores$score < -1),"text"]

#create a corpus
docs <- Corpus(VectorSource(neg_tweets))
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("kimkardashian", "khloekardashian")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
neg_word_freqs <- data.frame(word = names(v),freq=v)

write.csv(neg_word_freqs,file="Word Frequencies of Negative Tweets.csv",row.names=F)

