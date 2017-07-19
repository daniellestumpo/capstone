library(twitteR)
library(dismo)
library(ggmap)
library(rworldmap)
library(ggplot2)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(RCurl)
library(RJSONIO)
library(stringr)
library(maps)

#To extract the tweets
api_key = 'Z7rFL4zwzCis74puCkrEamprY'
api_secret = 'FIIuLCqmIUrtKEgCULMQmBvSSvdMJBi6wLPlGLBbBn8OhLBpAN'
access_token = '17665722-MCrCDkEBV6iEmQiU2b2PpX5lkoniBOPDcT6iKDFrH'
access_token_secret = ' sGCDPHzOKuLXJmx6bXbXynVsotLqRHslUt9BQYzjZyfTS'
cred <- OAuthFactory$new(consumerKey='Z7rFL4zwzCis74puCkrEamprY', consumerSecret='FIIuLCqmIUrtKEgCULMQmBvSSvdMJBi6wLPlGLBbBn8OhLBpAN',requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')
cred$handshake(cainfo="cacert.pem")

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


tweet=searchTwitter('All About Vegan Foods',n=1000)
tweet1=searchTwitter('Vegan Foods',n=2000)

tweet=twListToDF(tweet)
tweets1=twListToDF(tweet)
tweets=rbind(tweets,tweets1)

tweets=rbind(tweets,tweets1)


tweets=unique(tweets)
tweets$date <- format(tweets$created, format="%Y-%m-%d")
table(tweets$date)
d <- as.data.frame(table(tweets$screenName))
d <- d[order(d$Freq, decreasing=T), ] #descending order of tweeters according to frequency of tweets
names(d) <- c("User","Tweets")
head(d)
#plotting tweets by user
barplot(head(d$Tweets, 20), names=head(d$User, 20), horiz=T, las=1, main="Top 20: Tweets per User", col=1)
#Plotting the tweets by time of tweet
minutes <- 60
ggplot(data=tweets, aes(x=created)) +
  geom_histogram(aes(fill=..count..), binwidth=60*minutes) +
  scale_x_datetime("Date") +
  scale_y_continuous("Frequency")

#split the data frame into 3 parts for the algorithm
load("VeganTweets.rda")
chunk <- 700 
n <- nrow(tweets)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
vegantweets <- split(tweets,r)


#function to clean the tweets to remove characters and other non-essential stuff
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  some_txt = gsub(" $", "", some_txt) #added, works?
  some_txt = str_replace_all(some_txt, "[^[:alnum:]]", " ")
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}



###########################################################




#------------- Analyze the first set of tweets --------------------------------

db_key <- "5d911bcbaddf3bffccd77a89bd9a6e91" #1

getSentiment <- function (text, key){
  
  text <- URLencode(text);
  
  #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  ##########################################
  
  data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", db_key, "&text=",text, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  
  # get mood probability
  sentiment = js$output$result
  
  ###################################
  
  
  return(list(sentiment=sentiment))
}

tweets = vegantweets[[1]]

# get text 
tweet_txt =tweets$text

# clean text
tweet_clean = clean.text(tweet_txt)
tweet_num = length(tweet_clean)
# data frame (text, sentiment)
tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)

print("Getting sentiments...")
# apply function getSentiment
sentiment = rep(0, tweet_num)
for (i in 1:tweet_num)
{
  tmp = getSentiment(tweet_clean[i], db_key)
  
  tweet_df$sentiment[i] = tmp$sentiment
  
  print(paste(i," of ", tweet_num))
  
  
}

# delete rows with no sentiment
tweet_df1 <- tweet_df[tweet_df$sentiment!="",]

#----------------------------------------------------

# ----------------- Analyze the 2nd Batch of Tweets -------------------------
db_key <- "5d911bcbaddf3bffccd77a89bd9a6e91" #

getSentiment <- function (text, key){
  
  text <- URLencode(text);
  
  #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  ##########################################
  
  data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", db_key, "&text=",text, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  
  # get mood probability
  sentiment = js$output$result
  
  ###################################
  
  
  return(list(sentiment=sentiment))
}

tweets = vegantweets[[2]]

# get text 
tweet_txt =tweets$text

# clean text
tweet_clean = clean.text(tweet_txt)
tweet_num = length(tweet_clean)
# data frame (text, sentiment)
tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)

print("Getting sentiments...")
# apply function getSentiment
sentiment = rep(0, tweet_num)
for (i in 1:tweet_num)
{
  tmp = getSentiment(tweet_clean[i], db_key)
  
  tweet_df$sentiment[i] = tmp$sentiment
  
  print(paste(i," of ", tweet_num))
  
  
}

# delete rows with no sentiment
tweet_df2 <- tweet_df[tweet_df$sentiment!="",]

#--------------------------------------------------

# ----------------- Analyze the 3rd Batch of Tweets -------------------------
db_key <- "5d911bcbaddf3bffccd77a89bd9a6e91" #3

getSentiment <- function (text, key){
  
  text <- URLencode(text);
  
  #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  ##########################################
  
  data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", db_key, "&text=",text, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  
  # get mood probability
  sentiment = js$output$result
  
  ###################################
  
  
  return(list(sentiment=sentiment))
}


tweets = vegantweets[[3]]

# get text 
tweet_txt =tweets$text

# clean text
tweet_clean = clean.text(tweet_txt)
tweet_num = length(tweet_clean)
# data frame (text, sentiment)
tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)

print("Getting sentiments...")
# apply function getSentiment
sentiment = rep(0, tweet_num)
for (i in 1:tweet_num)
{
  tmp = getSentiment(tweet_clean[i], db_key)
  
  tweet_df$sentiment[i] = tmp$sentiment
  
  print(paste(i," of ", tweet_num))
  
  
}

# delete rows with no sentiment
tweet_df3 <- tweet_df[tweet_df$sentiment!="",]

#--------------------------------------------------


#Recombine the tweets
tweet_df <- rbind(tweet_df1, tweet_df2, tweet_df3)

#separate text by sentiment
sents = levels(factor(tweet_df$sentiment))
#emos_label <- emos


# get the labels and percents

labels <-  lapply(sents, function(x) paste(x,format(round((length((tweet_df[tweet_df$sentiment ==x,])$text)/length(tweet_df$sentiment)*100),2),nsmall=2),"%"))



nemo = length(sents)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = tweet_df[tweet_df$sentiment == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}
#emo.docs = iconv(emo.docs, to = "UTF-16")


# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("german"))
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels


# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"), scale = c(3,1), random.order = FALSE, title.size = 1.5)

#bar graph
plot <- data.frame(Sentiment = c("Negative", "Positive", "Neutral"), Percentage = c(21.19, 43.89, 34.92))

veganplot <- ggplot(plot, aes(Sentiment, Percentage, color = Sentiment, fill = Sentiment)) + geom_bar(stat = "identity", color = brewer.pal(nemo, "Dark2") ,fill = brewer.pal(nemo, "Dark2")) + ylab("Percentage of Tweets")

#Word Analysis
word <- rownames(Words)
Words <- as.data.frame(tdm, row.names = FALSE)
Words$word <- word

Positive_Words <- Words[order(-Words$`positive 43.89 %`),]
Positive_Words <- Positive_Words[1:5,]

ggplot(Positive_Words, aes(word, (Positive_Words$`positive 43.89 %`)/sum(tdm[1:nrow(tdm),1:3])*100)) + geom_bar(stat = "identity") + ylab("Percentage that word was used") + ggtitle("Positive Words") + theme(plot.title = element_text(hjust = 0.5))


Negative_Words <- Words[order(-Words$`negative 21.19 %`),]
Negative_Words <- Negative_Words[1:5,]

ggplot(Negative_Words, aes(word, (Negative_Words$`negative 21.19 %`)/sum(tdm[1:nrow(tdm),1:3])*100)) + geom_bar(stat = "identity") + ylab("Percentage that word was used") + ggtitle("Negative Words") + theme(plot.title = element_text(hjust = 0.5))


#Geo Plot
dfworld <- load("VeganTweets.rda")
locations <- data.frame(lon = as.numeric(tweets$longitude), lat = as.numeric(tweets$latitude))
locations <- subset(locations, !is.na(locations$lon))
# approximate lat/lon from textual location data.
with(locations, plot(lon, lat))

worldMap <- map_data("world")  # Easiest way to grab a world map shapefile

zp1 <- ggplot(worldMap)
zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group),  # Draw map
                       colour = gray(2/3), lwd = 1/3)
zp1 <- zp1 + geom_point(data = locations,  # Add points indicating users
                        aes(x = lon, y = lat),
                        colour = "SKY BLUE", alpha = 1/2, size = 5)
zp1 <- zp1 + coord_equal()  # Better projections are left for a future post
zp1 <- zp1 + theme_minimal()  # Drop background annotations
print(zp1)

