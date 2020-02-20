# Note: Downloads from Twitter and other sources were saved locally.
# The file path needs to be modified based on the computer.
# Twitter API credentials also need to be entered for 
# Citibank vs competitors in Twitter mentions to be updated.

# Install packages
#####
for (i in c('jsonlite', 'utils', 'DBI', 'readbitmap','rtweet','rjson','dplyr','httr','reshape2','ggplot2','maps','tidyverse','stringr','mapproj','maps','viridis','rworldmap','igraph','networkD3', 'base64enc', 'dbplyr', 'lubridate', 'knitr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

for (i in c('hms', 'scales', 'topicmodels',"syuzhet",'ggplot2','tidyr','wordcloud','lubridate','SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}
#####

# Downloading Citibank tweets
#####
citibank_search_1 = search_30day(q = "citibank", n = 25000, fromDate="202001010000",toDate="202001310000", env_name = "R",token = twitter_token)
citibank_search_2 = search_30day(q = "citibank", n = 25000, fromDate="202001010000",toDate="202001250000", env_name = "R",token = twitter_token)
citibank_search_3 = search_30day(q = "citibank", n = 25000, fromDate="202001010000",toDate="202001220000", env_name = "R",token = twitter_token)
citibank_search_4 = search_30day(q = "citibank", n = 25000, fromDate="202001010000",toDate="202001210000", env_name = "R",token = twitter_token)
citibank_search_5 = search_30day(q = "citibank", n = 25000, fromDate="202001010000",toDate="202001140000", env_name = "R",token = twitter_token)
citibank_search_6 = search_30day(q = "citibank", n = 25000, fromDate="202001010000",toDate="202001130000", env_name = "R",token = twitter_token)
citibank_search_7 = search_30day(q = "citibank", n = 25000, fromDate="201912310000",toDate="202001040000", env_name = "R",token = twitter_token)
citibank_tweets = rbind(citibank_search_1,citibank_search_2,citibank_search_3,citibank_search_4,citibank_search_5,citibank_search_6,citibank_search_7)

# Updating new the database with new tweet from 31st Jan 2020 to 5th Feb 2020
newone = search_30day(q = "citibank", n = 25000, fromDate="201901310000",toDate="202002040000", env_name = "R",token = twitter_token)
citibank_tweets = rbind(citibank_tweets, newone)

# Saving downloads locally
# citibank_tweets = saveRDS(citibank_search, file = 'filepath')

#Uploading saved data using local filepath
citibank_tweets <- readRDS("C:/Users/kfernandes/Desktop/Kripa Fernandes/Social Media Analytics/Group assignment/citibanksearch.rds")
#####

# Most popular tweets
#####
# Retweeted tweets: sorted by most to least retweeted
most_retweeted = citibank_tweets[order(-citibank_tweets$retweet_count),]

# Liked tweets: sorted by most to least liked
most_liked = citibank_tweets[order(-citibank_tweets$favorite_count),]
#####

# Tweet sources with graph
#####
# Graph frequency of tweet sources
source_frequency = as.data.frame(table(citibank_tweets$source))
source_frequency <- source_frequency[order(-source_frequency$Freq),]
source_frequency <- source_frequency[1:5,]

barplot(source_frequency$Freq,
        main = "TOP 5 source of tweets",
        ylab = "Volume of tweets",
        xlab = "Source",
        names.arg = source_frequency$Var1 ,
        col = "darkred",
        horiz = FALSE,
        cex.names=0.5)
#####

# Citibank stock prices and volume
#####
#Stock Price of citigroup -> https://fr.finance.yahoo.com/quote/C?ltr=1
plot(y = citibank_stock_price$Price,x = citibank_stock_price$Date, type = 'l', main = "Citigroup stock price over the month of Jan 2020", ylab = "Price in $", xlab = 'from 3rd Jan to 31 Jan')

# OR

barplot(citibank_stock_price$Price,
        main = "Stock price of citigroup",
        ylab = "Price in $",
        xlab = "Date",
        names.arg = citibank_stock_price$Date ,
        col = "darkgreen",
        horiz = FALSE,
        cex.names=0.5)

#Stock Volume of citibank -> https://fr.finance.yahoo.com/quote/C?ltr=1
plot(y = citibank_stock_price$Volume,x = citibank_stock_price$Date, type = 'l', main = "Citigroup stock volume over the month of Jan 2020", ylab = "Volume in $", xlab = 'from 3rd Jan to 31 Jan')

# OR 

barplot(citibank_stock_price$Volume,
        main = "Stock volume of citigroup",
        ylab = "Volume in $",
        xlab = "Date",
        names.arg = citibank_stock_price$Date ,
        col = "darkgreen",
        horiz = FALSE,
        cex.names=0.5)
#####

# Citigroup revenue per geographic area 
# and Citibank share in total revenue
#####
# Revenue repartition of citigroup around the globe -> https://www.citigroup.com/citi/news/2019/third-quarter-2019-earnings.htm
continent = c("North America", "EMEA", "South America", "Asia", "Corporate / Other" )
revenue = c(8458,2960,2725,3786,493)

pie(revenue,labels = revenue, col=rainbow(length(continent)),
    main="Q3 2019 Citigroup revenue in million $")
legend('topright',continent, cex = 0.6,
       fill = rainbow(length(continent)))


# Weight of citibank in citigroup results -> https://www.citigroup.com/citi/news/2019/third-quarter-2019-earnings.htm
department = c("Citibank / Global Consumer Banking", "Private / Institutional Clients Group", "Corporation & others" )
global_revenue = c(8648 , 9248 , 493)

pie(global_revenue,labels = global_revenue, col=rainbow(length(department)),
    main="Citigroup revenue per department")
legend('top',department, cex = 0.6,
       fill = rainbow(length(department)))
#####

# Citibank in Google Trend
#####
# Citibank trend on internet using "Google Trend"
citibank_google_trend = read.csv("C:/Users/kfernandes/Desktop/Kripa Fernandes/Social Media Analytics/Group assignment/citibank_group_trend.csv")

plot(y = citibank_google_trend$score, x = citibank_google_trend$date, type = "l" ,main = "Google Trend about 'citibank'", ylab = "Score", xlab = 'from Dec to Jan 2020')

barplot(citibank_google_trend$score, names.arg = citibank_google_trend$date,  main = "Google Trend about 'citibank'", ylab = "Score", xlab = 'from Dec to Jan 2020')
#####

# Golden Ratio calculation
#####
#Find Citibank influencers

#Importing user data
user_citi <- users_data(citibank_tweets)

#Finding mean followers and friends for users
counts_citi <- user_citi %>%
  group_by(screen_name) %>%
  summarize(follower = mean(followers_count),
            friend = mean(friends_count))

# Calculate and store the golden ratio
counts_citi$ratio <- counts_citi$follower/counts_citi$friend
head(counts_citi$ratio)

# Sort the data frame in decreasing order of follower count
counts_citi_sort <- arrange(counts_citi, desc(follower))

#List of users who can be used as Citibank influencers
citi_influencers <- counts_citi_sort[counts_citi_sort$follower>30000,]

#List of users to position advertisments for targetted promotion
citi_adverts <- counts_citi_sort[counts_citi_sort$follower<=2000,]
#####

# Citibank vs competitors in Twitter mentions
#####
# Extract tweets on #citibank and exclude retweets
citi_twts <- search_tweets("#citibank", n = 18000, include_rts = FALSE)
citi_ts <- ts_data(citi_twts, by = 'hours')
names(citi_ts) <- c("time", "citi_n")

#Extracting tweets on competitor banks
# Extract tweets on #hsbc and exclude retweets
hsbc_twts <- search_tweets("#hsbc", n = 18000, include_rts = FALSE)
hsbc_ts <- ts_data(hsbc_twts, by = 'hours')
names(hsbc_ts) <- c("time", "hsbc_n")

# Extract tweets on #bankofamerica and exclude retweets
boa_twts <- search_tweets("#bankofamerica", n = 18000, include_rts = FALSE)
boa_ts <- ts_data(boa_twts, by = 'hours')
names(boa_ts) <- c("time", "boa_n")

# Extract tweets on #BNPParibas and exclude retweets
bnp_twts <- search_tweets("#BNPParibas", n = 18000, include_rts = FALSE)
bnp_ts <- ts_data(bnp_twts, by = 'hours')
names(bnp_ts) <- c("time", "bnp_n")


# Merge the two time series objects and retain "time" column
merged_df1 <- merge(citi_ts, hsbc_ts, by = "time", all = TRUE)
merged_df2 <- merge(merged_df1, boa_ts, by = "time", all = TRUE)
merged_df <- merge(merged_df2, bnp_ts, by = "time", all = TRUE)

# Stack the tweet frequency columns
melt_df <- melt(merged_df, na.rm = TRUE, id.vars = "time")

# Plot frequency of tweets on the banks
ggplot(data = melt_df, aes(x = time, y = value, col = variable))+
  geom_line(lwd = 0.8)
#####


#Location plot
#####
citi_coord <- lat_lng(citibank_tweets)
citi_geo <- na.omit(citi_coord[, c("lat", "lng")])
cg <- as.data.frame(citi_geo)
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
mp <- mp + geom_point(aes(x=cg$lng, y=cg$lat) ,color="pink", size=3) 
mp
with(citi_geo, points(lng, lat, pch = 20, cex = 1, col = 'red'))
#####

# Citibank reponses to tweets
#####
replies <- citibank_tweets[citibank_tweets$reply_count > 0,]
replies <- as.data.frame(replies)
names(replies)


citi_og_tweets <- citibank_tweets[citibank_tweets$reply_count == 0,]
tweets_og <- unique(citi_og_tweets$screen_name)
tweets_og <- as.data.frame(tweets_og)
ct <- count(tweets_og)

reply <- replies %>%
  filter(str_detect(str_to_lower(screen_name), "citi"))
reply <- as.data.frame(reply)
reply_twt <- unique(reply$screen_name)
reply_twt <- as.data.frame(reply_twt)
rt <- count(reply_twt)

response_rate <- (rt/ct)
response_rate
#####

# Common interests of Citibank followers (Prototype)
#####
# friends <- get_friends(citibank_tweets$user_id, n = 30, retryonratelimit = FALSE, page = "-1", parse = TRUE, verbose = TRUE, token = NULL)
# write.csv(friends, "C:\\Users\\kfernandes\\Desktop\\Kripa Fernandes\\Social Media Analytics\\Group assignment\\friends.csv")

# friends_id <- read.csv("C:\\Users\\kfernandes\\Desktop\\Kripa Fernandes\\Social Media Analytics\\Group assignment\\friends.csv")
friends_id1 <- transform(friends_id, freq.loc = ave(seq(nrow(friends_id)), user_id, FUN=length))
friends_id2 <- friends_id1[friends_id1$freq.loc > 1,]
friends_id2 %>% arrange(desc(freq.loc))

#Dataframe of common users followed by sample of Citibank follower's
user_id_friends <- friends_id2[friends_id2$freq.loc > 2,]
#####

# Network analysis
#####
network <- citibank_tweets[, c("screen_name", "retweet_screen_name")]

network1 <- network[complete.cases(network), ]

#Convert network to matrix
network_m <- as.matrix(network1)

#Plot network in interactive graph
p <- simpleNetwork(network1, height="300px", width="300px")
p

rtwt_network <- graph_from_edgelist(el = network_m, directed = TRUE)

#Out degree scores for the network
outd <- degree(rtwt_network, mode = c("out"))
outds <- sort(outd, decreasing = TRUE)

#Top 10 users with highest out degree scores
outds[1:10]

#In degree scores for the network
ind <- degree(rtwt_network, mode = c("in"))
inds <- sort(ind, decreasing = TRUE)

#Top 10 users with highest in degree scores
inds[1:10]

#Betweenness scores
btwn <- betweenness(rtwt_network, directed = TRUE)
btwns <- btwn %>% 
  sort(decreasing = TRUE) %>%
  round()

#Top 10 users with highest in betweenness scores
btwns[1:10]
#####

# Text pre-processing
#####
#We create and preprocess the subset of tweets that are in english
Comments<-citibank_tweets[citibank_tweets$lang=='en',c('screen_name','status_id','text','created_at','status_url')]
names(Comments)[names(Comments)=='text']<-'message'
names(Comments)[names(Comments)=='status_id']<-'id'
names(Comments)[names(Comments)=='created_at']<-'created_time'
Comments<-Comments[!duplicated(Comments$message), ] #we delete duplicates as 
CommentsToPrint<-Comments
Comments<-select (Comments,-c('screen_name','status_url'))

Comments1 <- mutate(Comments, message = gsub(x = message, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

#We tokenize the Comments that we have
Tokenized <- Comments1 %>% unnest_tokens(output = "word",
                                         input = message,
                                         token = "words",
                                         drop=FALSE,to_lower=TRUE)
Tokenized1 <- filter(Tokenized, substr(word, 1, 1) != '#', 
                     substr(word, 1, 1) != '@') 
remove<-c('citibank','1','0','2','3','4','5','6','7','8','9','citi','https','t.co')

Tokenized1 <- Tokenized1[ !Tokenized1$word %in% remove,] #we remove the previously stated words
Tokenized2 <- Tokenized1 %>% anti_join(get_stopwords()) 
Tokenized3 <- Tokenized2 %>% count(id,word)

#We create the Document Term Matrix
DTM <- Tokenized3 %>% cast_dtm(id,word,n,weighting = tm::weightTfIdf)
DTMDense <- removeSparseTerms(DTM,0.7)
#####

# Wordcloud and bigram
#####
#We create a frequency table of the words mentioned in the tweets
Freq <- Tokenized3 %>% group_by(word) %>% 
  summarize(freq = n()) %>%
  arrange(-freq)                  

#We create a word cloud based on the information from the just created frequency table.
wordcloud(Freq$word, Freq$freq,
          max.words=40,
          scale=c(3,1))

# Implementing bigrams
remove<-c('https','citibank','citi','t.co','1','0','2','3','4','5','6','7','8','9')
BigramCount <- Comments %>% unnest_tokens(output = "bigram",
                                          input = message,
                                          token = "ngrams",n=2, drop=FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% get_stopwords()$word) %>%
  filter(!word2 %in% get_stopwords()$word) %>%
  filter(!word1 %in% remove) %>%
  filter(!word2 %in% remove) %>% 
  unite(bigram, word1, word2, sep = " ") %>%
  count(id,bigram)

# BigramCount <- BigramCount[ !BigramCount$bigram %in% remove,] #we remove the previously stated words
BigramDTM <- BigramCount  %>% cast_dtm(id,bigram,n)

# make a wordcloud
BiCount <- BigramCount %>% group_by(bigram) %>% summarize(freq = n())
wordcloud(BiCount$bigram,BiCount$freq,max.words = 40)

remove<-c('citibank','1','0','2','3','4','5','6','7','8','9','citi','https','t.co')
BigramCount1 <- BigramCount[ !BigramCount$bigram %in% remove,]
#####

# Sentiment analysis: Count of negative/positive tweets, by post and over time

# Sentiment analysis: Negative/Positive tweet count, per post, timeline of sentiments
#####
Comments<-citibank_tweets[citibank_tweets$lang=='en',c('created_at','text','status_id')]
names(Comments)[names(Comments)=='text']<-'message'
names(Comments)[names(Comments)=='status_id']<-'id'
names(Comments)[names(Comments)=='created_at']<-'created_time'
Comments<-Comments[!duplicated(Comments$message), ]
Comments1 <- mutate(Comments, message = gsub(x = message, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

Tokenized <- Comments1 %>% unnest_tokens(output = "word",
                                         input = message,
                                         token = "words",
                                         drop=FALSE,to_lower=TRUE)
get_sentiments("bing") %>% 
  count(sentiment)

Sentiment <- inner_join(Tokenized,get_sentiments("bing"))

# Get the most positive/negative words
summarySentiment <- Sentiment %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)
par(oma=c(0,4,0,0),mfrow=c(1,2))

summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Get get a summary of the sentiment per post
statusSentiment <- Sentiment %>%
  count(id, sentiment) %>%               
  spread(sentiment, n, fill = 0) %>%      
  mutate(sentiment = positive - negative)

#Using the afinn dictionary:
statusSentiment <- inner_join(Tokenized3,get_sentiments("afinn")) %>%
  group_by(id) %>%                      
  summarize(Sentiment = sum(value)) 

mean(statusSentiment$Sentiment)

statusSentiment <- Comments %>% left_join(statusSentiment,by="id") %>% 
  mutate(Sentiment = ifelse(is.na(Sentiment),0,Sentiment))

CommentstoPrint2<-CommentsToPrint %>% left_join(statusSentiment,by="id") %>% 
  mutate(Sentiment = ifelse(is.na(Sentiment),0,Sentiment))
CommentstoPrint2<-select(CommentstoPrint2,-c('id','message.y','created_time.y'))
names(CommentstoPrint2)[names(CommentstoPrint2)=='message.x']<-'Tweet message'
names(CommentstoPrint2)[names(CommentstoPrint2)=='created_time.x']<-'Time of Creation'
names(CommentstoPrint2)[names(CommentstoPrint2)=='status_url']<-'URL of tweet'
names(CommentstoPrint2)[names(CommentstoPrint2)=='screen_name']<-'Tweeter name'

head(CommentstoPrint2)


# Now, we can plot the tweets over time
statusSentiment<-as.data.frame(statusSentiment)
time<-statusSentiment[,1]
sentiment <- statusSentiment[order(statusSentiment$created_time),"Sentiment"]
lim <- max(abs(sentiment))

#Plot sentiment by time
plot(1:length(sentiment), 
     sentiment, 
     xaxt="n",
     type="l",
     ylab="Valence",
     xlab="Time (days:hour:minute)",
     main="Sentiment", 
     ylim=c(-lim,lim))
abline(h = 0, col = "red", lty = 3)

axis(1,at=1:length(sentiment), 
     labels=paste0(substr(time[order(time)],1,10),"\n",substr(time[order(time)],12,16)))
#####

# Classification of emotions expressed
#####
#getting emotions using in-built function
mysentiment<-get_nrc_sentiment(citibank_tweets$text)

#calculationg total score for each sentiment
Sentimentscores<-data.frame(colSums(mysentiment[,]))

names(Sentimentscores)<-"Score"
Sentimentscores<-cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(Sentimentscores)<-NULL

#plotting the sentiments with scores
ggplot(data=Sentimentscores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Citibank")
#####

# Topic analysis: Classifcation of topics, topics per post
#####
#We recreate the comments dataset
Comments<-citibank_tweets[citibank_tweets$lang=='en',c('status_id','text','created_at')]
Comments<-Comments[!duplicated(Comments$text), ]

allTweets<-Comments[,c('status_id','text')]

# Create the dtm 
tweetsTokenized <- allTweets %>% unnest_tokens(output = "word",
                                               input = text,
                                               token = "words",
                                               drop=FALSE,to_lower=TRUE) %>%
  filter(!word %in% get_stopwords()$word) %>%
  filter(!word %in% remove)

# do some basic preprocessing steps:
tweetsTokenized <- tweetsTokenized %>%
  anti_join(stop_words) %>% 
  count(status_id,word , sort=TRUE) %>%
  cast_dtm(document = status_id, term = word,
           value = n, weighting = tm::weightTf)

## Estimate the model for the topics
tweets_lda <- LDA(tweetsTokenized, k = 3,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) ) #most of the time we ll use gibbs. the list makes sure work in the correct way.

#Get the terms per topic
tweet_topics <- tidy(tweets_lda, matrix = "beta")

#Getting the top terms of each topic
top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")

top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#get the topics per document: which topics determine the documents?
tweet_documents <- tidy(tweets_lda, matrix = "gamma")

# CHoose, per tweet, the most important topic (the one with the highest weight)
tweet_doc_topic <- tweet_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 
print(head(tweet_doc_topic))
tweet_doc_topic %>%
  group_by(topic) %>% 
  summarise(nbr_documents = n())
print(head(tweet_doc_topic))
#####

# Timeline of tweets: Day, hour (source: https://www.promptcloud.com/blog/data-mining-analytics-emma-watson-tweets-with-r/)
#####

# Plotting tweet count per day
ggplot(data = citibank_tweets, aes(x = wday(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Days of the week") + ylab("Tweet frequency") + 
  theme_minimal() +
  scale_fill_gradient(low = "turquoise3", high = "darkgreen")

# Extract only time from the timestamp 
citibank_tweets$time <- hms::hms(second(citibank_tweets$created_at), 
                           minute(citibank_tweets$created_at), 
                           hour(citibank_tweets$created_at))

# Converting to `POSIXct` `
citibank_tweets$time <- as.POSIXct(citibank_tweets$time)

ggplot(data = citibank_tweets)+
  geom_density(aes(x = time, y = ..scaled..),
               fill="turquoise3", alpha=0.3) + 
  xlab("Hours") + ylab("Tweet frequency") +
  scale_x_datetime(breaks = date_breaks("2 hours"), 
                   labels = date_format("%H:%M")) +
  theme_minimal()
#####