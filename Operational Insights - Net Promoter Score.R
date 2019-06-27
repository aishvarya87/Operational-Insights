################# Samsung Sentiment Analysis - Part I - Postive and Negative Sentiment ######################

samsung_tweets <- userTimeline("SamsungSupport", n=1000, since="2018-01-01")

n.tweet <- length(samsung_tweets)

samsung_tweets.df <- twListToDF(samsung_tweets)
head(samsung_tweets.df)
head(samsung_tweets.df$text)


samsung_tweets.df2 <- gsub("http.*","",samsung_tweets.df$text)

samsung_tweets.df2 <- gsub("https.*","",samsung_tweets.df2)

samsung_tweets.df2 <- gsub("#.*","",samsung_tweets.df2)

#samsung_tweets.df2 <- gsub("@.*","",samsung_tweets.df2)

head(samsung_tweets.df2)


############### get sentiment score ##############

word.df <- as.vector(samsung_tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(samsung_tweets.df2, emotion.df) 
View(emotion.df2)

write.csv(emotion.df2, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Forecast Modelling\\Tableau FCST method\\emotion_samsung.csv", row.names = FALSE)

############ get positive sentiment ################

sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

########### get negative sentiment ################
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

sent.value

############ segregate positive and negative tweets #############

positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)

########### alternate way ##################

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)


####################### Bind the sentiment to the tweet ####################

head(samsung_tweets)
category_senti2 <- cbind(samsung_tweets.df2,category_senti, sent.value) 
head(category_senti2)

############ count of sentiment ################
table(category_senti)


########################################################################################
#################Sentiment Analysis - Part II - All Sentiments ##################

#getting emotions using in-built function
mysentiment_samsung <- get_nrc_sentiment((samsung_tweets.df$text))

#calculationg total score for each sentiment
Sentimentscores_samsung <- data.frame(colSums(mysentiment_samsung[,]))
Sentimentscores_samsung

names(Sentimentscores_samsung)<-"Score"
Sentimentscores_samsung <-cbind("sentiment"=rownames(Sentimentscores_samsung),Sentimentscores_samsung)
rownames(Sentimentscores_samsung)<-NULL


View(Sentimentscores_samsung)
#plotting the sentiments with scores
ggplot(data=Sentimentscores_samsung,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Samsung")


######################################################################################################
################# Apple Sentiment Analysis - Part I - Postive and Negative Sentiment##################

apple_tweets <- userTimeline("AppleSupport", n=1000, since="2018-01-01")

n.tweet <- length(apple_tweets)

apple_tweets.df <- twListToDF(apple_tweets)
head(apple_tweets.df)
head(apple_tweets.df$text)


apple_tweets.df2 <- gsub("http.*","",apple_tweets.df$text)
apple_tweets.df2 <- gsub("https.*","",apple_tweets.df2)
apple_tweets.df2 <- gsub("#.*","",apple_tweets.df2)
#apple_tweets.df2 <- gsub("@.*","",apple_tweets.df2)

head(apple_tweets.df2)


############### get sentiment score ##############

word.df <- as.vector(apple_tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(apple_tweets.df2, emotion.df) 

write.csv(emotion.df2, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Forecast Modelling\\Tableau FCST method\\emotion_apple.csv", row.names = FALSE)

############ get positive sentiment ################

sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive

########### get negative sentiment ################
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

sent.value

############ segregate positive and negative tweets #############

positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)

########### alternate way ##################

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)


####################### Bind the sentiment to the tweet ####################

head(apple_tweets)
category_senti2 <- cbind(apple_tweets.df2,category_senti, sent.value) 
head(category_senti2)

############ count of sentiment ################
table(category_senti)


################################################################################################
################# Sentiment Analysis - Part II - All Sentiments ##################

#getting emotions using in-built function
mysentiment_apple <- get_nrc_sentiment((apple_tweets.df$text))

#calculationg total score for each sentiment
Sentimentscores_apple <- data.frame(colSums(mysentiment_apple[,]))
Sentimentscores_apple

names(Sentimentscores_apple)<-"Score"
Sentimentscores_apple <-cbind("sentiment"=rownames(Sentimentscores_apple),Sentimentscores_apple)
rownames(Sentimentscores_apple)<-NULL

#plotting the sentiments with scores
ggplot(data=Sentimentscores_apple,aes(x=sentiment,y=Score)) + 
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none") +
  xlab("Sentiments") + ylab("scores") + 
  ggtitle("Sentiments of people behind the tweets on Apple")




Med insights


# install.packages("NLP", lib="C:/Program Files/R/R-3.5.0/library")
# install.packages("syuzhet", lib="C:/Program Files/R/R-3.5.0/library")
# install.packages("tm", lib="C:/Program Files/R/R-3.5.0/library")

library("NLP")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("ggplot2")
library("manipulate")
library("wordcloud")
library("dplyr")
library("topicmodels")


setwd("C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia")
getwd()

library(readxl)
IM_results <- read_excel("IM results.xlsx")
View(IM_results)

IM_data.df <- data.frame(IM_results)

#View(IM_data.df)

head(IM_data.df$reason)


IM_data.df2 <- gsub("http.*","",IM_data.df$reason)

IM_data.df2 <- gsub("https.*","",IM_data.df2)

IM_data.df2 <- gsub("#.*","",IM_data.df2)

IM_data.df2 <- gsub("[^0-9A-Za-z///' ]", "", IM_data.df2)

#IM_data.df2 <- gsub("paste0("\\b(",paste(stopwords, collapse="|"),")\\b")", IM_data.df2)

head(IM_data.df2)

#View(IM_data.df2)

#stopwords <- readLines('stopwords.txt')     #Your stop words file
IM_data.df2 <- removeWords(IM_data.df2,stopwords("english"))     #Remove stopwords
IM_data.df2 <- removeWords(IM_data.df2,stopwords("spanish"))

############### get sentiment score ##############

word.df <- as.vector(IM_data.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(IM_data.df2, emotion.df) 
#View(emotion.df2)

write.csv(emotion.df2, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia\\IM_emotions.csv", row.names = FALSE)

############ get positive sentiment ################

sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]
most.positive

########### get negative sentiment ################
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

sent.value

############ segregate positive and negative tweets #############

# positive.comments <- word.df[sent.value > 0]
# head(positive.comments)
# 
# negative.comments <- word.df[sent.value < 0]
# head(negative.comments)
# 
# View(word.df)
# 
# word.df <- word.df[!grepl("NA", word.df),]
# 
# neutral.comments <- word.df[sent.value == 0]
# head(neutral.comments)

########### alternate way ##################

category_senti <- ifelse(sent.value < 0.2, "Negative", ifelse(sent.value > 0.3, "Positive", "Neutral"))
head(category_senti)


######################### Plot the emotions in Barchart ##################################

#getting emotions using in-built function
#mysentiment_samsung <- get_nrc_sentiment((samsung_tweets.df$text))
View(emotion.df)

#calculationg total score for each sentiment
Sentimentscores_samsung <- data.frame(colSums(emotion.df[,]))
Sentimentscores_samsung

names(Sentimentscores_samsung)<-"Score"
Sentimentscores_samsung <-cbind("sentiment"=rownames(Sentimentscores_samsung),Sentimentscores_samsung)
rownames(Sentimentscores_samsung)<-NULL

View(Sentimentscores_samsung)

#plotting the sentiments with scores
ggplot(data=Sentimentscores_samsung,aes(x=sentiment,y=Score, label=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  geom_text(check_overlap = T,# automatically reduce overlap (deletes some labels)
            vjust = "bottom", # adjust the vertical orientation
            nudge_y = 1, # move the text up a bit so it doesn't touch the points
            size = 3 # make the text smaller (to reduce overlap more)
  ) +
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the comments on Samsung")

######################################################################################
####################### Bind the sentiment to the tweet #############################

category_senti2 <- cbind(IM_data.df,category_senti, sent.value) 
head(category_senti2)

write.csv(category_senti2, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia\\IM_senti_score.csv", row.names = FALSE)

############ count of sentiment ################
table(category_senti)


######################## Time Series Plot of sentiment ##############################


IM_data.df3 <- category_senti2

#View(IM_data.df3)

## Time series of positive-negative sentiments

library(scales)
IM_data.df3$Month <- as.Date(IM_data.df3$Responsedate)

#By Day
ggplot(IM_data.df3, aes(Responsedate, sent.value)) + geom_line() +
  xlab("Month") + ylab("Sentiments")
#By Month
ggplot(IM_data.df3, aes(Month, sent.value)) + geom_line() +
  xlab("Month") + ylab("Sentiments")

ggplot(IM_data.df3, aes(x = Month, y = sent.value)) + 
  geom_line(stat = "identity", color="#0072B2") +
  labs(x = "Month", y = "sentiments") +
  scale_x_date(labels = date_format("%m-%Y"))


####################################################################################################################################
######################################################### TEXT ANALYTICS ########################################################
####################################################################################################################################

#View(IM_data.df)

# build a corpus, and specify the source to be character vectors
reason_Corpus <- Corpus(VectorSource(IM_data.df$reason))

reason_Corpus <- sapply(reason_Corpus, function(row) iconv(row, "latin1", "ASCII", sub=""))
reason_Corpus <- Corpus(VectorSource(reason_Corpus))


#convert to lower case
reason_Corpus <- tm_map(reason_Corpus, content_transformer(tolower))

# # remove punctuation
reason_Corpus <- tm_map(reason_Corpus, removePunctuation)

# remove numbers
reason_Corpus <- tm_map(reason_Corpus, removeNumbers)

# remove URLs
removeURL <- function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)

# # ### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE)
reason_Corpus <- tm_map(reason_Corpus, content_transformer(removeURL))

# #

# add extra stop words: 'available' and 'via'
myStopwords1 <- c(stopwords("english"), "available", "via", "rt")
myStopwords2<- c(stopwords(source="smart"), "ae", "rt")

# remove stopwords from corpus
reason_Corpus <- tm_map(reason_Corpus, removeWords, myStopwords1)
#
reason_Corpus <- tm_map(reason_Corpus, removeWords, myStopwords2)

##

#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
reason_CorpusCopy <- reason_Corpus

# stem words
#apple_Corpus <- tm_map(apple_Corpus, stemDocument)
#samsung_Corpus <- tm_map(samsung_Corpus, stemDocument)
#google_Corpus <- tm_map(google_Corpus, stemDocument)


##################################################################################
################################# Mining #########################################

# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(reason_Corpus[[i]]))
}

#################### Stem completion ############################

# myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)

#apple_Corpus <- tm_map(apple_Corpus, content_transformer(stemCompletion), dictionary = apple_CorpusCopy)
#samsung_Corpus <- tm_map(samsung_Corpus, content_transformer(stemCompletion), dictionary = samsung_CorpusCopy)
#google_Corpus <- tm_map(google_Corpus, content_transformer(stemCompletion), dictionary = google_CorpusCopy)

#################################################################################################

################################## count frequency of “mining” ##################################


reason_tdm <- TermDocumentMatrix(reason_Corpus, control = list(wordLengths = c(3, Inf)))
reason_tdm

## Freqency words and Association
reason_idx <- which(dimnames(reason_tdm)$Terms == "battery")
reason_idx
inspect(reason_tdm[reason_idx + (0:10), 1:20])

#inspect frequent words
(freq.terms <- findFreqTerms(reason_tdm, lowfreq=100))

#(freq.terms1 <- findFreqTerms(reason_idx, lowfreq=100))

term.freq <- rowSums(as.matrix(reason_tdm))
term.freq <- subset(term.freq, term.freq >=5)
reason_df <- data.frame(term = names(term.freq), freq = term.freq)
#View(reason_df)

write.csv(reason_df, file = "C:\\Users\\aishvarya.b\\Documents\\Work related documents\\Medallia\\freqent_terms.csv", row.names = FALSE)


###################################### Plot the words - BARCHART ########################################

#View(reason_df)

sorted_reason_df <- reason_df[order(reason_df$freq, decreasing= TRUE),]

sorted_reason_df <- sorted_reason_df %>%
  slice(1:20)

ggplot(sorted_reason_df, aes(x=term, y=freq)) + geom_bar(stat = "identity", aes(x=reorder(term, desc(-freq)))) + 
  xlab("Terms") + ylab("Count") +coord_flip() +ggtitle("Frequent words - Reason for overall score")

##################### WORD ASSOCIATIONS ###########################################

# which words are associated with 'Battery'?
findAssocs(reason_tdm, "battery", 0.05)

# which words are associated with 'UPS'?
findAssocs(reason_tdm, "ups", 0.05)

# which words are associated with 'Shipping'?
findAssocs(reason_tdm, "shipping", 0.1)


################################################################################################################
####################################### WORDCLOUD METHOD #######################################################

#install.packages("wordcloud")
library("wordcloud")

reason_wc <- as.matrix(reason_tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(reason_wc), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 100,
          random.order = F, colors = brewer.pal(8, 'Set1'))

##############################################################################################
############################# Clustering ########################################

# remove sparse terms
reason_tdm2 <- removeSparseTerms(reason_tdm, sparse = 0.96)
reason_m2 <- as.matrix(reason_tdm2)

# cluster terms
distMatrix1 <- dist(scale(reason_m2))
fit1 <- hclust(distMatrix1, method = "ward.D")

plot(fit1)
rect.hclust(fit1, k = 8) # cut tree into 6 clusters


##########################################################################################################
##################################### Topic Model ################################################

library(topicmodels)

reason_dtm <- as.DocumentTermMatrix(reason_Corpus)

#View(reason_dtm)

rowTotals <- apply(reason_dtm , 1, sum) #Find the sum of words in each Document
reason_dtm_new   <- reason_dtm[rowTotals> 0, ] #remove all docs without words

## 
reason_lda <- LDA(reason_dtm_new, k = 8) # find 8 topics
reason_term <- terms(reason_lda, 12) # first 6 terms of every topic
reason_term

##
require(data.table) #fore IDate
reason_term <- apply(reason_term, MARGIN = 2, paste, collapse = ", ")
reason_term


######################### BARPLOTS of FREQ WORDS ###############################################

m <- as.matrix(reason_tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


################ Plot Promotor or Detractor over time #######################################

########################## Plot Promoter #############################

library(scales)
IM_data.df3$Month <- as.Date(IM_data.df3$Service.Date)

library(dplyr)
pro.df <- filter(IM_data.df3, IM_data.df3$Alert.type == 'Promoter')

pro.df$Service.Date <- as.Date(pro.df$Service.Date)
freqs <- aggregate(pro.df$Object.ID, by=list(pro.df$Service.Date), FUN=length)
freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")

ggplot(freqs, aes(x=names, y=x, label=x, width=0.7)) + geom_bar(stat="identity", fill = "#009E73") +
  geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.001,angle = 30,size = 4) + 
  scale_x_date(breaks="1 week", labels=date_format("%m-%d"),
               limits=c(as.Date("2018-05-15"),as.Date("2018-11-3"))) +
  ylab("Frequency") + xlab("Service Date") + ggtitle("Promoters over Time")

View(pro.df)



######################### Plot Detractor ##############################


det.df <- filter(IM_data.df3, IM_data.df3$Alert.type == 'Detractor')

det.df$Service.Date <- as.Date(det.df$Service.Date)
freqs <- aggregate(pro.df$Object.ID, by=list(det.df$Service.Date), FUN=length)
freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")

ggplot(freqs, aes(x=names, y=x, label=x, width=0.7)) + geom_bar(stat="identity", fill = "#FF6666") +
  geom_text(check_overlap = T,vjust = "bottom",nudge_y = 0.001,angle = 30,size = 4) +
  scale_x_date(breaks="1 week", labels=date_format("%m-%d"),
               limits=c(as.Date("2018-05-15"),as.Date("2018-11-3"))) +
  ylab("Frequency") + xlab("Service Date") + ggtitle("Detractors over Time")

View(det.df)

