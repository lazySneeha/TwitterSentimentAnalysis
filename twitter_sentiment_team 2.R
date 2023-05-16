# install packages
#install.packages("rtweet")
#install.packages("syuzhet")
#install.packages("tidytext")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("NLP")
#install.packages("dplyr")

# load packages
library(rtweet)
library(syuzhet)
library(tidytext)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(NLP)
library(dplyr)
library(writexl)
library(reshape2)
library(ggplot2)
library(textstem)

TSLA <- read.csv("TSLA_data_2023.csv")
# set up authentication
#auth_setup_default()

# sentiment analysis using syuzhet
TSLAsent_syuzhet <- get_sentiment(TSLA$text, method = "syuzhet")
View(get_sentiment_dictionary())
TSLA$text[4]

# sentiment analysis using afinn
TSLAsent_afinn <- get_sentiment(TSLA$text, method = "afinn")
View(get_sentiment_dictionary(dictionary = "afinn"))

# sentiment analysis using nrc
TSLAsent_nrc <- get_sentiment(TSLA$text, method = "nrc")
View(get_sentiment_dictionary(dictionary = "nrc"))


# convert TSLA$full_text to character vector
#TSLA$text <- as.character(TSLA$text)

# get nrc sentiment data
TSLA_nrc_data <- get_nrc_sentiment(TSLA$text)
View(TSLA_nrc_data)
dim(TSLA_nrc_data)
summary(TSLA_nrc_data)

# combine sentiment data with TSLA data
TSLA <- cbind(TSLA, TSLAsent_afinn, TSLAsent_syuzhet, TSLAsent_nrc, TSLA_nrc_data)

# visualize sentiment analysis
# set the margin size to be smaller
par(mar = c(4, 4, 2, 1))
plot(TSLAsent_nrc, type = "l")
# set x-axis limits
#plot(TSLA$created_at, TSLA$TSLAsent_nrc, xlim = c(0, 10))


# plot bar chart
barplot(TSLAsent_nrc, main = "Sentiment Analysis using NRC", xlab = "Sentiment Categories", ylab = "Count")


# remove duplicates
attach(TSLA)
dup <- duplicated(user_id)
table(dup)

NewTSLA <- TSLA[!duplicated(TSLA$user_id), ]
attach(NewTSLA)
dup <- duplicated(user_id)
table(dup)

# create wordcloud
worddata <- NewTSLA %>%
  unnest_tokens(word, text)
View(worddata)

worddata %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# create keyword variables
Delivery <- grepl("Delivery", text, ignore.case = TRUE)
table(Delivery)

Deals <- grepl("Deals", text, ignore.case = TRUE)
table(Deals)

Electric <- grepl("Electric", text, ignore.case = TRUE)
table(Electric)

Autopilot <- grepl("Autopilot", text, ignore.case = TRUE)
table(Autopilot)

Battery <- grepl("Battery", text, ignore.case = TRUE)
table(Battery)

NewTesla <- cbind(NewTSLA,Delivery, Deals, Electric, Autopilot, Battery )

NewTesla2 <- subset(NewTesla, select = -c(Delivery, Deals, Electric, Autopilot, Battery))
View(NewTesla2)

#Cleaning Tweets
# create a dataframe with only interested variables
attach(NewTesla2)
data_tweet <- NewTesla[,c("user_id","text")] 
data_tweet <- data.frame(doc_id = user_id, text = text, stringsAsFactors = FALSE)

# Construct the corpus
# let R (tm package) know that we use this data as our corpus for text analysis
tweets_1 = Corpus(DataframeSource(data_tweet))
tweets_1[[1]]$content   #check the text of first document


# noise removal -- for tweets
#  Retweet removal
removeRT <- function(x){gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)}
tweets_2 = tm_map(tweets_1,content_transformer(removeRT))
tweets_2[[1]]$content

#  Hashtag removal
removeHashtag <- function(x){gsub("#\\S+", "", x)}
tweets_3 = tm_map(tweets_2,content_transformer(removeHashtag))
tweets_3[[1]]$content

#  URL removal
removeURL <- function(x){gsub("http[^[:space:]]*", "", x)}
tweets_4 = tm_map(tweets_3,content_transformer(removeURL))
tweets_4[[1]]$content

#  HTML removal
unescapeHTML <- function(str) {return(gsub("<.*?>", "", str))}
tweets_5 = tm_map(tweets_4,content_transformer(unescapeHTML))
tweets_5[[1]]$content

# Mention removal
removeMention <- function(x){gsub("@\\w+", "", x)}
tweets_6 = tm_map(tweets_5,content_transformer(removeMention))
tweets_6[[1]]$content

#  Carriage removal
removeCarriage <- function(x){gsub("[\r\n]", "", x)}
tweets_7 = tm_map(tweets_6,content_transformer(removeCarriage))
tweets_7[[1]]$content

#   Emoticon removal
removeEmoticon <- function(x){gsub("[^\x01-\x7F]", "", x)}
tweets_8 = tm_map(tweets_7,content_transformer(removeEmoticon))
tweets_8[[1]]$content



#  General pre- processing procedures
# Lowercase
tweets_9 = tm_map(tweets_8,content_transformer(tolower))
tweets_9[[1]]$content

# Remove Punctuation
tweets_10 <- tm_map(tweets_9, removePunctuation)
tweets_10[[1]]$content

#  remove Numbers
tweets_11 <- tm_map(tweets_10,removeNumbers)
tweets_11[[1]]$content

#  remove stopwords
tweets_12 <- tm_map(tweets_11,removeWords,stopwords("english"))
tweets_12[[1]]$content

#  remove task specific stopwords
tweets_13 <- tm_map(tweets_12,removeWords,c("tesla", "t.co", "https", "support"))
tweets_13[[1]]$content

#  strip Whitespace
tweets_14 <- tm_map(tweets_13,stripWhitespace)
tweets_14[[1]]$content

#  word stemming
tweets_15<-tm_map(tweets_14,stemDocument)
tweets_15[[1]]$content

#  word lemmatization
#install.packages('textstem')

tweets_16 <- tm_map(tweets_15, lemmatize_strings)
tweets_16[[1]]$content

tweets17 <- data.frame(text = sapply(tweets_16, as.character), stringsAsFactors = FALSE)
tweets18 <- cbind(Index = rownames(tweets17),user_id, tweets17)
rownames(tweets18) <- 1:nrow(tweets18)

# Create word  cloud with new data set
Teslaworddata <- tweets18 %>%
  unnest_tokens(word, text,to_lower = T)# output=word, input=text, text: case sensitive



# Writes the Teslaworddata dataframe to an Excel file named "Tesla_word_data.xlsx"
write_xlsx(Teslaworddata, "Tesla_word_data.xlsx")

# Writes the NewTesla2 dataframe to an Excel file named "NewTesla2.xlsx"
write_xlsx(NewTesla2, "NewTesla2.xlsx")



# wordcloud
Teslaworddata %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(6,"Dark2")))




png("WordCloud_Tesla_tweets.png", width = 12, height = 8, units = "in", res = 300)
Teslaworddata %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("grey", "blue"),
                   max.words = 100)
dev.off()


#get the emotions using the NRC dictionary
emotions <- get_nrc_sentiment(text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))

png("quickplot_emotion_Tesla_tweets.png", width = 12, height = 8, units = "in", res = 300)
quickplot(emotion, data=emo_sum, weight=count, geom="bar", fill=emotion, ylab="count")+ggtitle("Tesla Tweets emotion")
dev.off()
# Remove the duplicate columns
NewTesla <- NewTesla[!names(NewTesla) %in% duplicates]

# Calculate AFINN sentiment scores for NewTesla
NewTesla_sent_afinn <- get_sentiment(NewTesla$text, method = "afinn")

# Calculate Syuzhet sentiment scores for NewTesla
NewTesla_sent_syuzhet <- get_sentiment(NewTesla$text, method = "syuzhet")

# Calculate NRC sentiment scores for NewTesla
NewTesla_nrc_data <- get_nrc_sentiment(NewTesla$text)

# Combine sentiment data with NewTesla data
NewTesla <- cbind(NewTesla, TSLAsent_afinn = NewTesla_sent_afinn, TSLAsent_syuzhet = NewTesla_sent_syuzhet, NewTesla_nrc_data)

# Check column names of NewTesla
#colnames(NewTesla)

# Identify the duplicate columns
#duplicates <- colnames(NewTesla)[duplicated(colnames(NewTesla))]

# Check column names again
#colnames(NewTesla)
# Check the lengths again
cat("Length of AFINN scores:", length(NewTesla$TSLAsent_afinn), "\n")
cat("Length of Syuzhet scores:", length(NewTesla$TSLAsent_syuzhet), "\n")
cat("Number of rows in NewTesla data frame:", nrow(NewTesla), "\n")


# Add AFINN sentiment scores to NewTesla
#NewTesla$TSLAsent_afinn <- NewTesla_sent_afinn

# Add Syuzhet sentiment scores to NewTesla
#NewTesla$TSLAsent_syuzhet <- NewTesla_sent_syuzhet

# Check the lengths again
cat("Length of AFINN scores:", length(NewTesla$TSLAsent_afinn), "\n")
cat("Length of Syuzhet scores:", length(NewTesla$TSLAsent_syuzhet), "\n")
cat("Number of rows in NewTesla data frame:", nrow(NewTesla), "\n")

# Calculate the correlation coefficient
correlation <- cor(NewTesla$TSLAsent_afinn, NewTesla$TSLAsent_syuzhet)
cat("Correlation coefficient between AFINN and Syuzhet sentiment scores:", correlation, "\n")

# Create a scatterplot with a linear regression line
ggplot(NewTesla, aes(x = TSLAsent_afinn, y = TSLAsent_syuzhet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_minimal() +
  ggtitle("Scatterplot of AFINN vs. Syuzhet Sentiment Scores") +
  xlab("AFINN Sentiment Scores") +
  ylab("Syuzhet Sentiment Scores")


# Calculate the average NRC sentiment score for each tweet
NewTesla$TSLAsent_nrc_diff <- rowMeans(NewTesla_nrc_data[, c("positive", "negative")])

# Calculate the correlation coefficients
correlation_nrc_syuzhet <- cor(NewTesla$TSLAsent_nrc_diff, NewTesla$TSLAsent_syuzhet)
correlation_nrc_afinn <- cor(NewTesla$TSLAsent_nrc_diff, NewTesla$TSLAsent_afinn)

cat("Correlation coefficient between NRC and Syuzhet sentiment scores:", correlation_nrc_syuzhet, "\n")
cat("Correlation coefficient between NRC and AFINN sentiment scores:", correlation_nrc_afinn, "\n")

# Create a scatterplot for NRC vs Syuzhet sentiment scores
ggplot(NewTesla, aes(x = TSLAsent_nrc_diff, y = TSLAsent_syuzhet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_minimal() +
  ggtitle("Scatterplot of NRC vs. Syuzhet Sentiment Scores") +
  xlab("NRC Sentiment Scores") +
  ylab("Syuzhet Sentiment Scores")

# Create a scatterplot for NRC vs AFINN sentiment scores
ggplot(NewTesla, aes(x = TSLAsent_nrc_diff, y = TSLAsent_afinn)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  theme_minimal() +
  ggtitle("Scatterplot of NRC vs. AFINN Sentiment Scores") +
  xlab("NRC Sentiment Scores") +
  ylab("AFINN Sentiment Scores")

# Perform linear regression for AFINN vs. Syuzhet sentiment scores
lm_afinn_syuzhet <- lm(TSLAsent_syuzhet ~ TSLAsent_afinn, data = NewTesla)

# View the summary of the regression model
summary(lm_afinn_syuzhet)

# Perform linear regression for AFINN vs. NRC sentiment scores
lm_afinn_nrc <- lm(TSLAsent_nrc ~ TSLAsent_afinn, data = NewTesla)

# View the summary of the regression model
summary(lm_afinn_nrc)

# Perform linear regression for Syuzhet vs. NRC sentiment scores
lm_syuzhet_nrc <- lm(TSLAsent_nrc ~ TSLAsent_syuzhet, data = NewTesla)

# View the summary of the regression model
summary(lm_syuzhet_nrc)

# Load the data
nrc_data <- read.csv("nrc.csv")

# Display the column names
colnames(nrc_data)

# Display unique sentiment values
unique_sentiments <- unique(nrc_data$sentiment)
print(unique_sentiments)


