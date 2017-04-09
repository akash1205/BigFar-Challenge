setwd("/Users/akashjain/Desktop/BigFar Challenge/Twitter Sentiment Analysis")
install.packages("devtools")
install.packages("Rstem")
require(devtools)

install_url("http://www.omegahat.net/Rstem/Rstem_0.4-1.tar.gz")
require(Rstem)

install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

library(sentiment)

library(plyr)

library(ggplot2)

install.packages("wordcloud")



install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")

library(slam)

library(wordcloud)

library(RColorBrewer)

Demonetization_tweets = read.csv(file ="Demonetizationfinal.csv" ,header = TRUE)




Demonetization_text = Demonetization_tweets$text



Demonetization_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Demonetization_text)



Demonetization_text = gsub("@\\w+", "", Demonetization_text)



Demonetization_text = gsub("[[:punct:]]", "", Demonetization_text )



Demonetization_text = gsub("[[:digit:]]", "", Demonetization_text)



Demonetization_text = gsub("http\\w+", "", Demonetization_text)



Demonetization_text = gsub("[ \t]{2,}", "", Demonetization_text)

Demonetization_text = gsub("^\\s+|\\s+$", "", Demonetization_text)

try.error = function(x)
  
{
  
  
  
  y = NA
  
  
  
  try_error = tryCatch(tolower(x), error=function(e) e)
  
  
  
  if (!inherits(try_error, "error"))
    
    y = tolower(x)
  
 
  
  return(y)
  
}

Demonetization_text = sapply(Demonetization_text, try.error)



Demonetization_text = Demonetization_text [!is.na(Demonetization_text)]

names(Demonetization_text) = NULL

class_emo = classify_emotion(Demonetization_text, algorithm="bayes", prior=1.0)



emotion = class_emo[,7]



emotion[is.na(emotion)] = "unknown"



class_pol = classify_polarity(Demonetization_text, algorithm="bayes")



polarity = class_pol[,4]



sent_df = data.frame(text= Demonetization_text, emotion=emotion,
                     
                     polarity=polarity, stringsAsFactors=FALSE)



sent_df = within(sent_df,
                 
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))




ggplot(sent_df, aes(x=emotion)) +
  
  geom_bar(aes(y=..count.., fill=emotion)) +
  
  scale_fill_brewer(palette="Dark2") +
  
  labs(x="emotion categories", y="number of tweets") +
  
  labs(title = "Sentiment Analysis of Tweets about Demonetization\n(classification by emotion)")




ggplot(sent_df, aes(x=polarity)) +
  
  geom_bar(aes(y=..count.., fill=polarity)) +
  
  scale_fill_brewer(palette="Dark2") +
  
  labs(x="polarity categories", y="number of tweets") +
  
  labs(title = "Sentiment Analysis of Tweets about Demonetization\n(classification by polarity)")

