#This coding was built to crawling twitter data and analyze it
#this analyze include wordcloud analysis, positive and negative word pattern and sentiment analysis

# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
library(RCurl)
# plotting and pipes - tidyverse!
library(ggplot2)
library(plyr)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)
library(httr)
library(devtools)
library(usethis)
library(Rcpp)
library(httpuv)
library(writexl)
#dari Pak ade
library(stringr)
library(tm)
library(wordcloud)
library(readxl)
library(ggplot2)
library(xlsx)
library(RColorBrewer)
library(ROAuth)
#tambahan
library(tidyr)
library(widyr)

# your app in twitter API
appname <- "psbb1"

## api key (just example)
key <- "KNr44aHeSpgi0uYovd6tgV6CZ"

## api secret (just example)
secret <- "DdI86kbBPF4P3QomiSytnblxhSdCVQ3vuizritJW7l5jm7l2Dn"

# this coding was to create token so you can connect with twitter API to crawling twitter data
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret
  )

#search 10000 tweets with contains word "Psbb" using indonesian language
tweets.by.terms2 <- search_tweets(q = c("psbb"), n = 10000,
                                lang = "id",
                                include_rts = FALSE,
                                retryonratelimit=TRUE)

#save it with xlsx file

write_xlsx(tweets.by.terms2,"E:/aa/PL 4103 Sistem Informasi Perencanaan/Intro to Twitter Analysis 2/Tugas SIP/Tugas_SIP.xlsx")#path to store the Excel file\\file name.xlsx")
#open your saved file
file_excel<-read_excel("E:/aa/PL 4103 Sistem Informasi Perencanaan/Intro to Twitter Analysis 2/Tugas SIP/Tugas_SIP41.xlsx")
View(file_excel)#optional if you want to open it on your apps

#this coding used to identify twitter user location with top 10 location 
file_excel%>%
  count(location) %>%
  top_n(10)%>%
  mutate(location = reorder(location, n)) %>%
  ggplot(aes(x = n, y = location)) +
  geom_col() +
  geom_text(aes(label=n), hjust=-0.5, color="black", size=2) +
  labs(x = "Count",
       y = "Location",
       title = "10 Lokasi Terbanyak Dari Pengguna")

#Text Analysis
#build a corpus (from tweets) - Corpus (the plural form of corpora) is large and structured sets of texts
tw.corpus <- Corpus(VectorSource(file_excel$text))

# Convert to lower case
tw.corpus.proc <- tm_map(tw.corpus, tolower)

# remove punctuation and numbers
tw.corpus.proc <- tm_map(tw.corpus.proc, removePunctuation)
tw.corpus.proc <- tm_map(tw.corpus.proc, removeNumbers)

#create a user-defined function to remove URL, 'http' followed by non-space characters
removeURL <- function(x) gsub("http[^[:space:]]*", "", x) #This is a function to remove url

# remove URL using removeURL function
tw.corpus.proc <- tm_map(tw.corpus.proc, removeURL)

#create a user-defined function to remove emoticon (emoticon was converted to ASCII code)
removeEmoticon <- function(x) gsub("[^\x01-\x7F]", "", x)

#Remove emoticon using removeEmoticon function
tw.corpus.proc <- tm_map(tw.corpus.proc, removeEmoticon)

#Remove stopwords - stopwords are words that do not have any additional meaning to the core concept words

#We will build our list of stop words that are suitable for indonesian tweets
#we will use the list of stopword developed by ...

stopwords.id <- t(read.delim("E:/aa/PL 4103 Sistem Informasi Perencanaan/Intro_to_Twitter_Analyses-master/id.stopwords.02.01.2016.txt")) # t is the transpose function
View(stopwords.id)
# remove stopwords from tweets using our predefined Indonesian stopwords
tw.corpus.proc <- tm_map(tw.corpus.proc, removeWords, stopwords.id)

# We should anticipate that there might be some tweets writtern in English, we will remove English stopwords as well
tw.corpus.proc <- tm_map(tw.corpus.proc, removeWords, stopwords("english"))

#we should remove word which not related to PSBB and remove word "PSBB"
tw.corpus.proc <- tm_map(tw.corpus.proc, removeWords, c("kab", "blm", "krn", "klo","udh", "gue","berlaku","jawa","gubernur","jam", "pas","biar", 
                                                        "psbb", "penerapan", "pembatasan", "wilayah","bener","nih", "detikcom","bang", "cnnindonesia", "tuh",
                                                        "min", "udah", "gini", "masuk", "sosial","tau", "mah", "gua", "gue", "kasih", "banget", "wkwk", "spt", "siang", "yuk", "dpt",
                                                        "orang", "barat","kabupaten", "daerah", "kak", "gitu","emang", "msh", "sampe", "sdh", "berskala", "besar","kota","diberlakukan", "g",
                                                        "iya", "via", "tdk", "udah", "april", "nya", "gimana", "pakai", "nya", 
                                                        "sih","dgn","gak", "psbb","aja","ada","kalo","utk","dgn"))
#Review the word frequency
tw_tdm <- TermDocumentMatrix(tw.corpus.proc) 
tw_tdm.mat <- as.matrix(tw_tdm) #change to matrix
tw_tdm.mat <- sort(rowSums(tw_tdm.mat),decreasing=TRUE) # Sort the words by frequency
tw_tdm.mat <- data.frame(word = names(tw_tdm.mat),freq=tw_tdm.mat) # Change to dataframe
head(tw_tdm.mat, 20) # view the first 20 words

#reading pos words and neg words as data frame  
pos.words <- read.csv("E:/aa/PL 4103 Sistem Informasi Perencanaan/Intro to Twitter Analysis 2/positive.txt")
neg.words <- read.csv("E:/aa/PL 4103 Sistem Informasi Perencanaan/Intro to Twitter Analysis 2/negative.txt")

neg.words<-unlist(neg.words)#unlist list of word
neg.words<-as.vector(neg.words)# convert to vector

pos.words<-unlist(pos.words)#unlist list of word
pos.words<-as.vector(pos.words)# convert to vector

#identify negative words in data frame contain word matrix
required_neg <- tw_tdm.mat[tw_tdm.mat$word %in% neg.words$abnormal,]
View(required_neg)

#identify positive words in data frame contain word matrix
required_pos <- tw_tdm.mat[tw_tdm.mat$word %in% pos.words$a.,]
View(required_pos)

#creating wordcloud for negative words
set.seed(1234)
wordcloud(words = required_neg$word, freq = required_neg$freq, min.freq = 10,
          max.words=50, random.order=F, scale=c(4,.5),
          colors=brewer.pal(8, "Dark2"))

#creating barplot for top 20 of negative words
required_neg %>% top_n(20) %>% ggplot(aes(x=freq, y=reorder(word, freq))) + geom_bar(stat = "identity") + geom_text(aes(label=freq),vjust=0.3, hjust=1, color="white", size=2)+ labs(x= "frekuensi", y= "kata", title="Tingkat Kemunculan dari 20 Kalimat\n Negatif Terbanyak")

#creating wordcloud for positive words
set.seed(1234)
wordcloud(words = required_pos$word, freq = required_pos$freq, min.freq = 10,max.words=60, random.order=FALSE, scale=c(4,.5), colors=brewer.pal(8, "Dark2"))
#Display word frequency using word cloud

#creating barplot for top 20 of positive words
required_pos %>% top_n(20) %>% ggplot(aes(x=freq, y=reorder(word, freq))) + geom_bar(stat = "identity") + geom_text(aes(label=freq),vjust=0.3, hjust=1, color="white", size=2)+ labs(x= "frekuensi", y= "kata",title="Tingkat Kemunculan dari 20 Kalimat\n Positif Terbanyak")

#creating wordcloud for top 50words (all of them including positive and negative ones)
set.seed(1234)
wordcloud(tw.corpus.proc,min.freq=10,max.words=50,scale=c(2,.5), colors=brewer.pal(8, "Dark2"), random.order=F)

#creating barplot fot top 50 words
tw_tdm.mat %>% top_n(20) %>% ggplot(aes(x=freq, y=reorder(word, freq))) + geom_bar(stat = "identity") + geom_text(aes(label=freq),vjust=0.3, hjust=1, color="white", size=2)+ labs(x= "frekuensi", y= "kata",title="Tingkat Kemunculan dari 20 Kalimat Terbanyak")

pos.words<-as.list(pos.words)
neg.words<-as.list(neg.words)

##################
# Step 6: Analyzing tweets - sentiments and polarizations
##################
# load and scan the words into R

# Create a function for sentiment analysis based on Rohit Nair's script.
sentiment.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#Apply sentiments function to the tweets
result <- sentiment.score(file_excel$text,pos.words,neg.words)

#separate negative sentence, positive sentence, and neutral sentence
#create new csv contain negative sentence
result_neg<-which(result["klasifikasi"]=="Negatif")
data_neg<-result[result_neg,][c(3,1,2)]
View(data_neg)
write_xlsx(result, "E:/aa/PL 4103 Sistem Informasi Perencanaan/Intro to Twitter Analysis 2/Tugas SIP/hasil akhit.xlsx")

#create new csv contain positive sentence
result_pos<-which(result["klasifikasi"]=="Positif")
data_pos<-result[result_pos,][c(3,1,2)]
View(data_pos)
write.csv(data_pos, file='E:/aa/PL 4103 Sistem Informasi Perencanaan/Intro to Twitter Analysis 2/Tugas SIP/data_pos.csv', row.names=F)

#create new csv contain neutral sentence
result_net<-which(result["klasifikasi"]=="Netral")
data_net<-result[result_net,][c(3,1,2)]
View(data_net)
write.csv(data_pos, file='E:/aa/PL 4103 Sistem Informasi Perencanaan/Intro to Twitter Analysis 2/Tugas SIP/data_net.csv', row.names=F)

# View the summary of the results 
summary(result$score)
#creating histogram
hist(result$score, col = "yellow", main = 'Score of tweets', ylab = 'Count of tweets')
count(result$score)
#creating barplot
qplot(result$score,xlab = "Score of tweets") + geom_text(aes(label=result$score),vjust=0.3, hjust=1, color="white", size=2)

