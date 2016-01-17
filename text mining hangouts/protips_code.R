library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(RJSONIO)
library(jsonlite)
library(lubridate)
library(tm)
library(RTextTools)
library(wordcloud) 
library(glmnet)
library(Matrix)
library(zoo)
library(cluster)
library(HSAUR)

hangouts.json <- fromJSON(txt = "Hangouts.json", flatten = TRUE)

## transform hiearchical key:value format of json into a table with columns and rows
## llply takes in a list as its first arugment, applies a function to every element in the list, and returns a list
## laply takes in a list, applies a function to every element in list, and returns a vector
## we are extracting just the text value from every object in the json file, and combining it with the timestamp and participantid values
## each individual chat is its own dataframe, and we combine it all into a list object
## note: the ply family of functions are easily parallelizable for larger datasets, see plyr documentation for more details

hangouts.texts.list <- 
  llply(hangouts.json[[2]]$conversation_state.event, function(x) {
    
    data.frame(
      cbind(
        text = laply(x$chat_message.message_content.segment, function(y) {
          
          text <- ifelse(length(y$text) == 0,"", y$text)
          
          
        }, .progress = "text"),
        timestamp = x$timestamp,
        gaiai_id = x$sender_id.gaia_id
      ))
    
  }, .progress = "text")


## grab participantid to participant name mappings

participant.data <- 
  unique(ldply(hangouts.json[[2]]$conversation_state.conversation.participant_data, function(z) {
    
    data.frame(z[,c("fallback_name","id.gaia_id")])
    
  }))

## piping is a functionality native to the dplyr package, allows us to write easy to read sequential code
## in sql speak this is: select id.gaiai_id, max(fallback_name) = fallback_name from participant.data group by 1;

participant.data <- participant.data %>% group_by(id.gaia_id) %>% summarise(fallback_name = max(fallback_name))
  

## it would be useful to label each element in the hangouts list we created earlier
## I chose a simple label: concatenate all the participant names in a chat
hangouts.labels <-
  laply(hangouts.json[[2]]$conversation_state.conversation.participant_data, function(a) {
    
    paste(a$fallback_name,collapse="_",sep="")
    
  })

names(hangouts.texts.list) <- hangouts.labels
names(hangouts.texts.list)

## For simplicity sake, I choose to perform my analysis on one specific chat: you can easily use vectorized functions to
## perform your analysis on every single chat in one go.
## I chose a group chat to make it more interesting.

hangout.data <- hangouts.texts.list[[16]]


## Some data cleaning:
## convert UNIX timestasmp to POSIXct timestamps
## calculate wordcounts- simple algorithm counts the number of spaces in each text blob
## other calculated columns such as date, hour, and minute

hangout.data$timestamp = as.POSIXct(as.numeric(levels(hangout.data$timestamp))[hangout.data$timestamp]/1000000, origin = "1970-01-01")
hangout.data$wordcount = laply(gregexpr(" ",hangout.data$text), function(x) length(x))
hangout.data$date = as.Date(hangout.data$timestamp)
hangout.data$hour = hour(hangout.data$timestamp)
hangout.data$minute = minute(hangout.data$timestamp)
hangout.data$gaiai_id <- as.character(hangout.data$gaiai_id)
hangout.data$text <- as.character(hangout.data$text)
hangout.data$text <- iconv(hangout.data$text, "latin1", "ASCII", sub="") ## removes emojis and other nonstandard characters


## add participant name by joining to our participant mapping dataframe
## merge is the function for performing joins
## can be slow for larger data, I recommend the data.table package

hangout.data <- merge(hangout.data,participant.data, by.x = "gaiai_id", by.y = "id.gaia_id",all.x = TRUE)
hangout.data$participant <- ifelse(is.na(hangout.data$fallback_name), "Jason Jea", hangout.data$fallback_name)
## note: there are some data gaps in Google's export.  For example, in this particular chat my participant id maps to an 
##       empty string name.  I do some manual processing to label text that is coming from me


## our first plot!  simple looking at word counts trended, split by sender

## the first block here does some fast data cleaning: filtering by data, aggregating at participant and date level,
##                                                    and calculating moving averages
hangout.data %>% filter(date <= as.Date("2015-12-25"), date >= as.Date("2015-06-01")) %>%
  group_by(date,participant) %>% summarise(wordcount = sum(wordcount)) %>% ungroup %>%
  group_by(participant) %>%
  mutate(smooth = rollmean(wordcount,7,na.pad=TRUE,align="left")) %>% ungroup %>%
## the actual plotting using ggplot: aes() defines x, y, and legends
## geom defines the type of plot
## all other arguments are for formatting
  ggplot(aes(x=date,y=smooth, colour = participant)) + geom_line(size = 1) +
  scale_x_date(name = "Date", breaks = date_breaks("2 weeks")) +
  scale_colour_brewer(palette = "Set1") +
  scale_y_continuous(name="Word Count") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20, vjust = .75, hjust = .6, angle = 90), 
        axis.text.y = element_text(size = 20), plot.title = element_text(size = 25),
        legend.text = element_text(size=20),
        legend.title = element_blank()) 
ggsave(file="plots/wordcountstrended.png",dpi =300, width=15,height=7.5, type = "cairo-png") 

## chat frequency by hour of day, split by sender

hangout.data %>% group_by(participant, hour) %>% summarise(wordcount = sum(wordcount)) %>% ungroup %>%
  group_by(participant) %>% mutate(percentmix = wordcount/sum(wordcount)) %>% ungroup %>%
  ggplot(aes(x=hour,y=percentmix, colour = participant)) + geom_line(size = 2) +
  scale_x_continuous(name = "Hour of Day", breaks = seq(0,24,1)) +
  scale_y_continuous(name = "Percentage of Chat Frequency",labels = percent, breaks = seq(0,.14,.02), limits = c(0,.14)) +
  scale_colour_brewer(palette = "Set1") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.title.y = element_text(size = 20), axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20, vjust = .75, hjust = .6), 
        axis.text.y = element_text(size = 20), plot.title = element_text(size = 25),
        legend.text = element_text(size=20),
        legend.title = element_blank()) 



## create summary dataframe, grouping by participant and ate

hangout.summary <-
  hangout.data[,c("date","participant","text","wordcount")] %>%
  group_by(date,participant) %>%
  summarise(text =paste(text, collapse = " "), wordcount = sum(wordcount)) %>%
  group_by(date) %>% mutate(daycount = sum(wordcount)) %>% ungroup %>%
  filter(wordcount >= 300)
hangout.summary$document <- paste(hangout.summary$date,hangout.summary$participant, sep = " ")



## create corpus (object that allows you to perform fast text manipulation on your data)
## examples: removing punctioning, stemming

hangoutCorpus.summary <- Corpus(DataframeSource(data.frame(hangout.summary[length(hangout.summary$text) != 0,]$text)))
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, stripWhitespace)
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, removePunctuation)
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, removeNumbers)
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, tolower)
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, removeWords, stopwords("english"))
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, PlainTextDocument)
hangoutCorpus.summaryStemmed <- tm_map(hangoutCorpus.summary, stemDocument,language="english")
hangoutCorpus.summaryStemmed <- tm_map(hangoutCorpus.summaryStemmed, PlainTextDocument)



## create document-term-matrix: data is now structured in the format where every document is a row
## and every term is a column.  the weighting argument decides what values are in the cells, tfidf is the most common

hangoutdtm.summaryStemmed <- DocumentTermMatrix(hangoutCorpus.summaryStemmed, control = list(minWordLength = 3,weighting=weightTfIdf))
findFreqTerms(hangoutdtm.summaryStemmed, lowfreq=.05)


## convert your dtm to a matrix 

hangout.summary.matrix <- as(as.matrix(hangoutdtm.summaryStemmed),"sparseMatrix")


## create wordclouds.  wordcloud package takes in two main arugments: a vector of words, and a vector of frequencies
## these frequences can be anything.  The first wordcloud sums up all tfidf values for each word.  
## Second wordcloud takes the max tfidf value.
## the png function will save your generated image to the specified file path

hangout.summary.dataframe <- data.frame(word = names(sort(colSums(hangout.summary.matrix),decreasing=TRUE)),
                                       freq=sort(colSums(hangout.summary.matrix),decreasing=TRUE))

#I do some cleaning to exclude some of the more boring words
png("plots/wordcloud_subset.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(hangout.summary.dataframe[!grepl("haha|like|yeah|you|yes|no|hiii",hangout.summary.dataframe$word) & 
                                     nchar(as.character(hangout.summary.dataframe$word)) < 15,]$word,
          hangout.summary.dataframe[!grepl("haha|like|yeah|you|yes|no|hiii",hangout.summary.dataframe$word) & 
                                     nchar(as.character(hangout.summary.dataframe$word)) < 15,]$freq,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 1, scale = c(4, 0.2))
dev.off()

hangout.summary.wordcloud <-
  data.frame(word = as.character(colnames(hangout.summary.matrix)),
             freq = apply(hangout.summary.matrix,2, max))
png("plots/wordcloud_subset_max.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(hangout.summary.wordcloud[!grepl("haha|like|yeah|you|yes|no|hiii",hangout.summary.wordcloud$word) & 
                                      nchar(as.character(hangout.summary.wordcloud$word)) < 15,]$word,
          hangout.summary.wordcloud[!grepl("haha|like|yeah|you|yes|no|hiii",hangout.summary.wordcloud$word) & 
                                      nchar(as.character(hangout.summary.wordcloud$word)) < 15,]$freq,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = .05, scale = c(4, 0.2))
dev.off()



## it would be interesting to see if we can predict which person sent the message based on just the words in the message
## I'm a big fan of the LASSO regression method:
## tl;dr LASSO helps regularize for overfitting.  Very useful for datasets where num columns >>>> num rows

hangout.full.matrix <- as(as.matrix(hangoutdtm.summaryStemmed),"sparseMatrix")

hangout.model <- cv.glmnet(hangout.full.matrix,
                           factor(hangout.summary$participant), family = "multinomial")


## extract the coefficients for each person and each word

betas <-
  adply(c(1:length(coef(hangout.model, s="lambda.min"))), 1, function(x) {
    
    data <-
      data.frame(cbind( word = as.vector(dimnames(coef(hangout.model, s="lambda.min")[[x]])[[1]]),
                        coeff = as.vector(coef(hangout.model, s="lambda.min")[[x]]),
                        participant = names(coef(hangout.model,s="lambda.min"))[x]))
    return(data)
  }, .progress = "text")

betas$coeff <- as.numeric(levels(betas$coeff))[betas$coeff]



## We can investigate some of highest weighted words for each person:

arrange(betas[betas$participant == "Dustin",],coeff)[(nrow(betas[betas$participant == "Dustin",])-100):nrow(betas[betas$participant == "Dustin Lao",]),]
arrange(betas[betas$participant == "Jason Jea",],coeff)[(nrow(betas[betas$participant == "Jason Jea",])-50):nrow(betas[betas$participant == "Jason Jea",]),]



## wordclouds based on the coefficients from the model, for each person:

png("plots/jason_wordcloud.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(as.character(betas[betas$coeff > 0 & betas$participant == "Jason Jea",]$word),
          betas[betas$coeff > 0& betas$participant == "Jason Jea",]$coeff,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 0)
dev.off()

png("plots/dustin_wordcloud.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(betas[betas$coeff > 0 & betas$participant == "Dustin",]$word,
          betas[betas$coeff > 0& betas$participant == "Dustin",]$coeff,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 0)
dev.off()

png("plots/brian_wordcloud.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(betas[betas$coeff > 0 & betas$participant == "Brian",betas$word,]$word,
          betas[betas$coeff > 0 & betas$participant == "Brian",betas$word,]$coeff,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 0)
dev.off()

png("plots/keith_wordcloud.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(betas[betas$coeff > 0 & betas$participant == "Keith",betas$word,]$word,
          betas[betas$coeff > 0 & betas$participant == "Keith",betas$word,]$coeff,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 0)
dev.off()



## we can also try to group our conversations by topic.  k-means is an unsupervised learning algorithm.
## this means we don't have to do any topic labeling, and it naturally try and cluster similar conversations.
## note: results might be better if we aggregated just at the date level, such that each day is a document rather than
## each day and participant.

kmeans.results <- kmeans(hangout.full.matrix, 10)   


## function that I stole from a Dr. Mosby at UT that displays the top words in each cluster.
## this can probably be rewritten to be more elegant.

for (i in 1:length(kmeans.results$withinss)) {    
  inGroup <- which(kmeans.results$cluster==i)     #For each cluster, this defines the documents in that cluster
  within <- hangout.full.matrix[inGroup,]            
  if(length(inGroup)==1) within <- t(as.matrix(within)) #This is a formatting correction when there is only one doc in a cluster
  out <- hangout.full.matrix[-inGroup,]              
  words <- apply(within,2,mean) - apply(out,2,mean) #Take the difference in means for each term
  print(c("Cluster", i), quote=F)
  labels <- order(words, decreasing=T)[1:10] #Take the top 20 Labels
  print(names(words)[labels], quote=F)     #From here down just labels
  if(i==length(kmeans.results$withinss)) { 
    print("Cluster Membership")
    print(table(kmeans.results$cluster))
    print("Within cluster sum of squares by cluster")
    print(kmeans.results$withinss)
  }
}

kmeans.results <- kmeans(as.matrix(hangout.full.matrix)[1:50,], 10) 
disse <- daisy(as.matrix(hangout.full.matrix)[1:50,])^2
plot(silhouette(kmeans.results$cl,disse))

## other things we can do:

## sentiment analysis- download a lexicon of sentiment (basically a dictionary that maps words to sentiment), map to corpus
## naive bayes is a popular classification algorithm that we can use to try and guess at sentiment

## time series analysis: predict word count by date and time