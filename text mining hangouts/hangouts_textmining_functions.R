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

hangouts.json <- fromJSON(txt = "C:/Users/jjea/Documents/Personal/casey gift/Takeout/Hangouts/Hangouts.json", flatten = TRUE)


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

participant.data <- 
  unique(ldply(hangouts.json[[2]]$conversation_state.conversation.participant_data, function(z) {
    
    data.frame(z[,c("fallback_name","id.gaia_id")])
    
  }))

participant.data <- participant.data %>% group_by(id.gaia_id) %>% summarise(fallback_name = max(fallback_name))
  
hangouts.labels <-
  laply(hangouts.json[[2]]$conversation_state.conversation.participant_data, function(a) {
    
    paste(a$fallback_name,collapse="_",sep="")
    
  })

names(hangouts.texts.list) <- hangouts.labels

#19 is Casey, 16 is gchat bros
hangout.data <- hangouts.texts.list[[16]]


hangout.data$timestamp = as.POSIXct(as.numeric(levels(hangout.data$timestamp))[hangout.data$timestamp]/1000000, origin = "1970-01-01")
hangout.data$wordcount = laply(gregexpr(" ",hangout.data$text), function(x) length(x))
hangout.data$date = as.Date(hangout.data$timestamp)
hangout.data$hour = hour(hangout.data$timestamp)
hangout.data$minute = minute(hangout.data$timestamp)
hangout.data$gaiai_id <- as.character(hangout.data$gaiai_id)

hangout.data <- merge(hangout.data,participant.data, by.x = "gaiai_id", by.y = "id.gaia_id",all.x = TRUE)

hangout.data$text <- as.character(hangout.data$text)
hangout.data$text <- iconv(hangout.data$text, "latin1", "ASCII", sub="")

hangout.data$participant <- ifelse(is.na(hangout.data$fallback_name), "Jason Jea", hangout.data$fallback_name)



hangout.data %>% filter(date <= as.Date("2015-12-25"), date >= as.Date("2015-06-01")) %>%
  group_by(date,participant) %>% summarise(wordcount = sum(wordcount)) %>% ungroup %>%
  group_by(participant) %>%
  mutate(smooth = rollmean(wordcount,7,na.pad=TRUE,align="left")) %>% ungroup %>%
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


hangout.summary <-
  hangout.data[,c("date","participant","text","wordcount")] %>%
  group_by(date,participant) %>%
  summarise(text =paste(text, collapse = " "), wordcount = sum(wordcount)) %>%
  group_by(date) %>% mutate(daycount = sum(wordcount)) %>% ungroup %>%
  filter(wordcount >= 300)
hangout.summary$document <- paste(hangout.summary$date,hangout.summary$participant, sep = " ")


hangoutCorpus.summary <- Corpus(DataframeSource(data.frame(hangout.summary[length(hangout.summary$text) != 0,]$text)))
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, stripWhitespace)
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, removePunctuation)
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, removeNumbers)
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, tolower)
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, removeWords, stopwords("english"))
hangoutCorpus.summary <- tm_map(hangoutCorpus.summary, PlainTextDocument)

hangoutdtm.summary <- DocumentTermMatrix(hangoutCorpus.summary, control = list(minWordLength = 3,weighting=weightTfIdf))
findFreqTerms(hangoutdtm.summary, lowfreq=1)

hangoutCorpus.summaryStemmed <- tm_map(hangoutCorpus.summary, stemDocument,language="english")
hangoutCorpus.summaryStemmed <- tm_map(hangoutCorpus.summaryStemmed, PlainTextDocument)
hangoutdtm.summaryStemmed <- DocumentTermMatrix(hangoutCorpus.summaryStemmed, control = list(minWordLength = 3,weighting=weightTfIdf))
findFreqTerms(hangoutdtm.summaryStemmed, lowfreq=.05)

hangout.summary.matrix <- as.matrix(hangoutdtm.summaryStemmed)
hangout.summary.dataframe <- data.frame(word = names(sort(colSums(hangout.summary.matrix),decreasing=TRUE)),
                                       freq=sort(colSums(hangout.summary.matrix),decreasing=TRUE))

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


hangout.full.matrix <- as(as.matrix(hangoutdtm.summaryStemmed),"sparseMatrix")

hangout.model <- cv.glmnet(hangout.full.matrix[,!grepl("lol|cunt|jasonzjea|lmao|rofl|hah|http|ooo|csgo",
                                                       colnames(hangout.full.matrix))],
                           factor(hangout.summary$participant), family = "multinomial")

betas <-
  adply(c(1:length(coef(hangout.model, s="lambda.min"))), 1, function(x) {
    
    data <-
      data.frame(cbind( word = as.vector(dimnames(coef(hangout.model, s="lambda.min")[[x]])[[1]]),
                        coeff = as.vector(coef(hangout.model, s="lambda.min")[[x]]),
                        participant = names(coef(hangout.model,s="lambda.min"))[x]))
    return(data)
  }, .progress = "text")

betas$coeff <- as.numeric(levels(betas$coeff))[betas$coeff]
arrange(betas[betas$participant == "Dustin Lao",],coeff)[(nrow(betas[betas$participant == "Dustin Lao",])-100):nrow(betas[betas$participant == "Dustin Lao",]),]
arrange(betas[betas$participant == "Jason Jea",],coeff)[(nrow(betas[betas$participant == "Jason Jea",])-50):nrow(betas[betas$participant == "Jason Jea",]),]

png("plots/jason_wordcloud.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(as.character(betas[betas$coeff > 0 & betas$participant == "Jason Jea" & !grepl("lol|cunt|jasonzjea|lmao|rofl|hah|http|stephen",as.character(betas$word)),]$word),
          betas[betas$coeff > 0& betas$participant == "Jason Jea" & !grepl("lol|cunt|jasonzjea|lmao|rofl|hah|http|stephen",betas$word),]$coeff,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 0)
dev.off()

png("plots/dustin_wordcloud.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(betas[betas$coeff > 0 & betas$participant == "Dustin Lao" & !grepl("lol|nicci|http",betas$word),]$word,
          betas[betas$coeff > 0& betas$participant == "Dustin Lao" & !grepl("lol|nicci|http",betas$word),]$coeff,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 0)
dev.off()

png("plots/brian_wordcloud.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(betas[betas$coeff > 0 & betas$participant == "Brian Chang" & !grepl("lol|nicci|http|tiff",betas$word),]$word,
          betas[betas$coeff > 0 & betas$participant == "Brian Chang" & !grepl("lol|nicci|http|tiff",betas$word),]$coeff,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 0)
dev.off()

png("plots/keith_wordcloud.png", width=12,height=8, units='in', res=300, type= "cairo")
wordcloud(betas[betas$coeff > 0 & betas$participant == "Keith Chan" & !grepl("lol|nicci|http|tiff",betas$word),]$word,
          betas[betas$coeff > 0 & betas$participant == "Keith Chan" & !grepl("lol|nicci|http|tiff",betas$word),]$coeff,
          colors=brewer.pal(8,"Dark2"),random.order=FALSE, min.freq = 0)
dev.off()


kmeans.results <- kmeans(hangout.full.matrix,                  
                         centers=10,
                         iter.max=50)   

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

distances.data <- data.frame(as.matrix(dist(hangout.full.matrix))) # euclidean distances between the rows
d<- d[!!rowSums(abs(d[-c(1:2)])),]
d<-as.matrix(d)
rownames(d)<-rownames(chat.m.edit)
fit <- cmdscale(d = d, eig=TRUE, k=15) # k is the number of dim
fit 