suppressMessages(suppressWarnings(library(RTextTools)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(caTools)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(rebus)))
suppressMessages(suppressWarnings(library(foreign)))
suppressMessages(suppressWarnings(library(nnet)))
suppressMessages(suppressWarnings(library(magrittr)))
set.seed(23)

load("data/nyt_data.rda")
load("data/nyt_words.rda")

#clean/match
nrc.sents = inner_join(words, get_sentiments("nrc"), by="word")
bing.sents = inner_join(words, get_sentiments("bing"), by="word")


bing.words <- bing.sents %>% select(word)
nrc.words <- nrc.sents %>% filter(sentiment %in% c("positive", "negative")) %>% select(word)
#The above procedure is done to ensure that each token is counted only once. As described below, 
#NRC gives a positive/negative value exactly once for each word.
num.bing=dim(bing.words)[1] #number of bing matches
num.nrc= dim(nrc.words)[1] #number of nrc matches

nrc.lengths <- apply(nrc.words, 1, nchar) #length of each word
bing.lengths <- apply(nrc.words, 1, nchar) #length of each word
df <- data.frame(length=c(nrc.lengths, bing.lengths), dict = c(rep("NRC", length(nrc.words)), rep("Bing", length(bing.words))))
ggplot(df, aes(x=factor(dict),y=length,fill=factor(dict)))+
  geom_boxplot() + labs(title="Word Lengths by Dictionary") +facet_wrap(~dict)
dev.copy(png, 'graph/brianp1.png')
dev.off()

nrc.binary <- filter(nrc.sents, sentiment %in% c("positive", "negative")) #just looking at positive/negative
compare <- inner_join(bing.sents, nrc.binary, 
                      by=c("word", "Article_ID"), suffix = c(".bing", ".nrc")) #words that match both
agreement = mean(compare$sentiment.bing==compare$sentiment.nrc)


  
#logistic model for topic
  
  #get most frequent words
  most.freq <- words$word %>%
    table()%>%
    as_tibble() %>%
    arrange(desc(n)) %>%
    slice(1:30)
  model.words <- most.freq$.
  
  #create feature that counts word appearances
  model.data = NYTimes
  for(i in 1:length(model.words)){
    feature.word = model.words[i]
    for(j in 1:dim(NYTimes)[1]){
      model.data[[feature.word]][j]=str_count(NYTimes$Title[j], 
                                              (START%|%SPC) %R% feature.word %R% (END%|%SPC))
    }
  }  

  #train classifier  
  model <- multinom(as.factor(Topic.Code) ~ . - Article_ID - Date - Title - Subject, data = model.data)
  
  #training set accuracy
  accuracy=mean(predict(model, newdata = model.data)==model.data$Topic.Code)