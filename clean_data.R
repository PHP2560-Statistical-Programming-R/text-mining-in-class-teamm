clean_data <- function() {
  
  load("data/nyt_data.rda")
  
  NYTimes$Title = as.character(NYTimes$Title)
  words_caps = unnest_tokens(NYTimes, word, Title,to_lower=FALSE)
  words = words = unnest_tokens(NYTimes, word, Title)
  
  save(words_caps,file="data/nyt_data.rda")
  save(words,file="data/nyt_data.rda")
}

clean_data()