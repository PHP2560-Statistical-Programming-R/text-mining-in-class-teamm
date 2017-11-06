clean_data <- function() {
  
  load("data/nyt_data.rda")
  
  NYTimes$Title = as.character(NYTimes$Title)
  words = unnest_tokens(NYTimes, word, Title,to_lower=FALSE)
  
  save(words,file="data/nyt_data.rda")
}

clean_data()