load("data/nyt_data.rda")

############## PART 1 ##############

####### SENTIMENTS #######

# Get dictionary with sentiment scores using lexicon afinn 
sentiment.scores = as.data.frame(get_sentiments("afinn"))

# Prepare the titles as strings by converting title to characters
NYTimes$title = (as.character(NYTimes$Title))

# Create a data frame that will contain the sentiment scores for each word in the title
nyt.sent = NYTimes %>%
  # group the data by title
  group_by(Title) %>%
  # get each word from the title out by unnesting
  unnest_tokens(output=word, input=title) %>%
  # get the sentiment scores for each word in the title 
  inner_join(sentiment.scores) %>%
  # get a mean of the score for each word in the title to get an average sentiment score for each title 
  summarize(title.score = mean(score))

# SAVE ALL THIS
save(nyt.sent, file="data/nyt_sentimentScores")

####### INTENSITY #######

load("data/nyt_sentimentScores")

# Get absolute value
nyt.sent = nyt.sent %>%
  mutate(intensity = abs(title.score))

# Get mean intensity

nyt.intensity = nyt.sent %>%
  group_by(Title) %>%
  summarize(mean.intensity = mean(intensity))

# SAVE ALL THIS
save(nyt.intensity, file = "data/nyt_intensityScores")

############## PART 2 ##############

####### PREPARE DATE/YEARS #######

#1. Convert Dates to Strings

# make the date as characters
NYTimes$Date = as.character(NYTimes$Date)

# now convert the date to strings
for (i in 1:length(NYTimes$Date)){
  NYTimes$Date[i] = toString(NYTimes$Date[i])
}

#2. Extract the Year
# create a pattern for how the year will look like
year_pattern = DGT %R% DGT %R% END
# get the year using this pattern and put this year in our data frame with scores
NYTimes$year = str_extract(NYTimes$Date, pattern=year_pattern)

####### SENTIMENT #######

# Get sentiment scores for titles
year.sent = NYTimes %>%
  # group the data by title
  group_by(Title, year) %>%
  # get each word from the title out by unnesting
  unnest_tokens(output=word, input=title) %>%
  # get the sentiment scores for each word in the title 
  inner_join(sentiment.scores) %>%
  # get a mean of the score for each word in the title to get an average sentiment score for each title 
  summarize(title.score = mean(score))

# Now, get mean sentiment scores for years
year.sent = year.sent %>%
  group_by(year) %>%
  summarize(year.sent = mean(title.score))

####### INTENSITY #######

# create dataframe containing the intensity
year.intensity = year.sent %>%
  mutate(intensity = abs(year.sent))

####### CLEAN YEARS #######

# change years to numeric now, first in the year.sent dataframe
year.sent$year = as.numeric(year.sent$year)

# and make it go from year 1 to year 10 (with 1996 as year 1)
year.sent = year.sent %>%
  mutate(year.num = ifelse(year.sent$year<=6, 10-year.sent$year, abs(96-year.sent$year)))

# now also save these year numbers into year.intensity dataframe
year.intensity$year.num = year.sent$year.num

####### SAVE ALL THE DATA #######

save(year.sent, file = "data/nyt_yearSentiment")
save(year.intensity, file = "data/nyt_yearIntensity")