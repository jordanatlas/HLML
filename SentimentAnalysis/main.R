# packages
library("plyr") # compute counts in aggregates

# inputs
PathTestData = "test.tsv";
PathTrainingData = "train.tsv";

#####################
# Load data
#####################

loadData <- function(testDataPath, trainingDataPath)
{
  # parse test set
  test <- read.table(PathTestData,
                     header = TRUE,
                     sep = "\t",
                     comment.char = "",
                     quote = "",
                     colClasses = c("integer","integer","character"))
  
  # parse training set
  train <- read.table(PathTrainingData,
                      header = TRUE,
                      sep = "\t",
                      comment.char = "",
                      quote = "",
                      colClasses = c("integer","integer","character","integer"))
  
  
  # combine test and train set into data
  Sentiment = rep(as.integer(NA), nrow(test))
  test <- cbind(test, Sentiment)
  data <- rbind(train, test)
  Predicted_Sentiment <- rep(as.integer(NA), nrow(data))
  data <- cbind(data, Predicted_Sentiment)
}

if (!exists("dataset"))
{
  dataset = loadData(PathTestData, PathTrainingData)
}

#####################
# Calculate features
#####################

# Determine number of characters in the phrase
NumCharactersInPhrase = unlist(lapply(dataset$Phrase, nchar))
dataset = cbind(dataset, NumCharactersInPhrase)

# Determine number of words in the phrase
NumWordsInPhrase = unlist(lapply(strsplit(dataset$Phrase, " "), length))
dataset = cbind(dataset, NumWordsInPhrase)

# Determine max word length in the phrase
MaxWordLengthInPhrase = unlist(lapply(lapply(strsplit(dataset$Phrase, " "), nchar), max))
dataset = cbind(dataset, MaxWordLengthInPhrase)

#####################
# Predict sentiment
#####################

dataset$Predicted_Sentiment = 2; # always predict neutral

#####################
# Validate
#####################

accuracy = round(mean(dataset$Sentiment == dataset$Predicted_Sentiment, na.rm = TRUE) * 100,2)