# packages
require("plyr") # compute counts in aggregates
require("nnet") # multinomial models

# constants
inputTestData <- "test.tsv"
inputTrainingData <- "train.tsv"
outputTestData <- "test_output.csv"
outputTrainingData <- "train_output.csv"
emptySentiment <- -1  # set in the final output stage for Sentiment = NA

##############################################################################
# Load data - combine test and training data, set Sentiment = NA for test set,
#             set PredictedSentiment to NA for test and training set.
##############################################################################

loadData <- function(testDataPath, trainingDataPath)
{
  # parse test set
  test <- read.table(testDataPath,
                     header = TRUE,
                     sep = "\t",
                     comment.char = "",
                     quote = "",
                     colClasses = c("integer","integer","character"))
     
  # parse training set
  train <- read.table(trainingDataPath,
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

dataset = loadData(inputTestData, inputTrainingData)

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
# Train model
#####################

model = multinom(Sentiment ~ NumCharactersInPhrase + NumWordsInPhrase + MaxWordLengthInPhrase,
                 data = train,
                 na.rm = TRUE)  # only use training set to train model

#####################
# Predict
#####################

dataset$Predicted_Sentiment = predict(model, dataset)

#####################
# Validate
#####################

accuracy = round(mean(dataset$Sentiment == dataset$Predicted_Sentiment, na.rm = TRUE) * 100,2)

#####################
# Output
#####################

test = dataset[is.na(dataset$Sentiment),]
train = dataset[!is.na(dataset$Sentiment),]

# set Sentiment = NA to numeric value so easier to import to other tools
test$Sentiment = emptySentiment

# write csv
write.csv(test, file = outputTestData)
write.csv(train, file = outputTrainingData)

write.csv(dataset[!is.na(dataset$Sentiment),], file = outputTrainingData )
