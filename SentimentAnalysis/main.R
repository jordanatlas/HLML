# required packages 
require("plyr") # package to compute counts in aggregates
require("nnet") # multinomial models package

# constants
inputTestData <- "test.tsv"
inputTrainingData <- "train.tsv"
outputTestData <- "test_output.csv"
outputTestDataKaggle <- "test_output_kaggle.csv"
outputTestDataWeka <- "test_output_weka.csv"
outputTrainingData <- "train_output.csv"
outputTrainDataWeka <- "train_output_weka.csv"
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

##############################################################################
# Load features - Load features from external sources for cases where feature
# calculation is expensive. Combine these features with our dataset.
# NOTE: Currently assuming that feature data has all rows from train and test in
# same order as train and test.  THis might not be safe for all new features 
# going forward
##############################################################################

polarityFeatureData <- "train_test_polarity.csv"
polarity <- read.csv(polarityFeatureData)
dataset <- cbind(dataset, polarity$polarity.avg)


#####################
# Calculate features
#####################

# Determine number of characters in the phrase
NumCharactersInPhrase = sapply(dataset$Phrase, nchar)
dataset = cbind(dataset, NumCharactersInPhrase)

# Determine number of words in the phrase
NumWordsInPhrase = sapply(strsplit(dataset$Phrase, " "), length)
dataset = cbind(dataset, NumWordsInPhrase)

# Determine max word length in the phrase
MaxWordLengthInPhrase = sapply(lapply(strsplit(dataset$Phrase, " "), nchar), max)
dataset = cbind(dataset, MaxWordLengthInPhrase)

# Determine which phrases have a "!"
ContainsExclamation = sapply(dataset$Phrase, grepl, "!")
dataset = cbind(dataset, ContainsExclamation)

# Determine which phrases have a "#"
ContainsPound = sapply(dataset$Phrase, grepl, "#")
dataset = cbind(dataset, ContainsPound)


#####################
# Data summary
#####################
summary(dataset)

#####################
# Train model
#####################

model = multinom(Sentiment ~ NumCharactersInPhrase + NumWordsInPhrase + MaxWordLengthInPhrase + polarity$polarity.avg
                 + ContainsExclamation + ContainsPound,
                 data = dataset,
                 na.rm = TRUE)  # only use training set to train model

#####################
# Predict
#####################

prediction = predict(model, dataset)
dataset$Predicted_Sentiment = as.integer(as.character(prediction))

#####################
# Validate
#####################

accuracy = round(mean(dataset$Sentiment == dataset$Predicted_Sentiment, na.rm = TRUE) * 100,2)

#####################
# Output
#####################

# Reorder dataset so that Sentiment is last
numcol <- ncol(dataset)
dataset <- dataset[,c(1,2,3,6:numcol,5,4)]

test = dataset[is.na(dataset$Sentiment),]
train = dataset[!is.na(dataset$Sentiment),]

# set Sentiment = emptySentiment to numeric value so easier to import to other tools
test$Sentiment = emptySentiment

# write generic csv
write.csv(test, file = outputTestData, row.names = FALSE)
write.csv(train, file = outputTrainingData, row.names = FALSE)

# write kaggle test output csv
test_kaggle <- test[, c('PhraseId', 'Predicted_Sentiment')]
colnames(test_kaggle) <- c("PhraseId", "Sentiment")
write.csv(test_kaggle, file = outputTestDataKaggle, row.names = FALSE)

# write weka friendly output
test_weka = subset(test, select=-c(PhraseId,SentenceId,Phrase,Predicted_Sentiment))
test_weka$Sentiment = '?'
train_weka = subset(train, select=-c(PhraseId,SentenceId,Phrase,Predicted_Sentiment))
write.csv(test_weka, file = outputTestDataWeka, row.names = FALSE)
write.csv(train_weka, file = outputTrainDataWeka, row.names = FALSE)