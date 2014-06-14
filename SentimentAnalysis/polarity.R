# packages
require("qdap") # has a sentiment analysis
require("tm")

# inputs
PathTestData = "test.tsv";
PathTrainingData = "train.tsv";
emptySentiment = -1;  # value to indicate empty sentiment value in test set

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
  
  # hello world
  
  # parse training set
  train <- read.table(PathTrainingData,
                      header = TRUE,
                      sep = "\t",
                      comment.char = "",
                      quote = "",
                      colClasses = c("integer","integer","character","integer"))
  
  
  # combine test and train set into data
  Sentiment = rep(as.integer(emptySentiment), nrow(test))
  test <- cbind(test, Sentiment)
  data <- rbind(train, test)
  Predicted_Sentiment <- rep(as.integer(emptySentiment), nrow(data))
  data <- cbind(data, Predicted_Sentiment)
}

dataset = loadData(PathTestData, PathTrainingData)

#####################
# Calculate features
#####################

n_data = nrow(dataset)
# Determine polarity of each phrase as determined by qdap
polarity.avg <- rep(0, n_data)
# Add sentiment of each word
for (i in 1:n_data) {
  if (i%%100==0)
  {
    print(i)
  }
  p <- polarity(removePunctuation(dataset$Phrase[i]))
  p.df <- data.frame(p)
  ave_polarity <- p.df$group.ave.polarity
  if (is.nan(ave_polarity))
  {
    ave_polarity <- 0
  }
  polarity.avg[i] <- ave_polarity 
}

dataset_out = data.frame(cbind(dataset$PhraseId, polarity.avg))
colnames(dataset_out) <- c("PhraseId", "polarity.avg")
write.csv(dataset_out, file = "train_test_polarity.csv", row.names=FALSE)
