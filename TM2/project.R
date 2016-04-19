### READING
# Read data from the .csv file.
data <- read.csv("./input/sentiment.csv")
# Remove neutral tweets from the dataset and keep only sentiment and text columns
tweets <- data[data["sentiment"] != "Neutral",c("sentiment", "text")]
tweets[,"sentiment"] <- factor(tweets[,"sentiment"])
# Log how many tweets we have loaded.
sprintf("Number of available tweets is %d.", nrow(tweets))

### LIBRARIES
library(tm) 
library(RTextTools)
library(caret)
library(kernlab)

### PREPROCESSING
# Make all words lowercase in all of the tweets.
tweets[,"text"] <- sapply(tweets[,"text"], tolower)
# Remove all the URLs as they are not relevant.
tweets[, "text"] <- sapply(tweets[,"text"], function(x) gsub("http\\S+\\s*", "", x))
# Remove all the @ mentions.
tweets[, "text"] <- sapply(tweets[,"text"], function(x) gsub("@\\S+\\s*", "", x))
# Remove the # character so that we have better matching. 
tweets[, "text"] <- sapply(tweets[,"text"], function(x) gsub("#", "", x))
# Remove the " character.
tweets[, "text"] <- sapply(tweets[,"text"], function(x) gsub("\"", "", x))

# Construct the corpus of our tweets.
tweets_corpus <- Corpus(VectorSource(tweets[,"text"]))
# Construct the document term matrix.
dt_matrix <- DocumentTermMatrix(tweets_corpus,
                        control=list(
                          wordLengths = c(1, Inf),            # we allow any length for words
                          bounds = list(global = c(5, Inf)),  # at least 5 documents
                          stemming = TRUE,                    # stem the words
                          removePunctuation = TRUE,           # remove the punctuation
                          weighting = weightTf                # weight with TF
                        ))
features_matrix <- as.matrix(dt_matrix)
sprintf("Using %d features.", ncol(features_matrix))
### ML Part

SIZE <- .10 # TODO: Change this to .90

trainIndex <- createDataPartition(tweets$sentiment,p=SIZE,list=FALSE)
# Create training and testing data.
trainData <- features_matrix[trainIndex,]
testData <- features_matrix[-trainIndex,]
trainLabels <- tweets[trainIndex, "sentiment"]
testLabels <- tweets[-trainIndex, "sentiment"]

sprintf("Using %d examples for our training data.", nrow(trainData))

trControl <- trainControl(
  verbose = TRUE, # display training information
  method = 'cv',  # use cross validation
  number = 10     # use k-folds cross validation with k = 10
)

### SVM - LINEAR SVM
# Train and Tune the SVM, performing Cross Validation.
tuneGrid <- data.frame(
  C = 2 ^ seq(-6, 4, 1)
)
svm <- train(trainData, 
             trainLabels, 
             method="svmLinear", # linear kernel SVM   
             tuneGrid=tuneGrid,
             trControl=trControl)
print(svm)
#p <- predict(svm, testData)
#summary(p)
#mean(p == testLabels)
