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

sample_size <- .10

trainIndex <- createDataPartition(tweets$sentiment,p=sample_size,list=FALSE)
trainData <- features_matrix[trainIndex,]

trainData <- features_matrix[trainIndex,]
testData <- features_matrix[-trainIndex,]
trainLabels <- tweets[trainIndex, "sentiment"]
testLabels <- tweets[-trainIndex, "sentiment"]

sprintf("Using %d examples for our training data.", nrow(trainData))

# Train and Tune the SVM, performing Cross Validation.
svm <- train(trainData,        # train features
             trainLabels,      # train labels
             method="svmLinear",          # use a linear kernel since the number of features is big       
             tuneGrid=data.frame(C = 1),  # tune the parameter C
             trControl=trainControl(
               verbose = TRUE,            
               method='cv',               # use Cross-Validation
               number=10                  # 10 folds
             ))
print(svm)
#p <- predict(svm, testData)
#summary(p)
#mean(p == testLabels)
