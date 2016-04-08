import pandas
import numpy as np
import logging as log
import sys
import nltk
import pickle

from sklearn.svm import LinearSVC
from sklearn.neighbors import KNeighborsClassifier

from nltk.classify import NaiveBayesClassifier, DecisionTreeClassifier, SklearnClassifier
from nltk.tokenize.casual import TweetTokenizer
from nltk.sentiment import SentimentAnalyzer
from nltk.sentiment.util import mark_negation, extract_unigram_feats
from nltk.corpus import stopwords

def setup_logging():
    root = log.getLogger()
    root.setLevel(log.DEBUG)

    ch = log.StreamHandler(sys.stdout)
    ch.setLevel(log.DEBUG)
    formatter = log.Formatter('%(asctime)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    root.addHandler(ch)

def process_data(df):
    USED_SAMPLE_SIZE = 0.5
    CROSS_VALIDATION_SIZE = 0.9
    df = df.head(n = int(len(df) * USED_SAMPLE_SIZE))
    mask = np.random.rand(len(df)) < CROSS_VALIDATION_SIZE
    training_data = tokenize_tweets(df[mask])
    testing_data = tokenize_tweets(df[~mask])
    # Log info
    log.info("Using a total of {0} tweets".format(len(df)))
    log.info("Training sample size: {0}".format(len(training_data)))
    log.info("Testing sample size: {0}".format(len(testing_data)))
    return training_data, testing_data

def process_tweet(tweet):
    # Use a tweet tokenizer form the nltk package.
    tokenizer = TweetTokenizer()
    tokens = tokenizer.tokenize(tweet)
    filtered = [w for w in tokens if w not in stopwords.words('english')]
    return mark_negation(filtered)

def tokenize_tweets(data):
    # Get the lowercase of the text.
    extracted = list(zip(data['text'].str.lower(), data['airline_sentiment']))
    # Use mark negation to capture better sentiment.
    tokenized = [(process_tweet(text), label) for text, label in extracted] 
    return tokenized

def add_features(train_data, sentim_analyzer):
    all_words = sentim_analyzer.all_words(train_data, labeled=True)
    # TODO: Change the min_freq 
    unigram_feats = sentim_analyzer.unigram_word_feats(all_words, min_freq=10)
    log.info("Number of features: {0}".format(len(unigram_feats)))
    sentim_analyzer.add_feat_extractor(extract_unigram_feats, unigrams=unigram_feats)

def extract_features(training_data, testing_data, sentim_analyzer):
    return sentim_analyzer.apply_features(training_data), sentim_analyzer.apply_features(testing_data)

def train_models(training_data, testing_data, sentim_analyzer, trainers):
    models = []
    for trainer in trainers:
        log.info("Training: {0}".format(trainer["name"]))
        classifier = sentim_analyzer.train(trainer["train"], training_data)
        # Evaluate the test set. 
        evaluation = sentim_analyzer.evaluate(testing_data)
        # Add to the models list.
        models.append((classifier, evaluation, trainer))
        # Save the classifier to a file.
        model_file="./models/{}.pickle".format(trainer["model_file"])
        log.info("Saving the model to file: {0}".format(model_file))
        f = open(model_file, "wb")
        pickle.dump({
            "classifier": classifier,
            "evaluation": evaluation,
            "trainer": trainer,
        }, f, -1)
    return models

def main():
    setup_logging()
    data_file = './airline-twitter-sentiment/Tweets.csv'
    df = pandas.read_csv(data_file)
    log.info("Read the data file from {0}".format(data_file))
    # Pre-proces tweets: tokenize, etc.
    training_data, testing_data = process_data(df)
    # Create a sentiment analyzer.
    sentim_analyzer = SentimentAnalyzer()
    # Add the feature extractor to the sentiment analyzer.
    add_features(training_data, sentim_analyzer)
    # Extract features from training and testing data.
    train, test = extract_features(training_data, testing_data, sentim_analyzer)
    # Select the type of classifier we are going to use.
    trainers = [
        {"name": "Naive Bayes Classifier",
        "model_file": "naive_bayes",
        "train": NaiveBayesClassifier.train,
        },
        {"name": "Linear SVC Classifier",
        "model_file": "linear_svc",
        "train": SklearnClassifier(LinearSVC(
            dual = False, # because number of samples > number of features
            )).train,},
        {"name": "K Nearest Neighbours Classifier",
        "model_file": "k_neighbours",
        "train": SklearnClassifier(KNeighborsClassifier(
            )).train,}
    ]
    # Train all enumerated models
    models = train_models(train, test, sentim_analyzer, trainers)
    for classifier, evaluation, trainer in models:
        print("EVALUATION for {0}".format(trainer['name']))
        for key,value in sorted(evaluation.items()):
            print('\t{0}: {1}'.format(key, value))
    

if __name__ == '__main__':
    main()
