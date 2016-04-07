import pandas
import numpy as np
import logging as log
import sys
import nltk

from nltk.classify import NaiveBayesClassifier, DecisionTreeClassifier
from nltk.tokenize.casual import TweetTokenizer
from nltk.sentiment import SentimentAnalyzer
from nltk.sentiment.util import mark_negation, extract_unigram_feats
from nltk.corpus import stopwords

TRAIN_SAMPLE_SIZE = 0.9

def setup_logging():
    root = log.getLogger()
    root.setLevel(log.DEBUG)

    ch = log.StreamHandler(sys.stdout)
    ch.setLevel(log.DEBUG)
    formatter = log.Formatter('%(asctime)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    root.addHandler(ch)

def validation_split(df):
    # Remove the neutral tweets.
    # REMOVE THIS AFTER FINAL SCRIPT
    df = df.head(n = 500)
    mask = np.random.rand(len(df)) < TRAIN_SAMPLE_SIZE
    return df[mask], df[~mask]

def process_tweet(tweet):
    # Use a tweet tokenizer form the nltk package.
    tokenizer = TweetTokenizer()
    tokens = tokenizer.tokenize(tweet)
    filtered = [w for w in tokens if w not in stopwords.words('english')]
    return mark_negation(filtered)

def process_data(data):
    # Get the lowercase of the text.
    extracted = list(zip(data['text'].str.lower(), data['airline_sentiment']))
    # Use mark negation to capture better sentiment.
    tokenized = [(process_tweet(text), label) for text, label in extracted] 
    return tokenized


def main():
    setup_logging()
    data_file = './airline-twitter-sentiment/Tweets.csv'
    df = pandas.read_csv(data_file)
    log.info("Read the data file from {0}".format(data_file))
    train_data, test_data = validation_split(df)
    log.info("Using a total of {0} tweets".format(len(df)))
    log.info("Training sample size: {0}".format(len(train_data)))
    log.info("Testing sample size: {0}".format(len(test_data)))
    train = process_data(train_data)
    test = process_data(test_data)
    sentim_analyzer = SentimentAnalyzer()
    all_words = sentim_analyzer.all_words(train, labeled=True)
    # TODO: Change the min_freq 
    unigram_feats = sentim_analyzer.unigram_word_feats(all_words, min_freq=10)
    sentim_analyzer.add_feat_extractor(extract_unigram_feats, unigrams=unigram_feats)
    training_set = sentim_analyzer.apply_features(train)
    test_set = sentim_analyzer.apply_features(test)
    trainer = NaiveBayesClassifier.train
    trainer = DecisionTreeClassifier.train
    # TODO generalize this to use multiple ML techniques.
    # TODO try various parameters and plot results.
    # Save the models in files.
    classifier = sentim_analyzer.train(trainer, training_set, save_classifier="./models/decision_tree.model", 
            verbose = True)
    # print(classifier.show_most_informative_features(10))
    for key,value in sorted(sentim_analyzer.evaluate(test_set).items()):
        print('{0}: {1}'.format(key, value))
    

if __name__ == '__main__':
    main()
