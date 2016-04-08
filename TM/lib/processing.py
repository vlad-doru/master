import pandas
import numpy as np
import nltk

from nltk.tokenize.casual import TweetTokenizer
from nltk.sentiment.util import mark_negation 
from nltk.corpus import stopwords

def process_data(df, sample_size = 1.0, split_size = 0.9):
    df = df.head(n = int(len(df) * sample_size))
    mask = np.random.rand(len(df)) < split_size
    training_data = tokenize_tweets(df[mask])
    testing_data = tokenize_tweets(df[~mask])
    # Log info
    print("Using a total of {0} tweets".format(len(df)))
    print("Training sample size: {0}".format(len(training_data)))
    print("Testing sample size: {0}".format(len(testing_data)))
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
