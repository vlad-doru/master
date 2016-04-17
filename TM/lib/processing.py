import pandas
import numpy as np
import nltk

from nltk.tokenize.casual import TweetTokenizer
from nltk.sentiment.util import mark_negation 
from nltk.corpus import stopwords

def process_data(df, negation = True):
    data = tokenize_tweets(df, negation)
    # Log info
    print("Using a total of {0} tweets".format(len(df)))
    return data

def tokenize_tweets(data, negation):
    # Get the lowercase of the text.
    extracted = list(zip(data['text'].str.lower(), data['airline_sentiment']))
    # Use mark negation to capture better sentiment.
    tokenized = [(process_tweet(text, negation), label) for text, label in extracted] 
    return tokenized

def process_tweet(tweet, negation):
    # Use a tweet tokenizer form the nltk package.
    tokenizer = TweetTokenizer()
    tokens = tokenizer.tokenize(tweet)
    if negation:
        return mark_negation(tokens)
    else:
        return tokens

