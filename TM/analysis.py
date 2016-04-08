import pandas
import numpy as np
import logging as log
import sys
import nltk
import pickle

data_file = './airline-twitter-sentiment/Tweets.csv'
df = pandas.read_csv(data_file)
