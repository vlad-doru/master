import pandas
import numpy as np
import logging as log
import sys

def load_data():
    data_file = './airline-twitter-sentiment/Tweets.csv'
    df = pandas.read_csv(data_file)
    print("Read the data file from {0}".format(data_file))
    return df
