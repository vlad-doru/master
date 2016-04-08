import pandas
import numpy as np
import logging as log
import sys

def setup_logging():
    root = log.getLogger()
    root.setLevel(log.DEBUG)

    ch = log.StreamHandler(sys.stdout)
    ch.setLevel(log.DEBUG)
    formatter = log.Formatter('%(asctime)s - %(levelname)s - %(message)s')
    ch.setFormatter(formatter)
    root.addHandler(ch)

def load_data():
    data_file = './airline-twitter-sentiment/Tweets.csv'
    df = pandas.read_csv(data_file)
    log.info("Read the data file from {0}".format(data_file))
    return df
