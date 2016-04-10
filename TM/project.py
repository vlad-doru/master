import pandas
import numpy as np
import nltk

from lib import loading
from lib import processing
from lib import classify

from sklearn.svm import LinearSVC
from sklearn.neighbors import KNeighborsClassifier

from nltk.sentiment import SentimentAnalyzer
from nltk.classify import NaiveBayesClassifier, DecisionTreeClassifier, SklearnClassifier

def train_models(training_data, testing_data, sentim_analyzer, trainers):
    models = []
    for trainer in trainers:
        models.append(classify.train_model(training_data, testing_data, 
            sentim_analyzer, trainer))
    return models

def main():
    df = loading.load_data()
    # Pre-proces tweets: tokenize, etc.
    training_data, testing_data = processing.process_data(df)
    # Create a sentiment analyzer.
    sentim_analyzer = SentimentAnalyzer()
    # Add the feature extractor to the sentiment analyzer.
    classify.add_features(training_data, sentim_analyzer)
    # Extract features from training and testing data.
    train, test = classify.extract_features(training_data, testing_data, sentim_analyzer)
    # Select the type of classifier we are going to use.
    trainers = [
        {"name": "Naive Bayes Classifier",
        "model_file": "naive_bayes",
        "train": NaiveBayesClassifier.train,
        },
        # {"name": "Linear SVC Classifier",
        # "model_file": "linear_svc",
        # "train": SklearnClassifier(LinearSVC(
        #     C = 1,
        #     dual = False, # because number of samples > number of features
        #     )).train,},
        # {"name": "K Nearest Neighbours Classifier",
        # "model_file": "k_neighbours",
        # "train": SklearnClassifier(KNeighborsClassifier(
        #     )).train,}
    ]
    # Train all enumerated models
    models = train_models(train, test, sentim_analyzer, trainers)
    for classifier, evaluation, trainer in models:
        print("EVALUATION for {0}".format(trainer['name']))
        for key,value in sorted(evaluation.items()):
            print('\t{0}: {1}'.format(key, value))
    

if __name__ == '__main__':
    main()
