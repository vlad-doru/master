import pandas
import numpy as np
import nltk
import pickle

from lib import loading
from lib import processing

from sklearn.svm import LinearSVC
from sklearn.neighbors import KNeighborsClassifier

from nltk.sentiment.util import extract_unigram_feats
from nltk.sentiment import SentimentAnalyzer
from nltk.classify import NaiveBayesClassifier, DecisionTreeClassifier, SklearnClassifier

def add_features(train_data, sentim_analyzer, min_freq = 10):
    all_words = sentim_analyzer.all_words(train_data, labeled=True)
    unigram_feats = sentim_analyzer.unigram_word_feats(all_words, min_freq= min_freq)
    print("Number of features: {0}".format(len(unigram_feats)))
    sentim_analyzer.add_feat_extractor(extract_unigram_feats, unigrams=unigram_feats)

def extract_features(training_data, testing_data, sentim_analyzer):
    return sentim_analyzer.apply_features(training_data), sentim_analyzer.apply_features(testing_data)

def train_models(training_data, testing_data, sentim_analyzer, trainers):
    models = []
    for trainer in trainers:
        print("Training: {0}".format(trainer["name"]))
        classifier = sentim_analyzer.train(trainer["train"], training_data)
        # Evaluate the test set. 
        evaluation = sentim_analyzer.evaluate(testing_data)
        # Add to the models list.
        models.append((classifier, evaluation, trainer))
        # Save the classifier to a file.
        if "model_file" in trainer:
            model_file="./models/{}.pickle".format(trainer["model_file"])
            print("Saving the model to file: {0}".format(model_file))
            f = open(model_file, "wb")
            pickle.dump({
                "classifier": classifier,
                "evaluation": evaluation,
                "trainer": trainer,
            }, f, -1)
    return models

def main():
    df = loading.load_data()
    # Pre-proces tweets: tokenize, etc.
    training_data, testing_data = processing.process_data(df)
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
            C = 1,
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
