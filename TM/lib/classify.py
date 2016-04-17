import pickle
import sys
import random

from nltk.util import bigrams
from collections import defaultdict
from nltk.sentiment.util import extract_unigram_feats, extract_bigram_feats
from sklearn import cross_validation

def add_features(data, sentim_analyzer, min_freq = 10):
    # Unigrams as features.
    all_words = sentim_analyzer.all_words(data, labeled=True)
    unigram_feats = sentim_analyzer.unigram_word_feats(all_words, min_freq = min_freq)
    sentim_analyzer.add_feat_extractor(extract_unigram_feats, unigrams = unigram_feats)
    print("Unigram features", len(unigram_feats))
    sys.stdout.flush()
    # Bigrams for features.
    bigram_feats = []
    bigram_freq = defaultdict(int)
    for tokens, _ in data:
        for b in bigrams(tokens):
            bigram_freq[b] += 1
    for bigram, freq in bigram_freq.items():
        if freq <= min_freq:
            continue
        else:
            bigram_feats.append(bigram)
    print("Bigram features: ", len(bigram_feats))
    sys.stdout.flush()
    sentim_analyzer.add_feat_extractor(extract_bigram_feats, bigrams = bigram_feats)

def extract_features(data, sentim_analyzer):
    return sentim_analyzer.apply_features(data)

def train_model(input_data, sentim_analyzer, trainer, sample_size = None):
    if sample_size == None:
        sample_size = len(input_data)
    if "name" in trainer:
        print("Training: {0}".format(trainer["name"]))
        sys.stdout.flush()
    data = random.sample(input_data, sample_size)
    print("Sampled", len(data), "elements")
    sys.stdout.flush()
    cv = cross_validation.KFold(len(data), n_folds=10, shuffle = True)
    evaluations = []
    fold = 0
    for train_index, test_index in cv:
        train_set = set(train_index)
        test_set = set(test_index)
        fold+=1
        print("\tTraining fold", fold)
        sys.stdout.flush()

        training_data = [x for i, x in enumerate(data) if i in train_set]
        testing_data = [x for i, x in enumerate(data) if i in test_set]

        training_features = extract_features(training_data, sentim_analyzer)
        testing_features = extract_features(testing_data, sentim_analyzer)

        classifier = sentim_analyzer.train(trainer["train"], training_features)
        print("\tEvaluating fold", fold)
        sys.stdout.flush()
        evaluations.append(sentim_analyzer.evaluate(testing_features))
    return evaluations
