import pickle

from nltk.sentiment.util import extract_unigram_feats, extract_bigram_feats

def add_features(train_data, sentim_analyzer, min_freq = 10):
    all_words = sentim_analyzer.all_words(train_data, labeled=True)
    unigram_feats = sentim_analyzer.unigram_word_feats(all_words, min_freq = min_freq)
    sentim_analyzer.add_feat_extractor(extract_unigram_feats, unigrams = unigram_feats)
    bigram_feats = sentim_analyzer.bigram_collocation_feats(all_words, min_freq = min_freq)
    print(bigram_feats)
    sentim_analyzer.add_feat_extractor(extract_bigram_feats, bigrams = bigram_feats)

def extract_features(training_data, testing_data, sentim_analyzer):
    return sentim_analyzer.apply_features(training_data), sentim_analyzer.apply_features(testing_data)

def train_model(training_data, testing_data, sentim_analyzer, trainer):
    if "name" in trainer:
        print("Training: {0}".format(trainer["name"]))
    classifier = sentim_analyzer.train(trainer["train"], training_data)
    # Evaluate the test set. 
    evaluation = sentim_analyzer.evaluate(testing_data)
    # Add to the models list.
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
    return (classifier, evaluation, trainer)
