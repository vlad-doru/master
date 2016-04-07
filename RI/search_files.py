#!/usr/bin/env python

"""serach_files.py: We use pyLucene to search the files previously indexed."""

__author__ = "Vlad-Doru Ion"
__copyright__ = "Copyright 2016, Universitatea din Bucuresti"
__email__ = "vlad.doru@gmail.com"

import glog as log
import lucene
import inspect
import optparse
import os
import sys
import java.io
import java.util
import math
import collections

from org.apache.lucene.index import DirectoryReader, Term, TermsEnum, MultiFields, DocsEnum
from org.apache.lucene.queryparser.classic import MultiFieldQueryParser, QueryParserBase, QueryParser
from org.apache.lucene.search import IndexSearcher, DocIdSetIterator
from org.apache.lucene.search.similarities import DefaultSimilarity
from org.apache.lucene.search.highlight import Highlighter, QueryScorer, SimpleHTMLFormatter
from org.apache.lucene.store import SimpleFSDirectory
from org.apache.lucene.util import Version, BytesRefIterator, BytesRef

from lib.custom_analyzer import CustomRomanianAnalyzer

from nltk.tokenize import WhitespaceTokenizer


def parseArgs(command):
    """Defines and parses command line arguments.

    :returns: options which represent the options and the index folder that we will use."""
    parser = optparse.OptionParser(usage="Usage: ./serach_files.py [options]")
    parser.add_option("-i", "--index", type="string",
                      metavar="INDEX_FOLDER", default="index", help="Index folder to use.")
    parser.add_option("-s", "--stopwords", type="string",
                      metavar="STOPWORDS_FILE", default="stopwords_ro.txt", help="Stopwords to take into consideration.")

    options, args = parser.parse_args(command)
    return options

def idf(docFreq, numDocs):
    return math.log(float(numDocs)/(float(docFreq)+1)) + 1

def search(index, stopwords_path):
    indexStore = SimpleFSDirectory(java.io.File(index))
    reader = DirectoryReader.open(indexStore)
    searcher = IndexSearcher(reader)
    # Again, we use the Romanian Analyzer.
    analyzer = CustomRomanianAnalyzer(stopwords_path)
    print("Please type nothing to exit.")
    while True:
        print("##############")
        query_input = raw_input("NEW QUERY:")
        if query_input == "":
            return

        print("Searching for {0}".format(query_input))
        parser = MultiFieldQueryParser(Version.LUCENE_CURRENT,
                            ["abstract", "body"], analyzer)
        parser.setDefaultOperator(QueryParserBase.OR_OPERATOR)
        query = None
        try: 
            query = MultiFieldQueryParser.parse(parser, query_input)
        except:
            continue

        # We use another field for computing the idf and tf.
        contents_parser = QueryParser(Version.LUCENE_CURRENT, "contents", analyzer)
        query_tokens = WhitespaceTokenizer().tokenize(query_input)
        query_terms = []
        for query_token in query_tokens:
            terms_set = java.util.HashSet()
            contents_query = QueryParser.parse(contents_parser, query_token)
            contents_query.extractTerms(terms_set)
            terms = [x for x in terms_set]
            if len(terms) == 0:
                continue
            assert(len(terms) == 1)
            query_terms.append((Term.cast_(terms[0]), query_token))

        for term, token in query_terms:
            print("IDF for token: {0}".format(token))
            idf_value = idf(reader.docFreq(term), reader.numDocs())
            print("\tDocument frequency: {0}".format(reader.docFreq(term))) 
            print("\tIndexed documents: {0}".format(reader.numDocs())) 
            print("\tIDF: {0}".format(idf_value))

        token_freq = collections.defaultdict(lambda: collections.defaultdict(int))
        for term, token in query_terms:
            term_bytes = term.bytes()
            docs_enum = MultiFields.getTermDocsEnum(reader, 
                    MultiFields.getLiveDocs(reader), 
                    "contents", 
                    term_bytes, 
                    DocsEnum.FLAG_FREQS)
            while True:
                doc = docs_enum.nextDoc()
                if doc == DocIdSetIterator.NO_MORE_DOCS:
                    break
                token_freq[token][doc] = docs_enum.freq()

        # Searching 
        highlighter = Highlighter(SimpleHTMLFormatter("<<", ">>"), QueryScorer(query))
        hits = searcher.search(query, 50)
        print("{0} total matching documents.".format(hits.totalHits))

        for index, hit in enumerate(hits.scoreDocs):
            doc = searcher.doc(hit.doc)
            print("#Document {0}".format(index + 1))
            print("Path: {0}".format(doc.get("path")))
            print("Name: {0}".format(doc.get("name")))
            print("Score: {0}".format(hit.score))
            print("Query terms frequencies: ")
            for _, token in query_terms:
                tf = token_freq[token][hit.doc]
                print("\t {0} TF: sqrt({1}) = {2}".format(token, tf, round(math.sqrt(float(tf)), 2)))

	    # Highlight the matches.
	    print("ABSTRACT MATCHES:")
            abstract = doc.get("abstract")
            tokenStream = analyzer.tokenStream(
                "abstract", java.io.StringReader(abstract))
            for fragment in highlighter.getBestTextFragments(tokenStream, abstract, True, 1):
                print(fragment.toString().strip())
	    print("BODY MATCHES:")
            body = doc.get("body")
            tokenStream = analyzer.tokenStream(
                "body", java.io.StringReader(body))
            for fragment in highlighter.getBestTextFragments(tokenStream, body, True, 1):
                print(fragment.toString().strip())
            print("-" * 10)


def main():
    # Parse the command line arguments.
    options = parseArgs(sys.argv)
    assert(options.index != None)
    index_path = os.path.abspath(options.index)
    stopwords_path = os.path.abspath(options.stopwords)
    # Log the paths that we are about to use.
    log.info("Using the index folder: {0}".format(index_path))
    log.info("Using the stopwords file: {0}".format(stopwords_path))

    log.info("Starting the Lucene VM. Using version: {0}".format(
        lucene.VERSION))
    lucene.initVM()

    search(index_path, stopwords_path)

if __name__ == '__main__':
    main()
    exit(0)
