#!/usr/bin/env python

"""serach_files.py: We use pyLucene to search the files previously indexed."""

__author__ = "Vlad-Doru Ion"
__copyright__ = "Copyright 2016, Universitatea din Bucuresti"
__email__ = "vlad.doru@gmail.com"

import glog as log
import lucene
import optparse
import os
import sys
import java.io

from org.apache.lucene.index import DirectoryReader
from org.apache.lucene.queryparser.classic import MultiFieldQueryParser, QueryParserBase, QueryParser
from org.apache.lucene.search import IndexSearcher
from org.apache.lucene.search.highlight import Highlighter, QueryScorer, SimpleHTMLFormatter
from org.apache.lucene.store import SimpleFSDirectory
from org.apache.lucene.util import Version

from lib.custom_analyzer import CustomRomanianAnalyzer


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


def search(index, stopwords_path):
    indexStore = SimpleFSDirectory(java.io.File(index))
    searcher = IndexSearcher(DirectoryReader.open(indexStore))
    # Again, we use the Romanian Analyzer.
    analyzer = CustomRomanianAnalyzer(stopwords_path)
    print("Please type nothing to exit.")
    while True:
        print("#" * 10)
        query_input = raw_input("NEW QUERY:")
        if query_input == "":
            return

        print("Searching for {0}".format(query_input))
        parser = MultiFieldQueryParser(Version.LUCENE_CURRENT,
                            ["abstract", "body"], analyzer)
        parser.setDefaultOperator(QueryParserBase.OR_OPERATOR)
        query = MultiFieldQueryParser.parse(parser, query_input)
        highlighter = Highlighter(
            SimpleHTMLFormatter("<<", ">>"), QueryScorer(query))
        hits = searcher.search(query, 50)
        print("{0} total matching documents.".format(hits.totalHits))

        for index, hit in enumerate(hits.scoreDocs):
            doc = searcher.doc(hit.doc)
            print("#Document {0}".format(index + 1))
            print("Path: {0}".format(doc.get("path")))
            print("Name: {0}".format(doc.get("name")))
            print("Score: {0}".format(hit.score))
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
