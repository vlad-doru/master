#!/usr/bin/env python

"""serach_files.py: We use pyLucene to search the files previously indexed."""

__author__      = "Vlad-Doru Ion"
__copyright__   = "Copyright 2016, Universitatea din Bucuresti"
__email__       = "vlad.doru@gmail.com"

import glog as log
import lucene
import optparse
import os
import sys
import java.io

from org.apache.lucene.analysis.ro import RomanianAnalyzer
from org.apache.lucene.index import DirectoryReader
from org.apache.lucene.queryparser.classic import QueryParser
from org.apache.lucene.search import IndexSearcher
from org.apache.lucene.search.highlight import Highlighter, QueryScorer, SimpleHTMLFormatter 
from org.apache.lucene.store import SimpleFSDirectory
from org.apache.lucene.util import Version

def parseArgs(command):
    """Defines and parses command line arguments.

    :returns: options which represent the options and the index folder that we will use."""
    parser = optparse.OptionParser(usage = "Usage: ./serach_files.py [options]")
    parser.add_option("-i", "--index", type="string",
                      metavar="INDEX_FOLDER", default="index", help="Index folder to use.")
    options, args = parser.parse_args(command)
    return options

def run(index):
    indexStore = SimpleFSDirectory(java.io.File(index))
    searcher = IndexSearcher(DirectoryReader.open(indexStore))
    analyzer = RomanianAnalyzer()
    print("Please type nothing to exit.")
    while True:
        print("#" * 10)
        query_input = raw_input("NEW QUERY:")
        if query_input == "":
            return
 
        print("Searching for {0}".format(query_input))
        query = QueryParser(Version.LUCENE_CURRENT, "contents", analyzer).parse(query_input)
        highlighter = Highlighter(SimpleHTMLFormatter("<<", ">>"), QueryScorer(query))
        hits = searcher.search(query, 50)
        print("{0} total matching documents.".format(hits.totalHits))
 
        for index, hit in enumerate(hits.scoreDocs):
            doc = searcher.doc(hit.doc)
            contents = doc.get("contents")
            print("#Document {0}".format(index + 1))
            print("Path: {0}".format(doc.get("path")))
            print("Name: {0}".format(doc.get("name")))
            print("Score: {0}".format(hit.score))
            tokenStream = analyzer.tokenStream("contents", java.io.StringReader(contents))
            for fragment in highlighter.getBestTextFragments(tokenStream, contents, True, 1):
                print(fragment.toString().strip())
            print("-" * 10)
 
def main():
    # Parse the command line arguments.
    options = parseArgs(sys.argv)
    assert(options.index != None)
    index_path = os.path.abspath(options.index)
    # Log the paths that we are about to use.
    log.info("Using the index folder: {0}".format(index_path))

    log.info("Starting the Lucene VM. Using version: {0}".format(lucene.VERSION))
    lucene.initVM()

    run(index_path)

if __name__ == '__main__':
    main()
    exit(0)
