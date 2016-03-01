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
from org.apache.lucene.document import Document, Field, FieldType
from org.apache.lucene.index import FieldInfo, IndexWriter, IndexWriterConfig
from org.apache.lucene.store import SimpleFSDirectory
from org.apache.lucene.util import Version
from org.apache.lucene.search import IndexSearcher
from org.apache.lucene.search.highlight import Highlighter, TokenSources, QueryScorer, SimpleHTMLFormatter, SimpleFragmenter
from org.apache.lucene.index import DirectoryReader
from org.apache.lucene.queryparser.classic import QueryParser

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
    while True:
        print "-" * 60
        print "Hit enter with no input to quit."
        command = raw_input("Query:")
        if command == '':
            return
 
        print
        print "Searching for:", command
        query = QueryParser(Version.LUCENE_CURRENT, "contents", analyzer).parse(command)
        highlighter = Highlighter(SimpleHTMLFormatter("<< ", " >>"), QueryScorer(query))
        highlighter.setTextFragmenter(SimpleFragmenter(25))
        print query
        hits = searcher.search(query, 50)
        print "%s total matching documents." % hits.totalHits, "\n"
 
        for hit in hits.scoreDocs:
            doc = searcher.doc(hit.doc)
            contents = doc.get("contents")
            print 'path:', doc.get("path"), 'name:', doc.get("name"), "score:", hit.score
            print '------++++++++++++++++++++++++++++++++++++++++++--------'
            tokenStream = analyzer.tokenStream("contents", java.io.StringReader(contents))
            t = [f.toString().encode('utf-8') for f in highlighter.getBestTextFragments(tokenStream, contents, True, 2) if f.getScore()]
            print ' ... '.join(t).replace('\n', ' ')
            print ""
 
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
