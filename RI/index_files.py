#!/usr/bin/env python

"""index_files.py: We use pyLucene to index files from a given folder."""

__author__ = "Vlad-Doru Ion"
__copyright__ = "Copyright 2016, Universitatea din Bucuresti"
__email__ = "vlad.doru@gmail.com"

import glog as log
import lucene
import optparse
import os
import sys
import lib.indexer
import lib.custom_analyzer


def parseArgs(command):
    """Defines and parses command line arguments.

    :returns: (options, path) which represent the options and the folder that we need to index."""
    parser = optparse.OptionParser(
        usage="Usage: ./index_files.py [options] path")
    parser.add_option("-i", "--index", type="string",
                      metavar="INDEX_FOLDER", default="index", help="Index folder to use.")
    parser.add_option("-s", "--stopwords", type="string",
                      metavar="STOPWORDS_FILE", default="stopwords.txt", help="Stopwords to take into consideration.")
    options, args = parser.parse_args(command)
    path = args[1]
    if path == None or options.index == "":
        print("Missing path argument!\n")
        parser.print_help()
        exit(1)
    return options, path


def main():
    """Main function of the script which starts indexing."""

    # Parse the command line arguments.
    options, path = parseArgs(sys.argv)
    assert(path != None)
    assert(options.index != None)
    abs_path = os.path.abspath(path)
    # Check if the path given exists.
    if not os.path.exists(abs_path):
        log.fatal("The following folder does not exist: {0}".format(abs_path))
        exit(1)
    index_path = os.path.abspath(options.index)
    stopwords_path = os.path.abspath(options.stopwords)
    # Log the paths that we are about to use.
    log.info("Indexing files from path: {0}".format(abs_path))
    log.info("Using the index folder:   {0}".format(index_path))
    log.info("Using the stopwords file: {0}".format(stopwords_path))

    log.info("Starting the Lucene VM. Using version: {0}".format(
        lucene.VERSION))
    lucene.initVM()
    indexer = lib.indexer.Indexer(abs_path,
                                  index_path,
                                  lib.custom_analyzer.CustomRomanianAnalyzer,
                                  stopwords_path)
    indexer.indexDocs()

if __name__ == '__main__':
    main()
    exit(0)
