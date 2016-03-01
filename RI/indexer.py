#!/usr/bin/env python

"""indexer.py: We use pyLucene to index files from a given folder."""

__author__      = "Vlad-Doru Ion"
__copyright__   = "Copyright 2016, Universitatea din Bucuresti"
__email__       = "vlad.doru@gmail.com"

import glog as log
import lucene
import optparse
import os
import sys
import java.io

def parseArgs(command):
    """Defines and parses command line arguments.

    :returns: (options, path) which represent the options and the folder that we need to index."""
    parser = optparse.OptionParser(usage = "Usage: ./indexer.py [options] path")
    parser.add_option("-i", "--index", type="string",
                      metavar="INDEX_FOLDER", default=".index", help="Index folder to use.")
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
    index_path = os.path.join(abs_path, options.index)
    # Log the paths that we are about to use.
    log.info("Indexing files from path: {0}".format(abs_path))
    log.info("Using the index folder:   {0}".format(index_path))

    log.info("Starting the Lucene VM. Using version: {0}".format(lucene.VERSION))
    lucene.initVM()

if __name__ == '__main__':
    main()
    exit(0)
