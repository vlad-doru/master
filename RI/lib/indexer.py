"""indexer.py: Define the class used to index the files in a specified folder."""

import datetime
import glog as log
import java.io
import os
import textract

from org.apache.lucene.analysis.ro import RomanianAnalyzer
from org.apache.lucene.document import Document, Field, FieldType
from org.apache.lucene.index import FieldInfo, IndexWriter, IndexWriterConfig
from org.apache.lucene.store import SimpleFSDirectory
from org.apache.lucene.util import Version

class Indexer(object):
    """Used to index files from a specified folder using Apache Lucene."""

    def __init__(self, contentDir, indexDir):
        """Constructor for the indexer. """
        log.info("Constructing the indexer object.")
        self.__contentDir = contentDir
        self.__indexDir = indexDir

        if not os.path.exists(self.__indexDir):
            log.warning("The indexing folder does not exist.")
            log.info("Creating the folder for holding indexes: {0}".format(self.__indexDir))
            os.mkdir(self.__indexDir)

        indexStore = SimpleFSDirectory(java.io.File(self.__indexDir))
        config = IndexWriterConfig(Version.LUCENE_CURRENT, RomanianAnalyzer())
        # Always create a fresh index.
        config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
        self.__indexWriter = IndexWriter(indexStore, config)

        # Init the name field type.
        self.__nameField = FieldType()
        self.__nameField.setIndexed(True)
        self.__nameField.setStored(True)
        self.__nameField.setIndexOptions(FieldInfo.IndexOptions.DOCS_AND_FREQS_AND_POSITIONS)
        # Init the path field type.
        self.__pathField = FieldType()
        self.__pathField.setIndexed(True)
        self.__pathField.setStored(True)
        self.__pathField.setIndexOptions(FieldInfo.IndexOptions.DOCS_AND_FREQS)
    
    def indexDocs(self):
        """Start indexing docs in the self.__contentDir folder."""
        for root, dirnames, filenames in os.walk(self.__contentDir):
            for filename in filenames:
                path = os.path.join(root, filename)
                rel_path = os.path.join(os.path.relpath(root, self.__contentDir), filename)
                log.info("Indexing file: {0}".format(rel_path))
                contents = ""
                try:
                    contents = textract.process(path)
                except Exception as e:
                    log.error("Unexpected error when reading file {1}: {0}".format(filename, e.message()))
                try:
                    self.indexDoc(rel_path, filename, contents)
                except Exception as e:
                    log.error("Unexpected error when adding to index: {0}".format(e.message()))
        log.info("Commiting the index.")
        log.info("We have {0} documents indexed.".format(self.__indexWriter.numDocs()))
        self.__indexWriter.commit()
        log.info("Closing the index.")
        self.__indexWriter.close()

    def indexDoc(self, path, filename, contents):
        doc = Document()
        doc.add(Field("name", filename, self.__nameField))
        doc.add(Field("path", path, self.__pathField))
        log.info("Added the name field: {0}".format(filename))
        log.info("Added the path field: {0}".format(path))
        if len(contents) > 0:
	    log.info("Added the contents field: {0}".format(path))
            doc.add(Field("contents", contents, Field.Store.YES, Field.Index.ANALYZED))
        else:
            log.warning("No contents for {0}".filename)
	self.__indexWriter.addDocument(doc)
