"""indexer.py: Define the class used to index the files in a specified folder."""

import datetime
import glog as log
import java.io
import os
import textract
import custom_analyzer

from org.apache.lucene.analysis.ro import RomanianAnalyzer
from org.apache.lucene.document import Document, Field, FieldType
from org.apache.lucene.index import FieldInfo, IndexWriter, IndexWriterConfig
from org.apache.lucene.store import SimpleFSDirectory
from org.apache.lucene.util import Version

from nltk.tokenize import WhitespaceTokenizer

class Indexer(object):
    """Used to index files from a specified folder using Apache Lucene."""

    def __init__(self, contentDir, indexDir, customAnalyzer, stopwords_file):
        """Constructor for the indexer. """
        log.info("Constructing the indexer object.")
        self.__contentDir = contentDir
        self.__indexDir = indexDir

        if not os.path.exists(self.__indexDir):
            log.warning("The indexing folder does not exist.")
            log.info("Creating the folder for holding indexes: {0}".format(
                self.__indexDir))
            os.mkdir(self.__indexDir)

        indexStore = SimpleFSDirectory(java.io.File(self.__indexDir))
        # We use the Custom Romanian Analyzer.
        self.__analyzer = customAnalyzer(stopwords_file)
        config = IndexWriterConfig(Version.LUCENE_CURRENT, self.__analyzer)
        # Always create a fresh index.
        config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)
        self.__indexWriter = IndexWriter(indexStore, config)

        self.__nameField = FieldType()
        self.__nameField.setIndexed(True)
        self.__nameField.setStored(True)
        self.__nameField.setIndexOptions(FieldInfo.IndexOptions.DOCS_AND_FREQS)
 
        self.__pathField = FieldType()
        self.__pathField.setIndexed(True)
        self.__pathField.setStored(False)
        self.__pathField.setIndexOptions(FieldInfo.IndexOptions.DOCS_AND_FREQS)

    def indexDocs(self):
        """Start indexing docs in the self.__contentDir folder."""
        for root, dirnames, filenames in os.walk(self.__contentDir):
            for filename in filenames:
                # Pass over the hidden files.
                if filename[0] == ".":
                    continue
                path = os.path.join(root, filename)
                rel_path = os.path.join(os.path.relpath(
                    root, self.__contentDir), filename)
                print("-" * 20)
                log.info("Indexing file: {0}".format(rel_path))
                contents = ""
                # Try to read from .html, .txt, .docx, .pdf
                try:
                    contents = textract.process(path).strip()
                    log.info(
                        "Sucessfully read contents from file {0}".format(filename))
                except Exception as e:
                    log.error("Unexpected error when reading file {1}: {0}".format(
                        filename, e.message))
                # Try to index the document.
                try:
                    self.indexDoc(rel_path, filename, contents)
                except Exception as e:
                    log.error(
                        "Unexpected error when adding to index: {0}".format(e.message))
                # Print newline to look better.
        print("-" * 20)
        log.info("Commiting the index.")
        log.info("We have {0} documents indexed.".format(
            self.__indexWriter.numDocs()))
        self.__indexWriter.commit()
        log.info("Closing the index.")
        self.__indexWriter.close()
    
    def __splitContents(self, contents):
        span_generator = WhitespaceTokenizer().span_tokenize(contents)
        spans = [span for span in span_generator]
        ABSTRACT_SIZE = 20
        text_words = 0
        if len(spans) > ABSTRACT_SIZE:
            start = spans[ABSTRACT_SIZE][0]
            abstract = contents[:start]
            body = contents[start:]
            return abstract, body
        # Else there is just abstract
        return contents, ""

    def indexDoc(self, path, filename, contents):
        doc = Document()
	log.info("Added the name field: {0}".format(filename))
	doc.add(Field("name", filename, self.__nameField))
	log.info("Added the path field: {0}".format(path))
	doc.add(Field("path", path, self.__pathField))
        log.info("Added the contents field for {0}".format(path))
        doc.add(Field("contents", contents, Field.Store.NO, Field.Index.ANALYZED))
        if len(contents) > 0:
            abstract, body = self.__splitContents(contents)
            field = Field("abstract", abstract, Field.Store.YES, Field.Index.ANALYZED)
            field.setBoost(3.0)
            doc.add(field)
            field = Field("body", body, Field.Store.YES, Field.Index.ANALYZED)
            log.info("Added the abstract and body fields for {0}".format(path))
            doc.add(field)
        else:
            log.warning("No contents for {0}".filename)
        self.__indexWriter.addDocument(doc)
