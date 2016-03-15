import lucene
import textract
from lucene.collections import *
from org.tartarus.snowball.ext import RomanianStemmer
from org.apache.pylucene.analysis import PythonAnalyzer
from org.apache.lucene.analysis.core import LowerCaseTokenizer, StopFilter, StopAnalyzer
from org.apache.lucene.analysis.standard import StandardTokenizer
from org.apache.lucene.analysis.standard import StandardFilter
from org.apache.lucene.analysis.snowball import SnowballFilter
from org.apache.lucene.analysis.miscellaneous import ASCIIFoldingFilter
from java.util import HashSet, ArrayList, Arrays
from org.apache.lucene.analysis.util import CharArraySet
from org.apache.lucene.analysis import Analyzer

class CustomRomanianAnalyzer(PythonAnalyzer):

    def __init__(self, stopwords_file = ""):
        super(CustomRomanianAnalyzer, self).__init__()
        stopwords = []
        if len(stopwords_file) > 0:
            stopwords = textract.process(stopwords_file).split()
        self.__stopwords = StopFilter.makeStopSet(Arrays.asList(stopwords))


    def createComponents(self, field, reader):
        tokenizer = LowerCaseTokenizer(reader)
        filter = StandardFilter(tokenizer)
        filter = StopFilter(filter, self.__stopwords)
        filter = SnowballFilter(filter, RomanianStemmer())
        filter = ASCIIFoldingFilter(filter)
        return Analyzer.TokenStreamComponents(tokenizer, filter)
