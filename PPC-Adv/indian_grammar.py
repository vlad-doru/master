import sys
import collections

LAMBDA = "\\"
DEBUG = False

def debug(*args):
    if DEBUG:
        print(*args)

class IndianGrammar(object):

    """Clasa pentru gramatica paralela indiana"""

    def __init__(self, file_path):
        """Constructorul clasei fiind data calea unui fisier.

        :file_path: calea fisierului ce contine descrierea gramaticii
        """
        with open(file_path) as f:
            lines = f.readlines()
        ### NONTERMINALS
        self.N = set(lines[0].strip().split(" "))
        for x in self.N:
            if (len(x) != 1) or (not x.isalpha()) or (x != x.upper()):
                print("Acceptam doar litere mari ca neterminale.")
                exit(1)
        print("Neterminale:", ', '.join(self.N))
        ### TERMINALS
        self.T = set(lines[1].strip().split(" "))
        for x in self.T:
            if (len(x) != 1) or (not (x.isalpha() or x.isdigit())) or (x != x.lower()):
                print("Acceptam doar litere mici sau cifre ca neterminale.")
                exit(1)
        print("Terminale:", ', '.join(self.T))
        ### PRODUCTIONS
        self.P = collections.defaultdict(list)
        productions = lines[2:-1]
        print("Productii:")
        for p in productions:
            try:
                n, aux = p.strip().split("->")
                n = n.strip()
                if not (n in self.N):
                    raise Exception()
                out = aux.strip().split(" ")
                for x in out:
                    if not ((x == LAMBDA) or (x in self.N) or (x in self.T)):
                        raise Exception()
                self.P[n].append(out)
                print("\t{0} -> {1}".format(n, ' '.join(out)))
            except:
                print("Productiile trebuie sa fie de forma N -> ...")
                exit(1)
        ### START
        self.S = lines[-1].upper().strip()
        if not self.S in self.N:
            print("Simbolul de start trebuie sa fie parte din neterminale")
            exit(1)
        print("Simbolul de start: ", self.S)
    
    def vocabulary(self, max_depth):
        return self._voc([self.S], max_depth)

    def _voc(self, left, max_depth, tabs = ""):
        if left == LAMBDA:
            return set("")
        # See how many non terminals we have in the left side.
        left_n = set()
        for x in left:
            if x in self.N:
                left_n.add(x)
        # If we have zero terminals then we have a word.
        if len(left_n) == 0:
            word = ""
            for x in left:
                if x == LAMBDA:
                    continue
                word = word + x
            return set([word])
        # If we canoot do any more derivations we stop.
        if max_depth == 0:
            return set()
        words = set()
        # For each non terminal try to derive it's productions.
        debug(tabs, "LEFT", left)
        for n in left_n:
            for prod in self.P[n]:
                debug(tabs, "PROD", n, prod)
                right = []
                for x in left:
                    if x == n:
                        right = right + prod 
                    else:
                        right.append(x)
                debug(tabs, "RIGHT", right)
                aux_voc = self._voc(right, max_depth - 1, tabs = tabs + "\t")
                debug(tabs, "VOC", aux_voc)
                for x in aux_voc:
                    words.add(x)
        return words
        
        

def main():
    if len(sys.argv) != 2:
        print("Dati ca argument fisierul ce descrie gramatica.")
    file_path = sys.argv[1]
    grammar = IndianGrammar(file_path)
    while True:
        aux = input("Numarul maxim de derivari:")   
        if aux.strip() == "":
            break
        max_depth = 0
        try:
            max_depth = int(aux)
            if max_depth <= 0 :
                raise Exception()
        except:
            print("Introduceti un numar valid (> 0)")
            continue
        print("Calculam multimea cuvintelor obtinute prin maxim {0} derivari.".format(max_depth))
        v = grammar.vocabulary(max_depth)
        print("Cuvintele obtinute:", sorted(v))
    

if __name__ == '__main__':
    main()
