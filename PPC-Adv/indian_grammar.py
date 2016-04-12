import sys

LAMBDA = "\\"

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
        self.P = []
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
                self.P.append((n, out))
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
        

def main():
    if len(sys.argv) != 2:
        print("Dati ca argument fisierul ce descrie gramatica.")
    file_path = sys.argv[1]
    grammar = IndianGrammar(file_path)
    

if __name__ == '__main__':
    main()
