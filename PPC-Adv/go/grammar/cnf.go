package grammar

import (
	"fmt"
)

type CNFGrammar struct {
	*Grammar
}

func validation(g *Grammar, prod []string) error {
	if len(prod) == 1 && g.IsT(prod[0]) {
		return nil
	}
	if len(prod) == 2 && g.IsN(prod[0]) && g.IsT(prod[1]) {
		return nil
	}
	return fmt.Errorf("Productia nu este in CNF: %v", prod)
}

func NewCNFGrammar(file_path string) (*CNFGrammar, error) {
	g, err := NewGrammar(file_path, validation)
	if err != nil {
		return nil, err
	}
	return &CNFGrammar{g}, nil
}
