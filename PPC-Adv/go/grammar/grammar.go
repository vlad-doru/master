package grammar

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Grammar struct {
	N map[string]bool
	T map[string]bool
	P map[string][][]string
	S string
}

const LAMBDA = "\\"

type ProdValidation func([]string) error

func NewGrammar(file_path string, validation ProdValidation) (*Grammar, error) {
	file, err := os.Open(file_path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lines := make([]string, 0)
	for scanner.Scan() {
		lines = append(lines, strings.TrimSpace(scanner.Text()))
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	if len(lines) <= 4 {
		return nil, fmt.Errorf("Va rugam respectati formatul descrierii gramaticii.")
	}

	grammar := &Grammar{}
	// Process the non terminals.
	grammar.N = make(map[string]bool)
	line := strings.Split(lines[0], " ")
	for _, n := range line {
		if strings.ToUpper(n) != n {
			return nil, fmt.Errorf("Neterminal invalid: %s. Se accepta doar litere mari!", n)
		}
		grammar.N[n] = true
	}
	// Process the terminals.
	grammar.T = make(map[string]bool)
	line = strings.Split(lines[1], " ")
	for _, x := range line {
		if strings.ToLower(x) != x {
			return nil, fmt.Errorf("Terminal invalid: %s. Se accepta doar litere mici!")
		}
		grammar.T[x] = true
	}
	// Process production rules.
	index := 2
	grammar.P = make(map[string][][]string)
	for index < len(lines)-1 {
		line = strings.Split(lines[index], " ")
		index++
		if line[1] != "->" {
			return nil, fmt.Errorf("Productie invalida: %s. Forma corecta: N -> ...", lines[index])
		}
		_, ok := grammar.N[line[0]]
		if !ok {
			return nil, fmt.Errorf("Productie invalida: %s. Forma corecta: N -> ...", lines[index])
		}
		for _, x := range line[2:] {
			if x == "\\" {
				continue
			}
			_, N_ok := grammar.N[x]
			_, T_ok := grammar.T[x]
			if !N_ok && !T_ok {
				return nil, fmt.Errorf("Simbolul %s nu se afla in N, T sau \\.", x)
			}
		}
		if validation != nil {
			err := validation(line[2:])
			if err != nil {
				return nil, err
			}
		}
		grammar.P[line[0]] = append(grammar.P[line[0]], line[2:])
	}
	// Set the start symbol.
	_, ok := grammar.N[lines[len(lines)-1]]
	if !ok {
		return nil, fmt.Errorf("Simbolul de start trebuie sa faca parte din multimea neterminalelor.")
	}
	grammar.S = lines[len(lines)-1]
	return grammar, nil
}

func (g *Grammar) IsN(x string) bool {
	_, ok := g.N[x]
	return ok
}
