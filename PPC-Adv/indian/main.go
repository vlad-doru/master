package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Grammar struct {
	N map[string]bool
	T map[string]bool
	P map[string][]string
	S string
}

func NewGrammar(file_path string) (*Grammar, error) {
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
		if (len(n) != 1) || (strings.ToUpper(n) != n) {
			return nil, fmt.Errorf("Neterminal invalid: %s. Se accepta doar litere mari!", n)
		}
		grammar.N[n] = true
	}
	// Process the terminals.
	grammar.T = make(map[string]bool)
	line = strings.Split(lines[1], " ")
	for _, x := range line {
		if (len(x) != 1) || (strings.ToLower(x) != x) {
			return nil, fmt.Errorf("Terminal invalid: %s. Se accepta doar litere mici!")
		}
		grammar.T[x] = true
	}
	// Process production rules.
	index := 2
	grammar.P = make(map[string][]string)
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
		grammar.P[line[0]] = line[2:]
	}
	// Set the start symbol.
	_, ok := grammar.N[lines[len(lines)-1]]
	if !ok {
		return nil, fmt.Errorf("Simbolul de start trebuie sa faca parte din multimea neterminalelor.")
	}
	return grammar, nil
}

func main() {
	if len(os.Args) != 2 {
		fmt.Print("Va rugam sa dati fisierul ce descrie gramatica ca unic argument.")
		os.Exit(1)
	}
	_, err := NewGrammar(os.Args[1])
	if err != nil {
		fmt.Printf("[EROARE] %v\n", err)
		os.Exit(1)
	}
}
