package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Grammar struct {
	map[string]struct{} N
	map[string]struct{} T
	map[string][]string P
	string S
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
		fmt.Println(scanner.Text())
		lines = append(lines, strings.TrimSpace(scanner.Text()))
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return nil, nil

	// Process the non terminals.
}

func main() {
	if len(os.Args) != 2 {
		fmt.Print("Va rugam sa dati fisierul ce descrie gramatica ca unic argument.")
		os.Exit(1)
	}
	_, err := NewGrammar(os.Args[1])
	if err != nil {
		fmt.Printf("Fatal error: %v", err)
		os.Exit(1)
	}
}
