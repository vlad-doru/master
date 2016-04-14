package main

import (
	"./grammar"
	"bufio"
	"flag"
	"fmt"
	"os"
	"strings"
)

var File string

func init() {
	flag.StringVar(&File, "file", "", "Descrierea gramaticii.")
}

func main() {
	flag.Parse()
	g, err := grammar.NewCNFGrammar(File)
	if err != nil {
		fmt.Printf("[EROARE] %v\n", err)
		os.Exit(1)
	}
	scanner := bufio.NewScanner(os.Stdin)
	fmt.Printf("Va rugam sa introduceti sirul de caractere:")
	for scanner.Scan() {
		input := scanner.Text()
		if input == "" {
			break
		}
		w := strings.Split(input, " ")
		accepted := g.CYKParsing(w)
		if accepted {
			fmt.Printf("\n\t[ACCEPTAT]: %s\n", input)
		} else {
			fmt.Printf("\n\t[NEACCEPTAT]: %s\n", input)
		}
		fmt.Printf("--------------------------\n")
		fmt.Printf("Va rugam sa introduceti sirul de caractere:")
	}
}
