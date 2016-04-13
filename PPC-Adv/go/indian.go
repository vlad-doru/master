package main

import (
	"./grammar"
	"fmt"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Print("Va rugam sa dati fisierul ce descrie gramatica ca unic argument.")
		os.Exit(1)
	}
	_, err := grammar.NewGrammar(os.Args[1])
	if err != nil {
		fmt.Printf("[EROARE] %v\n", err)
		os.Exit(1)
	}
}
