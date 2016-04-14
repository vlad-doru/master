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
	indian, err := grammar.NewIndianGrammar(File)
	if err != nil {
		fmt.Printf("[EROARE] %v\n", err)
		os.Exit(1)
	}
	scanner := bufio.NewScanner(os.Stdin)
	fmt.Printf("Introduceti sirul pentru care vreti sa aplicati o derivare:")
	for scanner.Scan() {
		input := scanner.Text()
		if input == "" {
			break
		}
		s := strings.Split(input, " ")
		if err == nil {
			v := indian.Derivation(s)
			fmt.Printf("Prin derivarea sirului dat se obtine:\n")
			for _, w := range v {
				fmt.Printf("\t*%v\n", w)
			}
		}
		fmt.Printf("--------------------------\n")
		fmt.Printf("Introduceti sirul pentru care vreti sa aplicati o derivare:")
	}
}
