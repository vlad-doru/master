package main

import (
	"./grammar"
	"bufio"
	"flag"
	"fmt"
	"os"
	"sort"
	"strconv"
	"time"
)

var ShouldPrint bool
var File string

func init() {
	flag.BoolVar(&ShouldPrint, "print", true, "Printarea vocabularului.")
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
	fmt.Printf("Va rugam sa introduceti numarul maxim de derivari:")
	for scanner.Scan() {
		input := scanner.Text()
		if input == "" {
			break
		}
		max_depth, err := strconv.Atoi(input)
		if err == nil {
			start := time.Now()
			v := indian.Vocabulary(max_depth)
			duration := time.Since(start)
			if ShouldPrint {
				fmt.Printf("Cuvintele obtinute prin aplicarea a cel mult %d derivari sunt:\n", max_depth)
				words := []string{}
				for w, _ := range v {
					words = append(words, w)
				}
				sort.Strings(words)
				for _, w := range words {
					fmt.Printf("\t*%s\n", w)
				}
			}
			fmt.Printf("Dimensiunea vocabularului: %d\n", len(v))
			fmt.Printf("Durata de calcul a vocabularului: %.3f\n", duration.Seconds())
		}
		fmt.Printf("--------------------------\n")
		fmt.Printf("Va rugam sa introduceti numarul maxim de derivari:")
	}
}
