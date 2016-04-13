package main

import (
	"./grammar"
	"bufio"
	"flag"
	"fmt"
	"os"
	"sort"
	"strconv"
	"sync"
	"time"
)

var ShouldPrint bool
var File string

func init() {
	flag.BoolVar(&ShouldPrint, "print", true, "Printarea vocabularului.")
	flag.StringVar(&File, "file", "", "Descrierea gramaticii.")
}

type IndianGrammar struct {
	*grammar.Grammar
}

func NewIndianGrammar(file_path string) (*IndianGrammar, error) {
	g, err := grammar.NewGrammar(file_path)
	if err != nil {
		return nil, err
	}
	return &IndianGrammar{g}, nil
}

func (g *IndianGrammar) Vocabulary(max_depth int) map[string]bool {
	return g.derive([]string{g.S}, max_depth)
}

func (g *IndianGrammar) derive(left []string, max_depth int) map[string]bool {
	// Get all nonterminals in current derivation.
	left_n := make(map[string]bool)
	for _, x := range left {
		if g.IsN(x) {
			left_n[x] = true
		}
	}
	vocabulary := make(map[string]bool)
	if len(left_n) == 0 {
		word := ""
		for _, x := range left {
			if x == grammar.LAMBDA {
				continue
			}
			word = word + x
		}
		vocabulary[word] = true
		return vocabulary
	}
	if max_depth <= 0 {
		return vocabulary
	}
	// Mutex to protect the vocabulary dictionary.
	mutex := sync.Mutex{}
	// Waitgroup to wait for all goroutines to finish.
	group := sync.WaitGroup{}
	for n, _ := range left_n {
		group.Add(1) // Mark to wait for routine to finish.
		go func() {  // Paralellism
			for _, prod := range g.P[n] {
				group.Add(1)             // Mark to wait for routine to finish.
				go func(prod []string) { // Parallelism
					right := make([]string, 0)
					for _, x := range left {
						if x == n {
							right = append(right, prod...)
						} else {
							right = append(right, x)
						}
					}
					aux_voc := g.derive(right, max_depth-1)
					mutex.Lock() // Protect dictionary.
					for w, _ := range aux_voc {
						vocabulary[w] = true
					}
					mutex.Unlock()
					group.Done()
				}(prod)
			}
			group.Done()
		}()
	}
	group.Wait() // Wait for all to finish.
	return vocabulary
}

func main() {
	flag.Parse()
	indian, err := NewIndianGrammar(File)
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
			fmt.Printf("Durata de calcul a vocabularului: %.3f\n", duration.Seconds())
		}
		fmt.Printf("--------------------------\n")
		fmt.Printf("Va rugam sa introduceti numarul maxim de derivari:")
	}
}
