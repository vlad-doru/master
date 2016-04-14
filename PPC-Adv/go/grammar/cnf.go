package grammar

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
	"text/tabwriter"
)

type NProd struct {
	N string
	L string
	R string
}

type CNFGrammar struct {
	*Grammar

	inverse map[string][]string
	nprods  []NProd
	mutex   sync.RWMutex
}

func validation(g *Grammar, prod []string) error {
	if len(prod) == 1 && g.IsT(prod[0]) {
		return nil
	}
	if len(prod) == 2 && g.IsN(prod[0]) && g.IsN(prod[1]) {
		return nil
	}
	return fmt.Errorf("Productia nu este in CNF: %v", prod)
}

func NewCNFGrammar(file_path string) (*CNFGrammar, error) {
	g, err := NewGrammar(file_path, validation)
	if err != nil {
		return nil, err
	}

	inverse := make(map[string][]string)
	nprods := make([]NProd, 0)
	for n, ps := range g.P {
		for _, p := range ps {
			if len(p) == 1 {
				t := p[0]
				_, ok := inverse[t]
				if !ok {
					inverse[t] = make([]string, 0)
				}
				inverse[t] = append(inverse[t], n)
			} else {
				nprods = append(nprods, NProd{n, p[0], p[1]})
			}
		}
	}
	return &CNFGrammar{g, inverse, nprods, sync.RWMutex{}}, nil
}

func (g *CNFGrammar) addN(dp map[int]map[int]map[string]bool, i int, j int, N string) {
	_, ok := dp[i][j]
	if !ok {
		g.mutex.Lock() // Blocam mutex-ul la scriere.
		dp[i][j] = make(map[string]bool)
		g.mutex.Unlock()
	}
	dp[i][j][N] = true
}

func (g *CNFGrammar) CYKParsing(ws []string) bool {
	dp := make(map[int]map[int]map[string]bool)
	for i, w := range ws {
		ns, ok := g.inverse[w]
		if ok {
			dp[i] = make(map[int]map[string]bool)
			for _, n := range ns {
				g.addN(dp, i, i+1, n)
				g.addN(dp, i, i+1, w)
			}
		} else {
			return false
		}
	}
	wait := sync.WaitGroup{}
	// Calculam pentru fiecare subsecventa de lungime l.
	for l := 2; l <= len(ws); l++ {
		// Luam toti posibilii indici de inceput.

		for i := 0; i < len(ws)-l+1; i++ {
			j := i + l
			wait.Add(1)            // Efectuam pt fiecare subsecventa de lungime l calculele in paralel
			go func(i, j, l int) { // Paralelism
				// Impartim secventa [i, j] in [i, k] [k + 1, j]
				for k := i + 1; k < j; k++ {
					for _, p := range g.nprods {
						g.mutex.RLock() // Folosim un read-write mutex pentru performanta.
						_, left := dp[i][k][p.L]
						_, right := dp[k][j][p.R]
						g.mutex.RUnlock()
						if left && right {
							g.addN(dp, i, j, p.N)
						}
					}
				}
				wait.Done() // Marcam subsecventa ca procesata.
			}(i, j, l)
		}
		wait.Wait() // Asteptam toate subsecventele pentru a putea trece la lungimi mai mari.
	}
	_, accepted := dp[0][len(ws)][g.S]
	if accepted {
		// Printam matricea de programare dinamica.
		fmt.Println("\n---------------------------------")
		wr := new(tabwriter.Writer)
		// Formatare.
		wr.Init(os.Stdout, 0, 8, 0, '\t', 0)
		for i := 0; i < len(ws); i++ {
			line := []string{strconv.Itoa(i)}
			for j := 1; j <= len(ws); j++ {
				d, ok := dp[i][j]
				if !ok {
					line = append(line, "-")
					continue
				}
				cell := make([]string, 0)
				for n, _ := range d {
					cell = append(cell, n)
				}
				line = append(line, strings.Join(cell, ", "))
			}
			fmt.Fprintln(wr, strings.Join(line, "\t"))
		}
		wr.Flush()
	}
	return accepted
}
