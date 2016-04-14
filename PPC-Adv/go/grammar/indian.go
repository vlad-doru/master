package grammar

import (
	"sync"
)

type IndianGrammar struct {
	*Grammar

	vocabulary map[string]bool
	mutex      sync.Mutex
}

func NewIndianGrammar(file_path string) (*IndianGrammar, error) {
	g, err := NewGrammar(file_path, nil)
	if err != nil {
		return nil, err
	}
	v := make(map[string]bool)
	m := sync.Mutex{}
	return &IndianGrammar{g, v, m}, nil
}

func (g *IndianGrammar) Vocabulary(max_depth int) map[string]bool {
	g.derive([]string{g.S}, max_depth)
	return g.vocabulary
}

func (g *IndianGrammar) derive(left []string, max_depth int) {
	// Get all nonterminals in current derivation.
	left_n := make(map[string]bool)
	for _, x := range left {
		if g.IsN(x) {
			left_n[x] = true
		}
	}
	if len(left_n) == 0 {
		word := ""
		for _, x := range left {
			if x == LAMBDA {
				continue
			}
			word = word + x
		}
		g.mutex.Lock()
		g.vocabulary[word] = true
		g.mutex.Unlock()
		return
	}
	if max_depth <= 0 {
		return
	}
	// Waitgroup to wait for all goroutines to finish.
	group := sync.WaitGroup{}
	for n, _ := range left_n {
		group.Add(1) // Mark to wait for routine to finish.
		go func() {  // Paralellism
			for _, prod := range g.P[n] {
				right := make([]string, 0)
				for _, x := range left {
					if x == n {
						right = append(right, prod...)
					} else {
						right = append(right, x)
					}
				}
				g.derive(right, max_depth-1)
			}
			group.Done()
		}()
	}
	group.Wait() // Wait for all to finish.
}

func (g *IndianGrammar) Derivation(left []string) [][]string {
	// Get all nonterminals in current derivation.
	left_n := make(map[string]bool)
	for _, x := range left {
		if g.IsN(x) {
			left_n[x] = true
		}
	}
	if len(left_n) == 0 {
		return [][]string{left}
	}
	result := [][]string{}
	// Waitgroup to wait for all goroutines to finish.
	group := sync.WaitGroup{}
	for n, _ := range left_n {
		group.Add(1) // Mark to wait for routine to finish.
		go func() {  // Paralellism
			for _, prod := range g.P[n] {
				right := make([]string, 0)
				for _, x := range left {
					if x == n {
						right = append(right, prod...)
					} else {
						right = append(right, x)
					}
				}
				result = append(result, right)
			}
			group.Done()
		}()
	}
	group.Wait() // Wait for all to finish.
	return result
}
