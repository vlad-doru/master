#include <iostream>
#include <vector>
#include <ctime>
#include <cassert>

#include "timer.h"
#include "sample_sort.h"

int CORES = thread::hardware_concurrency();

bool good_sort(vector<int>& numbers) {
	for (int i = 1; i < numbers.size(); ++i) {
		if (numbers[i-1] > numbers[i]) {
      return false;
		}
	}
	return true;
}

void benchmark(int size) {
  for (int k = 0; k < 10; ++k) {
    vector<int> numbers;
    for(int i = 0; i < size; ++i) {
      numbers.push_back(rand() % 1000000000);
    }
    auto start = clock();
    // Copy the vector.
    vector<int> ns = numbers;
    Timer tmr;
    parallel_sort(ns, CORES * 2);
    double t = tmr.elapsed();
    cout << "Sample sort, " << t << ", " << size << endl;
    // Verifica corectitudinea vectorului.
    assert(good_sort(ns));
    // Sorteaza cu merge sort.
    ns = numbers;
    tmr.reset();
    merge_sort(ns);
    t = tmr.elapsed();
    cout << "Merge sort,  " << t << ", " << size << endl;
    assert(good_sort(ns));
  }
}

int main(int argc, char *argv[]) {
  srand (time(NULL));
	cout << "Paralelism maxim disponibil: " << CORES << endl;
	cout << "Algoritm, Timp (s), Dimensiune" << endl;

  vector<int> benchmark_sizes{1000, 100000, 1000000, 10000000};
  for (const auto size : benchmark_sizes) {
    benchmark(size);
  }
	return 0;
}
