#include <iostream>
#include <ctime>
#include <algorithm>
#include <fstream>
#include <vector>
#include <thread>
#include <utility>
#include <tuple>
#include <chrono>

class Timer
{
	public:
			Timer() : beg_(clock_::now()) {}
			void reset() { beg_ = clock_::now(); }
			double elapsed() const { 
					return std::chrono::duration_cast<second_>
							(clock_::now() - beg_).count(); }

	private:
			typedef std::chrono::high_resolution_clock clock_;
			typedef std::chrono::duration<double, std::ratio<1> > second_;
			std::chrono::time_point<clock_> beg_;
};

using namespace std;


void parallel_sort(vector<int>& numbers, int partitions_count) {
	/* sort(numbers.begin(), numbers.end()); */
	/* return; */

	// Partitionam vectorul in numarul de parititii cerute.
	int partition_size = numbers.size() / partitions_count;
	vector<pair<int, int>> partitions;
	for (int i = 0; i < partitions_count; ++i) {
		int start = i * partition_size;
		int end = (i == partitions_count - 1) ? numbers.size() : (i + 1) * partition_size;
		partitions.push_back(make_pair(start, end));
	}
	// Fiecare worker isi sorteaza partitia lui de vector.
	std::vector<std::thread> workers;
	for (const auto p : partitions) {
		workers.push_back(std::thread([&numbers, &p]() {
			sort(numbers.begin() + p.first, numbers.begin() + p.second);
		}));
	}
	// Asteptam ca toti workerii sa termine sortarea.
	for (auto& w : workers) {
		w.join();
	}
  // Alegem din fiecare partitie partitions_count - 1 elemente la distanta egala. 
	vector<int> samples;
  int s = partition_size / (partitions_count);
  for (const auto p : partitions) {
    for (int i = p.first + s; i < p. second; i += s) {
      samples.push_back(numbers[i]);
    }
  }
  // Sortam sample-ul. 
  sort(samples.begin(), samples.end());
  // Alegem pivotii.
  int p = samples.size() / partitions_count;
  vector<int> pivots;
  for (int i = p ; i < samples.size(); i += p) {
    pivots.push_back(samples[i]);
  }
  // Fiecare worker se ocupa sa interclaseze parti continue din partitiile sortate.
	std::vector<std::thread> merge_workers;
	for (int i = 0; i < partitions_count; ++i) {
		merge_workers.push_back(std::thread([&numbers, &partitions, &pivots, i]() {
		     
		}));
	}
	// Asteptam ca toti workerii sa termine sortarea.
	for (auto& w : merge_workers) {
		w.join();
	}
}

int main(int argc, char *argv[]) {

	int n = 10000000;
	vector<int> numbers;
	for(int i = 0; i < n; ++i) {
		numbers.push_back(rand() % 1000000);
	}
	cout << "Au fost generate " << n << " numere!" << endl;
	auto cores = thread::hardware_concurrency();
	cout << "Paralelism maxim disponibil: " << cores << endl;
	cout << "Incepe sortare" << "\n";
	auto start = clock();
	Timer tmr;
	parallel_sort(numbers, cores);
	double t = tmr.elapsed();
	cout << "Sorting time: " << t << " s\n";
	// Check.
	/* for (int i = 1; i < n; ++i) { */
	/* 	if (numbers[i-1] > numbers[i]) { */
	/* 		cout << "Sortare gresita " << endl; */
	/* 		return 1; */
	/* 	} */
	/* } */
	return 0;
}
