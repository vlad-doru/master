#include <iostream>
#include <ctime>
#include <algorithm>
#include <fstream>
#include <vector>
#include <thread>
#include <utility>
#include <tuple>

using namespace std;

vector<int> read_input(string file_name)
{
	cout << "Citire date" << endl;
	ifstream in(file_name);
	int n;
	in >> n;
	vector<int> numbers;
	for (int i = 0; i < n; ++i)
	{
		int x;	
		in >> x;
		numbers.push_back(x);
	}
	cout << "Terminare citire date" << endl;
	return move(numbers);
}

void parallel_sort(vector<int>& numbers, int partitions_count) {
	/* sort(numbers.begin(), numbers.end()); */
	/* return; */
	int partition_size = numbers.size() / partitions_count;
	vector<pair<int, int>> partitions;
	for (int i = 0; i < partitions_count; ++i) {
		int start = i * partition_size;
		int end = (i == partitions_count - 1) ? numbers.size() : (i + 1) * partition_size;
		partitions.push_back(make_pair(start, end));
	}
	std::vector<std::thread> workers;
	for (const auto p : partitions) {
		workers.push_back(std::thread([&numbers, p]() {
			sort(numbers.begin() + p.first, numbers.begin() + p.second);
		}));
	}
	for (auto& w : workers) {
		w.join();
	}
	// Avem cate un thread care isi sorteaza partitia respectiva.
	// Pornim cate 4 treaduri pentru a alege fiecare r pivoti.
	int oversampling = 4;
	vector<int> samples;
	/* // TODO: Sample from each patition here. */
	/* for (int i = 0; i < oversampling * (partitions - 1); ++i) { */
	/* 	samples.push_back(numbers[rand() % numbers.size()]); */ 
	/* } */
	/* // Sortam sampleul. */
	/* sort(samples.begin(), samples.end()); */
	/* int *pivots = new int[partitions]; */
	/* for (int i = 0, j = oversampling / 2; i < (partitions - 1); ++i, j += oversampling) { */
	/* 	pivots[i] = samples[j]; */
	/* } */
}

int main(int argc, char *argv[])
{
	if (argc != 2)
	{
		cout << "Va rugam dati ca argument calea fisierului de intrare\n";
		return 1;
	}
	string file = string(argv[1]);
	auto numbers = read_input(file);
	auto cores = thread::hardware_concurrency();
	cout << "Paralelism maxim disponibil: " << cores << endl;
	cout << "Incepe sortare" << endl;
	auto start = clock();
	parallel_sort(numbers, cores);
	cout << "Sorting time: " << (clock() - start) / (double)(CLOCKS_PER_SEC / 1000) << " ms\n";
	// Check.
	for (int i = 1; i < numbers.size(); ++i) {
		if (numbers[i-1] > numbers[i]) {
			cout << "Sortare gresita " << endl;
			return 1;
		}
	}
	return 0;
}
