#include <iostream>
#include <ctime>
#include <algorithm>
#include <fstream>
#include <vector>
#include <thread>

using namespace std;

vector<int> read_input(string file_name)
{
	cout << "Start reading data.\n";
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
	cout << "Reading data finished.\n";
	return move(numbers);
}

void parallel_sort(vector<int>& numbers) {
	sort(numbers.begin(), numbers.end());
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
	cout << "Available cores: " << cores << endl;
	cout << "Starting to sort.\n";
	auto start = clock();
	parallel_sort(numbers);
	cout << "Sorting time: " << (clock() - start) / (double)(CLOCKS_PER_SEC / 1000) << " ms\n";
	// Check.
	for (int i = 1; i < numbers.size(); ++i) {
		if (numbers[i-1] > numbers[i]) {
			cout << "Sortare gresita " << i;
			return 1;
		}
	}
	return 0;
}
