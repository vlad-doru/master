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

int* read_input(string file_name, int& size)
{
	cout << "Citire date" << endl;
	ifstream in(file_name);
	int n;
	in >> n;
	// Alocam memoria necesara pentru vector.
	int *numbers = new int[n];
	for (int i = 0; i < n; ++i)
	{
		int x;	
		in >> x;
		numbers[i] = x;
	}
	cout << "Terminare citire date" << endl;
	size = n;
	return numbers;
}

void merge(int *A,int *L,int leftCount,int *R,int rightCount) {
	int i = 0, j = 0, k = 0;

	while(i<leftCount && j< rightCount) {
		if(L[i]  < R[j]) A[k++] = L[i++];
		else A[k++] = R[j++];
	}
	while(i < leftCount) A[k++] = L[i++];
	while(j < rightCount) A[k++] = R[j++];
}

void merge_sort(int *A,int n) {
	if (n < 2) {		
		return;
	} 

	int mid = n/2;  // find the mid index. 

	// create left and right subarrays
	// mid elements (from index 0 till mid-1) should be part of left sub-array 
	// and (n-mid) elements (from mid to n-1) will be part of right sub-array
	int* L = new int[mid];
	int* R = new int[n - mid];
	
	for(int i = 0;i<mid;i++) L[i] = A[i]; // creating left subarray
	for(int i = mid;i<n;i++) R[i-mid] = A[i]; // creating right subarray

	merge_sort(L,mid);  // sorting the left subarray
	merge_sort(R,n-mid);  // sorting the right subarray
	merge(A,L,mid,R,n-mid);  // Merging L and R into A as sorted list.
	delete[] L;
	delete[] R;
}


void parallel_sort(int* numbers, int n, int partitions_count) {
	/* merge_sort(numbers, n); */
	/* return; */
	int partition_size = n / partitions_count;
	vector<pair<int, int>> partitions;
	for (int i = 0; i < partitions_count; ++i) {
		int start = i * partition_size;
		int end = (i == partitions_count - 1) ? n : (i + 1) * partition_size;
		partitions.push_back(make_pair(start, end));
	}
	std::vector<std::thread> workers;
	for (const auto p : partitions) {
		workers.push_back(std::thread([numbers, p]() {
			merge_sort(numbers + p.first, p.second - p.first);
		}));
	}
	for (auto& w : workers) {
		w.join();
	}
	/* // Avem cate un thread care isi sorteaza partitia respectiva. */
	/* // Pornim cate 4 treaduri pentru a alege fiecare r pivoti. */
	/* int oversampling = 4; */
	/* vector<int> samples; */
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

int main(int argc, char *argv[]) {

	int n = 10000000;
	int *numbers = new int[n];
	for(int i = 0; i < n; ++i) {
		numbers[i] = rand() % 1000000;
	}
	cout << "Au fost generate " << n << " numere!" << endl;
	auto cores = thread::hardware_concurrency();
	cout << "Paralelism maxim disponibil: " << cores << endl;
	cout << "Incepe sortare" << "\n";
	auto start = clock();
	Timer tmr;
	parallel_sort(numbers, n, cores * 2);
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
