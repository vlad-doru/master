#include <iostream>
#include <ctime>
#include <algorithm>
#include <fstream>
#include <vector>
#include <thread>
#include <utility>
#include <tuple>
#include <mutex>
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
	int partition_size = numbers.size() / partitions_count;
	vector<pair<int, int>> partitions;
	for (int i = 0; i < partitions_count; ++i) {
		int start = i * partition_size;
		int end = (i == partitions_count - 1) ? numbers.size() : (i + 1) * partition_size;
		partitions.push_back(make_pair(start, end));
	}
  // Facem sampling pentru elementele noastre.
	int oversampling = 8;
	vector<int> samples;
	for (int i = 0; i < oversampling * partitions_count; ++i) {
		samples.push_back(numbers[rand() % numbers.size()]); 
	}
	// Sortam sampleul.
	sort(samples.begin(), samples.end());
	// Alegem pivotii.
  vector<int> pivots;
	for (int i = 0, j = oversampling; i < (partitions_count - 1); ++i, j += oversampling) {
		pivots.push_back(samples[j]);
	}
	std::vector<std::thread> workers;
  std::vector<std::vector<int>> buckets;
  std::vector<std::mutex*> locks;
  // Initialize each of the buckets.
	for (const auto p : partitions) {
	  buckets.push_back(vector<int>());
	  locks.push_back(new std::mutex);
  }
	for (const auto p : partitions) {
		workers.push_back(std::thread([&numbers, &pivots, &buckets, &locks, p]() {
		  vector<vector<int>> local_buckets;
      for (int j = 0; j <= pivots.size(); ++j) {
        local_buckets.push_back(vector<int>()); 
      }
      for (int i = p.first; i < p.second; i++) {
        bool ok = false;
        for (int j = 0; j < pivots.size(); ++j) {
          if (numbers[i] < pivots[j]) {
            // add mutex.
            local_buckets[j].push_back(numbers[i]); 
            ok = true;
            break;
          }   
        }
        if (ok) {
          continue;
        }
        local_buckets[pivots.size()].push_back(numbers[i]);
      }
      // Put all the local buckets into the global bucket.
      for (int i = 0; i <= pivots.size(); ++i) {
        locks[i]->lock();
        buckets[i].insert(buckets[i].end(), local_buckets[i].begin(), local_buckets[i].end());
        locks[i]->unlock();
      }
		}));
	}
	for (auto& w : workers) {
		w.join();
	}
	std::vector<std::thread> sort_workers;
  for (auto& bucket : buckets) {
		sort_workers.push_back(std::thread([&bucket]() {
      sort(bucket.begin(), bucket.end());
    }));
  }
	numbers.clear();
	for (auto& w : sort_workers) {
		w.join();
	}
  for (auto& bucket : buckets) {
    numbers.insert(numbers.end(), bucket.begin(), bucket.end());
  }
}

int main(int argc, char *argv[]) {
  srand (0);
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
	parallel_sort(numbers, cores * 2);
	double t = tmr.elapsed();
	cout << "Sorting time: " << t << " s\n";
	// Check.
	for (int i = 1; i < n; ++i) {
		if (numbers[i-1] > numbers[i]) {
			cout << "Sortare gresita " << endl;
			return 1;
		}
	}
	return 0;
}
