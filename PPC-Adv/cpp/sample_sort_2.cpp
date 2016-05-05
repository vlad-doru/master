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

class Timer {
 public:
  Timer() : beg_(clock_::now()) {}
  void reset() { beg_ = clock_::now(); }
  double elapsed() const { 
    return std::chrono::duration_cast<second_>(clock_::now() - beg_).count(); 
  }

 private:
  typedef std::chrono::high_resolution_clock clock_;
  typedef std::chrono::duration<double, std::ratio<1> > second_;
  std::chrono::time_point<clock_> beg_;
};

using namespace std;

void parallel_sort(vector<int>& numbers, int partitions_count) {
  // Cream partiitile.
	int partition_size = numbers.size() / partitions_count;
	vector<pair<int, int>> partitions;
	for (int i = 0; i < partitions_count; ++i) {
		int start = i * partition_size;
		int end = (i == partitions_count - 1) ? numbers.size() : (i + 1) * partition_size;
		partitions.push_back(make_pair(start, end));
	}
  // Facem sampling pentru elementele noastre.
	int oversampling = 100;
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
	vector<thread> workers;
  vector<vector<int>> buckets;
  vector<mutex*> locks;
  // Initialize each of the buckets.
	for (int i = 0; i < partitions.size(); ++i) {
	  buckets.push_back(vector<int>());
	  locks.push_back(new mutex);
  }
	for (const auto p : partitions) {
		workers.push_back(thread([&numbers, &pivots, &buckets, &locks, p]() {
		  vector<vector<int>> local_buckets;
      for (int j = 0; j <= pivots.size(); ++j) {
        local_buckets.push_back(vector<int>()); 
        // Reserve in advance to avoid multiple allocations.
        local_buckets[j].reserve((p.second - p.first) / pivots.size());
      }
      for (int i = p.first; i < p.second; i++) {
        bool ok = false;
        for (int j = 0; j < pivots.size(); ++j) {
          if (numbers[i] < pivots[j]) {
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
	// Sortam fiecare bucket si il punem inapoi in vectorul initial.
	vector<thread> sort_workers;
	int aux = 0;
  for (auto& bucket : buckets) {
		sort_workers.push_back(thread([&bucket, &numbers, aux]() {
      sort(bucket.begin(), bucket.end());
      copy(bucket.begin(), bucket.end(), numbers.begin() + aux);
    }));
    aux += bucket.size();
  }
	for (auto& w : sort_workers) {
		w.join();
	}
}

int main(int argc, char *argv[]) {
  srand (0);
	int n = 50000000;
	vector<int> numbers;
	for(int i = 0; i < n; ++i) {
		numbers.push_back(rand() % 1000000000);
	}
	cout << "Au fost generate " << n << " numere!" << endl;
	auto cores = thread::hardware_concurrency();
	cout << "Paralelism maxim disponibil: " << cores << endl;
	cout << "Incepe sortare" << "\n";
	auto start = clock();
	// Copy the vector.
	vector<int> ns = numbers;
	Timer tmr;
	parallel_sort(ns, cores * 2);
	double t = tmr.elapsed();
	cout << "Parallel Sorting time: " << t << " s\n";
	// Verifica corectitudinea vectorului.
	for (int i = 1; i < n; ++i) {
		if (ns[i-1] > ns[i]) {
			cout << "Sortare gresita " << endl;
			return 1;
		}
	}
	// Sorteaza normal pentru comparatie.
	ns = numbers;
  tmr.reset();
	sort(ns.begin(), ns.end());
	t = tmr.elapsed();
	cout << "Sorting time: " << t << " s\n";
	return 0;
}
