#include <iostream>
#include <algorithm>
#include <vector>
#include <thread>
#include <utility>
#include <mutex>

#include "merge_sort.h"

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
	int oversampling = partitions_count;
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
  // Initializam fiecare bucket.
	for (int i = 0; i < partitions.size(); ++i) {
	  buckets.push_back(vector<int>());
	  locks.push_back(new mutex);
  }
	// Pentru fiecare partitie vom avea un worker care va distribui fiecare element
  // in bucket-ul corespunzator, conform pivotilor alesi.
	for (const auto p : partitions) {
		workers.push_back(thread([&numbers, &pivots, &buckets, &locks, p]() {
			// Vom folosi nise bucket-uri locale pentru a evita folosire repetata a 
			// mutexurilor.
		  vector<vector<int>> local_buckets;
      for (int j = 0; j <= pivots.size(); ++j) {
        local_buckets.push_back(vector<int>()); 
				// Rezervam memorie in avans pentru a evita multiple alocari.
        local_buckets[j].reserve((p.second - p.first) / pivots.size());
      }
      for (int i = p.first; i < p.second; i++) {
        bool ok = false;
        // Evitam cautarea binara, deoarece este mai rapid 
        // sa folosim o iterare pentru un numar mic de pivoti.
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
      // Copiem continutul bucket-urilor locale in bucketurile globale.
      for (int i = 0; i <= pivots.size(); ++i) {
        locks[i]->lock();
        buckets[i].insert(buckets[i].end(), local_buckets[i].begin(), local_buckets[i].end());
        locks[i]->unlock();
      }
		}));
	}
	// Asteptam ca fiecare worker sa termine job-ul sau.
	for (auto& w : workers) {
		w.join();
	}
	// Sortam fiecare bucket si il punem inapoi in vectorul initial.
	vector<thread> sort_workers;
	int pos = 0;
  for (auto& bucket : buckets) {
		sort_workers.push_back(thread([&bucket, &numbers, pos]() {
		  // Sortam bucket-ul.
      merge_sort(bucket);
      // Copiem bucket-ul inapoi in vectorul original.
      copy(bucket.begin(), bucket.end(), numbers.begin() + pos);
    }));
    pos += bucket.size();
  }
	for (auto& w : sort_workers) {
		w.join();
	}
}
