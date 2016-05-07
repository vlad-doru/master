#include <iostream>
#include <vector>
#include <thread>

#include <future>
#include <tbb/parallel_invoke.h>

using namespace std;

void parallel_mergesort(vector<int> & v, const int& left, const int& right) {
  if (left == right) {
    return;
  }
  int mid = left + (right - left)/2;
  // Sort the parts in parallel.
	vector<thread> sort_workers;
  tbb::parallel_invoke(
    [&v, left, mid]() {
      parallel_mergesort(v, left, mid);
    },
	  [&v, mid, right]() {
      parallel_mergesort(v, mid + 1, right);
    }
  );
  // Join the threads.
	for (auto& w : sort_workers) {
		w.join();
	}
	// Do the merge.
  int i = left;
  int j = mid + 1;
	int k = 0;
  vector<int> merged;
	merged.resize(right - left + 1);
  while (i <= mid && j <=right) {
    if (v[i] < v[j]) {
			merged[k++] = v[i++];
    } else {
			merged[k++] = v[j++];
    }
  }
  while (i <= mid) {
		merged[k++] = v[i++];
  }
  while (j <= right) {
		merged[k++] = v[j++];
  }
	std::copy(merged.begin(), merged.end(), v.begin() + left);
}
 
void parallel_merge_sort(vector<int> &v) {
  parallel_mergesort(v, 0, v.size() - 1);
}
