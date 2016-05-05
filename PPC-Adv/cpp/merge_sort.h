#include <iostream>
#include <vector>

using namespace std;

void mergesort(vector<int> & v, const int& left, const int& right) {
  if (left == right) {
    return;
  }
  int mid = left + (right - left)/2;
  mergesort(v, left, mid);
  mergesort(v, mid + 1, right);
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
 
void merge_sort(vector<int> &v) {
  mergesort(v, 0, v.size() - 1);
}
