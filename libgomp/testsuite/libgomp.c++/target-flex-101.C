/* { dg-additional-options -std=c++23 } */

/* C++23 container adaptors in target region.
   Severely needs additional tests.  */

#include <cstdio>
#include <utility>
#include <version>

#if __cpp_lib_flat_map >= 202207L
#define ENABLE_FLAT_MAP 1
#endif
#if __cpp_lib_flat_set >= 202207L
#define ENABLE_FLAT_SET 1
#endif

#ifdef ENABLE_FLAT_MAP
#include <flat_map>
#endif
#ifdef ENABLE_FLAT_SET
#include <flat_set>
#endif

#include "target-flex-common.h"

#ifdef ENABLE_FLAT_MAP
template<typename K, typename V, typename std::size_t Size>
bool test_flat_map(std::pair<K, V> (&arr)[Size])
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	using flat_map_type = std::flat_map<K, V>;
	flat_map_type map = {arr, arr + Size};

	VERIFY (!map.empty());
	for (const auto& element : arr)
	  VERIFY (map.contains(element.first));
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

template<typename K, typename V, typename std::size_t Size>
bool test_flat_multimap(std::pair<K, V> (&arr)[Size])
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	using flat_map_type = std::flat_map<K, V>;
	flat_map_type map = {arr, arr + Size};

	VERIFY (!map.empty());
	for (const auto& element : arr)
	  VERIFY (map.contains(element.first));
      }
      end:
      ok = inner_ok;
    }
  return ok;
}
#else
template<typename K, typename V, typename std::size_t Size>
bool test_flat_map(std::pair<K, V> (&arr)[Size]) { return true; }

template<typename K, typename V, typename std::size_t Size>
bool test_flat_multimap(std::pair<K, V> (&arr)[Size]) { return true; }
#endif

#ifdef ENABLE_FLAT_SET
template<typename T, typename std::size_t Size>
bool test_flat_set(T (&arr)[Size])
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	using flat_set_type = std::flat_set<T>;
	flat_set_type set = {arr, arr + Size};

	VERIFY (!set.empty());
	for (const auto& element : arr)
	  VERIFY (set.contains(element));
      }
      end:
      ok = inner_ok;
    }
  return ok;
}

template<typename T, typename std::size_t Size>
bool test_flat_multiset(T (&arr)[Size])
{
  bool ok;
  #pragma omp target map(from: ok) map(to: arr[:Size])
    {
      bool inner_ok = true;
      {
	using flat_multiset_type = std::flat_multiset<T>;
	flat_multiset_type multiset = {arr, arr + Size};

	VERIFY (!multiset.empty());
	for (const auto& element : arr)
	  VERIFY (multiset.contains(element));
      }
      end:
      ok = inner_ok;
    }
  return ok;
}
#else
template<typename T, typename std::size_t Size>
bool test_flat_set(T (&arr)[Size]) { return true; }

template<typename T, typename std::size_t Size>
bool test_flat_multiset(T (&arr)[Size]) { return true; }
#endif

int main()
{
  int arr[10] = {0,1,2,3,4,5,6,7,8,9};
  std::pair<int, int> pairs[10] = {{ 1,  2}, { 2,  4}, { 3,  6}, { 4,  8}, { 5, 10},
				   { 6, 12}, { 7, 14}, { 8, 16}, { 9, 18}, {10, 20}};

  return test_flat_set(arr)
	 && test_flat_multiset(arr)
	 && test_flat_map(pairs)
	 && test_flat_multimap(pairs) ? 0 : 1;
}
