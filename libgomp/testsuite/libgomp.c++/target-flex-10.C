/* Basic container usage.  */

#include <vector>
#include <deque>
#include <list>
#include <set>
#include <map>
#if __cplusplus >= 201103L
#include <array>
#include <forward_list>
#include <unordered_set>
#include <unordered_map>
#endif

bool vector_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::vector<int> vector;
      ok = vector.empty();
    }
  return ok;
}

bool deque_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::deque<int> deque;
      ok = deque.empty();
    }
  return ok;
}

bool list_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::list<int> list;
      ok = list.empty();
    }
  return ok;
}

bool map_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::map<int, int> map;
      ok = map.empty();
    }
  return ok;
}

bool set_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::set<int> set;
      ok = set.empty();
    }
  return ok;
}

bool multimap_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::multimap<int, int> multimap;
      ok = multimap.empty();
    }
  return ok;
}

bool multiset_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::multiset<int, int> multiset;
      ok = multiset.empty();
    }
  return ok;
}

#if __cplusplus >= 201103L

bool array_test()
{
  static constexpr std::size_t array_size = 42;
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::array<int, array_size> array{};
      ok = array[0] == 0
	   && array[array_size - 1] == 0;
    }
  return ok;
}

bool forward_list_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::forward_list<int> forward_list;
      ok = forward_list.empty();
    }
  return ok;
}

bool unordered_map_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::unordered_map<int, int> unordered_map;
      ok = unordered_map.empty();
    }
  return ok;
}

bool unordered_set_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      std::unordered_set<int> unordered_set;
      ok = unordered_set.empty();
    }
  return ok;
}

bool unordered_multimap_test()
{

  bool ok;
  #pragma omp target map(from: ok)
    {
      std::unordered_multimap<int, int> unordered_multimap;
      ok = unordered_multimap.empty();
    }
  return ok;
}

bool unordered_multiset_test()
{

  bool ok;
  #pragma omp target map(from: ok)
    {
      std::unordered_multiset<int> unordered_multiset;
      ok = unordered_multiset.empty();
    }
  return ok;
}

#else
bool array_test() { return true; }
bool forward_list_test() { return true; }
bool unordered_map_test() { return true; }
bool unordered_set_test() { return true; }
bool unordered_multimap_test() { return true; }
bool unordered_multiset_test() { return true; }
#endif

int main()
{
  const bool vec_res                = vector_test();
  __builtin_printf("vector            : %s\n", vec_res                ? "PASS" : "FAIL");
  const bool deque_res              = deque_test();
  __builtin_printf("deque             : %s\n", deque_res              ? "PASS" : "FAIL");
  const bool list_res               = list_test();
  __builtin_printf("list              : %s\n", list_res               ? "PASS" : "FAIL");
  const bool map_res                = map_test();
  __builtin_printf("map               : %s\n", map_res                ? "PASS" : "FAIL");
  const bool set_res                = set_test();
  __builtin_printf("set               : %s\n", set_res                ? "PASS" : "FAIL");
  const bool multimap_res           = multimap_test();
  __builtin_printf("multimap          : %s\n", multimap_res           ? "PASS" : "FAIL");
  const bool multiset_res           = multiset_test();
  __builtin_printf("multiset          : %s\n", multiset_res           ? "PASS" : "FAIL");
  const bool array_res              = array_test();
  __builtin_printf("array             : %s\n", array_res              ? "PASS" : "FAIL");
  const bool forward_list_res       = forward_list_test();
  __builtin_printf("forward_list      : %s\n", forward_list_res       ? "PASS" : "FAIL");
  const bool unordered_map_res      = unordered_map_test();
  __builtin_printf("unordered_map     : %s\n", unordered_map_res      ? "PASS" : "FAIL");
  const bool unordered_set_res      = unordered_set_test();
  __builtin_printf("unordered_set     : %s\n", unordered_set_res      ? "PASS" : "FAIL");
  const bool unordered_multimap_res = unordered_multimap_test();
  __builtin_printf("unordered_multimap: %s\n", unordered_multimap_res ? "PASS" : "FAIL");
  const bool unordered_multiset_res = unordered_multiset_test();
  __builtin_printf("unordered_multiset: %s\n", unordered_multiset_res ? "PASS" : "FAIL");
  const bool ok = vec_res
		  && deque_res
		  && list_res
		  && map_res
		  && set_res
		  && multimap_res
		  && multiset_res
		  && array_res
		  && forward_list_res
		  && unordered_map_res
		  && unordered_set_res
		  && unordered_multimap_res
		  && unordered_multiset_res;
  return ok ? 0 : 1;
}
