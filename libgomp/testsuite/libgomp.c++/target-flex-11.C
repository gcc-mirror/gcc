/* Check constructors/destructors are called in containers.  */

#include <vector>
#include <deque>
#include <list>
#include <set>
#include <map>
#include <utility>
#if __cplusplus >= 201103L
#include <array>
#include <forward_list>
#include <unordered_set>
#include <unordered_map>
#endif

#include "target-flex-common.h"

struct indirect_counter
{
  typedef int counter_value_type;
  counter_value_type *_count_ptr;

  indirect_counter(counter_value_type *count_ptr) BL_NOEXCEPT : _count_ptr(count_ptr) {
    ++(*_count_ptr);
  }
  indirect_counter(const indirect_counter& other) BL_NOEXCEPT : _count_ptr(other._count_ptr) {
    ++(*_count_ptr);
  }
  /* Don't declare a move constructor, we want to copy no matter what.  */
  ~indirect_counter() {
    --(*_count_ptr);
  }
};

bool operator==(indirect_counter const& lhs, indirect_counter const& rhs) BL_NOEXCEPT
  { return lhs._count_ptr == rhs._count_ptr; }
bool operator<(indirect_counter const& lhs, indirect_counter const& rhs) BL_NOEXCEPT
  { return lhs._count_ptr < rhs._count_ptr; }

#if __cplusplus >= 201103L
template<>
struct std::hash<indirect_counter>
{
  std::size_t operator()(const indirect_counter& ic) const noexcept
    { return std::hash<indirect_counter::counter_value_type *>{}(ic._count_ptr); }
};
#endif

/* Not a container, just a sanity check really.  */
bool automatic_lifetime_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	indirect_counter c = indirect_counter(&counter);
	indirect_counter(static_cast<int*>(&counter));
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool vector_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	std::vector<indirect_counter> vec(42, indirect_counter(&counter));
	VERIFY (counter == 42);
	vec.resize(32, indirect_counter(&counter));
	VERIFY (counter == 32);
	vec.push_back(indirect_counter(&counter));
	VERIFY (counter == 33);
	vec.pop_back();
	VERIFY (counter == 32);
	vec.pop_back();
	VERIFY (counter == 31);
	vec.resize(100, indirect_counter(&counter));
	VERIFY (counter == 100);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool deque_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	std::deque<indirect_counter> vec(42, indirect_counter(&counter));
	VERIFY (counter == 42);
	vec.resize(32, indirect_counter(&counter));
	VERIFY (counter == 32);
	vec.push_back(indirect_counter(&counter));
	VERIFY (counter == 33);
	vec.pop_back();
	VERIFY (counter == 32);
	vec.pop_back();
	VERIFY (counter == 31);
	vec.resize(100, indirect_counter(&counter));
	VERIFY (counter == 100);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool list_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	std::list<indirect_counter> list(42, indirect_counter(&counter));
	VERIFY (counter == 42);
	list.resize(32, indirect_counter(&counter));
	VERIFY (counter == 32);
	list.push_back(indirect_counter(&counter));
	VERIFY (counter == 33);
	list.pop_back();
	VERIFY (counter == 32);
	list.pop_back();
	VERIFY (counter == 31);
	list.resize(100, indirect_counter(&counter));
	VERIFY (counter == 100);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool map_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	std::map<int, indirect_counter> map;
	map.insert(std::make_pair(1, indirect_counter(&counter)));
	VERIFY (counter == 1);
	map.insert(std::make_pair(1, indirect_counter(&counter)));
	VERIFY (counter == 1);
	map.insert(std::make_pair(2, indirect_counter(&counter)));
	VERIFY (counter == 2);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool set_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter0 = 0;
      int counter1 = 0;
      {
	std::set<indirect_counter> set;
	set.insert(indirect_counter(&counter0));
	VERIFY (counter0 == 1);
	set.insert(indirect_counter(&counter0));
	VERIFY (counter0 == 1);
	set.insert(indirect_counter(&counter1));
	VERIFY (counter0 == 1 && counter1 == 1);
      }
      VERIFY (counter0 == 0 && counter1 == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool multimap_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	std::multimap<int, indirect_counter> multimap;
	multimap.insert(std::make_pair(1, indirect_counter(&counter)));
	VERIFY (counter == 1);
	multimap.insert(std::make_pair(1, indirect_counter(&counter)));
	VERIFY (counter == 2);
	multimap.insert(std::make_pair(2, indirect_counter(&counter)));
	VERIFY (counter == 3);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool multiset_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter0 = 0;
      int counter1 = 0;
      {
	std::multiset<indirect_counter> multiset;
	multiset.insert(indirect_counter(&counter0));
	VERIFY (counter0 == 1);
	multiset.insert(indirect_counter(&counter0));
	VERIFY (counter0 == 2);
	multiset.insert(indirect_counter(&counter1));
	VERIFY (counter0 == 2 && counter1 == 1);
      }
      VERIFY (counter0 == 0 && counter1 == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

#if __cplusplus >= 201103L

bool array_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	indirect_counter ic(&counter);
	std::array<indirect_counter, 10> array{ic, ic, ic, ic, ic,
					       ic, ic, ic, ic, ic};
	VERIFY (counter == 11);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool forward_list_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	std::forward_list<indirect_counter> forward_list(42, indirect_counter(&counter));
	VERIFY (counter == 42);
	forward_list.resize(32, indirect_counter(&counter));
	VERIFY (counter == 32);
	forward_list.push_front(indirect_counter(&counter));
	VERIFY (counter == 33);
	forward_list.pop_front();
	VERIFY (counter == 32);
	forward_list.pop_front();
	VERIFY (counter == 31);
	forward_list.resize(100, indirect_counter(&counter));
	VERIFY (counter == 100);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool unordered_map_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	std::unordered_map<int, indirect_counter> unordered_map;
	unordered_map.insert({1, indirect_counter(&counter)});
	VERIFY (counter == 1);
	unordered_map.insert({1, indirect_counter(&counter)});
	VERIFY (counter == 1);
	unordered_map.insert({2, indirect_counter(&counter)});
	VERIFY (counter == 2);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool unordered_set_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter0 = 0;
      int counter1 = 0;
      {
	std::unordered_set<indirect_counter> unordered_set;
	unordered_set.insert(indirect_counter(&counter0));
	VERIFY (counter0 == 1);
	unordered_set.insert(indirect_counter(&counter0));
	VERIFY (counter0 == 1);
	unordered_set.insert(indirect_counter(&counter1));
	VERIFY (counter0 == 1 && counter1 == 1);
      }
      VERIFY (counter0 == 0 && counter1 == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool unordered_multimap_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter = 0;
      {
	std::unordered_multimap<int, indirect_counter> unordered_multimap;
	unordered_multimap.insert({1, indirect_counter(&counter)});
	VERIFY (counter == 1);
	unordered_multimap.insert({1, indirect_counter(&counter)});
	VERIFY (counter == 2);
	unordered_multimap.insert({2, indirect_counter(&counter)});
	VERIFY (counter == 3);
      }
      VERIFY (counter == 0);
      end:
      ok = inner_ok;
    }
  return ok;
}

bool unordered_multiset_test()
{
  bool ok;
  #pragma omp target map(from: ok)
    {
      bool inner_ok = true;
      int counter0 = 0;
      int counter1 = 0;
      {
	std::unordered_multiset<indirect_counter> unordered_multiset;
	unordered_multiset.insert(indirect_counter(&counter0));
	VERIFY (counter0 == 1);
	unordered_multiset.insert(indirect_counter(&counter0));
	VERIFY (counter0 == 2);
	unordered_multiset.insert(indirect_counter(&counter1));
	VERIFY (counter0 == 2 && counter1 == 1);
      }
      VERIFY (counter0 == 0 && counter1 == 0);
      end:
      ok = inner_ok;
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
  const bool auto_res               = automatic_lifetime_test();
  const bool vec_res                = vector_test();
  const bool deque_res              = deque_test();
  const bool list_res               = list_test();
  const bool map_res                = map_test();
  const bool set_res                = set_test();
  const bool multimap_res           = multimap_test();
  const bool multiset_res           = multiset_test();
  const bool array_res              = array_test();
  const bool forward_list_res       = forward_list_test();
  const bool unordered_map_res      = unordered_map_test();
  const bool unordered_set_res      = unordered_set_test();
  const bool unordered_multimap_res = unordered_multimap_test();
  const bool unordered_multiset_res = unordered_multiset_test();
  std::printf("sanity check      : %s\n", auto_res               ? "PASS" : "FAIL");
  std::printf("vector            : %s\n", vec_res                ? "PASS" : "FAIL");
  std::printf("deque             : %s\n", deque_res              ? "PASS" : "FAIL");
  std::printf("list              : %s\n", list_res               ? "PASS" : "FAIL");
  std::printf("map               : %s\n", map_res                ? "PASS" : "FAIL");
  std::printf("set               : %s\n", set_res                ? "PASS" : "FAIL");
  std::printf("multimap          : %s\n", multimap_res           ? "PASS" : "FAIL");
  std::printf("multiset          : %s\n", multiset_res           ? "PASS" : "FAIL");
  std::printf("array             : %s\n", array_res              ? "PASS" : "FAIL");
  std::printf("forward_list      : %s\n", forward_list_res       ? "PASS" : "FAIL");
  std::printf("unordered_map     : %s\n", unordered_map_res      ? "PASS" : "FAIL");
  std::printf("unordered_set     : %s\n", unordered_set_res      ? "PASS" : "FAIL");
  std::printf("unordered_multimap: %s\n", unordered_multimap_res ? "PASS" : "FAIL");
  std::printf("unordered_multiset: %s\n", unordered_multiset_res ? "PASS" : "FAIL");
  const bool ok = auto_res
		  && vec_res
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
