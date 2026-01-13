// { dg-do run  }

#undef _GLIBCXX_CONCEPT_CHECKS

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <utility>

using __gnu_test::test_container;
using __gnu_test::proxy_random_access_iterator_wrapper;

typedef std::pair<int, int> V;
typedef test_container<V, proxy_random_access_iterator_wrapper> Container;

struct Deduced
{
  template<typename A, typename B>
  bool operator()(const std::pair<A, B>& lhs,
		  const std::pair<A, B>& rhs) const
  { return lhs.first < rhs.first; }
};

struct Conversion
{
  struct First
  {
    First(const V& p) : v(p.first)
    {}

    int v;
  };

  bool operator()(First lhs, First rhs) const
  { return lhs.v < rhs.v; }
};

template<typename Cmp>
void
test01(Cmp cmp)
{
  V s1[] = { V(0, 10), V(8, 18), V(6, 16), V(7, 17), V(9, 19),
	     V(5, 15), V(4, 14), V(3, 13), V(2, 12), V(1, 11) };
  const int N = sizeof(s1) / sizeof(V);
  Container con(s1, s1 + N);

  std::make_heap(con.begin(), con.end() - std::ptrdiff_t(2), cmp);
  std::push_heap(con.begin(), con.end() - std::ptrdiff_t(1), cmp);
  std::push_heap(con.begin(), con.end(), cmp);
#if __cplusplus >= 201103L 
  (void)std::is_heap_until(con.begin(), con.end(), cmp);
#endif  
  std::sort_heap(con.begin(), con.end(), cmp);
  for(int i = 0; i < N; ++i)
  {
    VERIFY( s1[i].first == i );
    VERIFY( s1[i].second == 10 + i );
  }
}

int
main()
{
  test01(Deduced());
  test01(Conversion());
  return 0;
}
