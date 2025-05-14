// { dg-do run { target c++23 } }

#include <algorithm>
#include <queue>
#include <ranges>
#include <span>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <vector>

struct Gt {
  template<typename T, typename U>
  bool operator()(T const& l, U const & r) {
    return l > r;
  }
};

template<typename Range, typename Cont, typename Cmp = std::less<int>>
constexpr void
do_test(Cmp cmp = Cmp())
{
  // The queue's value_type.
  using V = typename Cont::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [&](std::priority_queue<V, Cont, Cmp> l, std::span<T> r) {
    if (l.size() != r.size())
      return false;

    std::vector<T> s(r.begin(), r.end());
    std::ranges::sort(s, cmp);
    for (auto const& v : s | std::views::reverse) {
      if (v != l.top())
        return false;
      l.pop();
    }
    return true;
  };

  std::priority_queue<V, Cont, Cmp> pq(std::from_range, Range(a, a+0));
  pq.push_range(Range(a, a+0));
  VERIFY( pq.empty() );

  pq.push_range(Range(a, a+4));
  VERIFY( eq(pq, {a, 4}) );

  pq.push_range(Range(a+4, a+9));
  VERIFY( eq(pq, {a, 9}) );
}

template<typename T, typename Alloc = std::allocator<T>>
struct NoAppendRangeCont : std::vector<T, Alloc>
{
  template<typename R>
  void append_range(R&&) = delete;
};

template<typename Range>
void
do_test_c()
{
  do_test<Range, std::vector<int>>();
  do_test<Range, std::vector<int>, Gt>();
  do_test<Range, std::deque<int>>();
  do_test<Range, NoAppendRangeCont<int>>();
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_c<test_forward_range<int>>();
  do_test_c<test_range_nocopy<int, input_iterator_wrapper_nocopy>>();
  do_test_c<test_forward_range<short>>();

  return true;
}

int main()
{
  test_ranges();
}
