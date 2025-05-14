// { dg-do run { target c++23 } }

#include <ranges>
#include <span>
#include <stack>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <vector>

template<typename Range, typename Cont>
constexpr void
do_test()
{
  // The stack's value_type.
  using V = typename Cont::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [](std::stack<V, Cont> l, std::span<T> r) {
    if (l.size() != r.size())
      return false;
    for (auto const& v : r | std::views::reverse) {
      if (v != l.top())
        return false;
      l.pop();
    }
    return true;
  };

  std::stack<V, Cont> s;
  s.push_range(Range(a, a+0));
  VERIFY( s.empty() );

  s.push_range(Range(a, a+4));
  VERIFY( eq(s, {a, 4}) );

  s.push_range(Range(a+4, a+9));
  VERIFY( eq(s, {a, 9}) );
}

template<typename T, typename Alloc = std::allocator<T>>
struct NoAppendRangeCont : std::deque<T, Alloc>
{
  template<typename R>
  void append_range(R&&) = delete;
};

template<typename Range>
void
do_test_c()
{
  do_test<Range, std::deque<int>>();
  do_test<Range, std::vector<int>>();
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
