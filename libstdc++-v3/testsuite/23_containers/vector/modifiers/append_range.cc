// { dg-do run { target c++23 } }

#include <vector>
#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

template<typename Range, typename Alloc>
constexpr void
do_test()
{
  // The vector's value_type.
  using V = typename std::allocator_traits<Alloc>::value_type;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{1,2,3,4,5,6,7,8,9};

  auto eq = [](const std::vector<V, Alloc>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;
    for (auto i = 0u; i < l.size(); ++i)
      if (l[i] != r[i])
	return false;
    return true;
  };

  Range r4(a, a+4);
  Range r5(a+4, a+9);

  std::vector<V, Alloc> v;
  v.append_range(r4);
  VERIFY( eq(v, {a, 4}) );
  v.append_range(r5); // larger than v.capacity()
  VERIFY( eq(v, a) );
  v.append_range(Range(a, a));
  VERIFY( eq(v, a) );
  v.clear();
  v.append_range(Range(a, a));
  VERIFY( v.empty() );
}

template<typename Range>
void
do_test_a()
{
  do_test<Range, std::allocator<int>>();
  do_test<Range, __gnu_test::SimpleAllocator<int>>();
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_a<test_forward_range<int>>();
  do_test_a<test_forward_sized_range<int>>();
  do_test_a<test_sized_range_sized_sent<int, forward_iterator_wrapper>>();

  do_test_a<test_input_range<int>>();
  do_test_a<test_input_sized_range<int>>();
  do_test_a<test_sized_range_sized_sent<int, input_iterator_wrapper>>();

  do_test_a<test_range<int, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range<int, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range_sized_sent<int, input_iterator_wrapper_nocopy>>();

  do_test_a<test_forward_range<short>>();
  do_test_a<test_input_range<short>>();

  // Not lvalue-convertible to int
  struct C {
    C(int v) : val(v) { }
    operator int() && { return val; }
    bool operator==(int b) const { return b == val; }
    int val;
  };
  using rvalue_input_range = test_range<C, input_iterator_wrapper_rval>;
  do_test<rvalue_input_range, std::allocator<int>>();

  return true;
}

void
test_overlapping()
{
  using __gnu_test::test_input_range;
  using __gnu_test::test_forward_range;

  struct X {
    unsigned* p;
    constexpr X(int i = 0) : p(new unsigned(i)) { }
    constexpr X(const X& m) : p(new unsigned(*m.p)) { }
    constexpr X(X&& m) noexcept : p(m.p) { m.p = nullptr; }
    constexpr ~X() { delete p; }
  };

  std::vector<X> vec;
  unsigned size = 5;
  vec.reserve(size);
  for (unsigned i = 0; i < size; ++i)
    vec.emplace_back(i);

  // Append an input range that overlaps with vec.
  {
    __gnu_test::test_input_range<X> r(vec.data(), vec.data() + size);
    vec.append_range(r);
    VERIFY( vec.size() == 2 * size );
    for (unsigned i = 0; i < size; ++i)
    {
      VERIFY( *vec[i].p == i );
      VERIFY( *vec[i+size].p == i );
    }
  }

  size = vec.size() - 2;
  vec.resize(size);
  for (unsigned i = 0; i < size; ++i)
    *vec[i].p = i;

  // Repeat with unused capacity in the vector.
  {
    __gnu_test::test_input_range<X> r(vec.data(), vec.data() + size);
    vec.append_range(r);
    VERIFY( vec.size() == 2 * size );
    for (unsigned i = 0; i < size; ++i)
    {
      VERIFY( *vec[i].p == i );
      VERIFY( *vec[i+size].p == i );
    }
  }

  size = vec.size() - 2;
  vec.resize(size);
  for (unsigned i = 0; i < size; ++i)
    *vec[i].p = i;

  // Repeat with input range that doesn't overlap full vector.
  {
    __gnu_test::test_input_range<X> r(vec.data() + 1, vec.data() + 4);
    vec.append_range(r);
    VERIFY( vec.size() == size + 3 );
    for (unsigned i = 0; i < size; ++i)
    {
      VERIFY( *vec[i].p == i );
      if (i < 3)
	VERIFY( *vec[i+size].p == i+1 );
    }
  }

  size = 5;
  vec.resize(size);
  for (unsigned i = 0; i < size; ++i)
    *vec[i].p = i;

  // Append a forward range that overlaps with vec.
  {
    __gnu_test::test_forward_range<X> r(vec.data(), vec.data() + size);
    vec.append_range(r);
    VERIFY( vec.size() == 2 * size );
    for (unsigned i = 0; i < size; ++i)
    {
      VERIFY( *vec[i].p == i );
      VERIFY( *vec[i+size].p == i );
    }
  }

  size = vec.size() - 2;
  vec.resize(size);
  for (unsigned i = 0; i < size; ++i)
    *vec[i].p = i;

  // Repeat with insufficient unused capacity in the vector.
  {
    __gnu_test::test_forward_range<X> r(vec.data(), vec.data() + size);
    vec.append_range(r);
    VERIFY( vec.size() == 2 * size );
    for (unsigned i = 0; i < size; ++i)
    {
      VERIFY( *vec[i].p == i );
      VERIFY( *vec[i+size].p == i );
    }
  }

  size = vec.size() / 2;
  vec.resize(size);

  // Repeat with sufficient unused capacity in the vector.
  {
    __gnu_test::test_forward_range<X> r(vec.data(), vec.data() + size);
    vec.append_range(r);
    VERIFY( vec.size() == 2 * size );
    for (unsigned i = 0; i < size; ++i)
    {
      VERIFY( *vec[i].p == i );
      VERIFY( *vec[i+size].p == i );
    }
  }
}

constexpr bool
test_constexpr()
{
  // XXX: this doesn't test the non-forward_range code paths are constexpr.
  do_test<std::span<short>, std::allocator<int>>();

  // Some basic tests for overlapping ranges in constant expressions.
  struct InputRange
  {
    struct Sent { const void* end; };

    struct Iter
    {
      using value_type = int;
      using difference_type = int;
      constexpr explicit Iter(int* p) : ptr(p) { }
      constexpr Iter& operator++() { ++ptr; return *this; }
      constexpr Iter operator++(int) { auto i = *this; ++ptr; return i; }
      constexpr int operator*() const { return *ptr; }
      constexpr bool operator==(const Iter&) const = default;
      constexpr bool operator==(const Sent& s) const { return ptr == s.end; }
      int* ptr;
    };

    Iter iter;
    Sent sent;

    constexpr InputRange(int* f, int* l) : iter{f}, sent{l} { }
    constexpr Iter begin() const { return iter; }
    constexpr Sent end() const { return sent; }
  };
  static_assert( std::ranges::input_range<InputRange> );
  static_assert( ! std::ranges::forward_range<InputRange> );

  std::vector<int> vec(5);

  // Test overlapping input ranges
  vec.resize(vec.capacity());
  vec.append_range(InputRange(vec.data(), vec.data() + 3)); // no capacity
  vec.reserve(vec.capacity() + 2);
  vec.append_range(InputRange(vec.data(), vec.data() + 4)); // some capacity
  vec.reserve(vec.capacity() + 6);
  vec.append_range(InputRange(vec.data(), vec.data() + 5)); // enough capacity

  // Test overlapping forward ranges
  vec.resize(vec.capacity());
  vec.append_range(std::span<int>(vec));               // no capacity
  vec.reserve(vec.size() + 2);
  vec.append_range(std::span<int>(vec).subspan(1, 4)); // some capacity
  vec.reserve(vec.size() + 6);
  vec.append_range(std::span<int>(vec).subspan(1, 5)); // enough capacity

  return true;
}

int main()
{
  test_ranges();
  test_overlapping();
  static_assert( test_constexpr() );
}
