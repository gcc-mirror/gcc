// { dg-do run { target c++23 } }

#include <span>
#include <string>
#include <testsuite_allocator.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

template<typename Range, typename Alloc>
constexpr void
do_test()
{
  // The vector's value_type.
  using V = typename std::allocator_traits<Alloc>::value_type;
  using CT = std::char_traits<V>;

  // The range's value_type.
  using T = std::ranges::range_value_t<Range>;
  T a[]{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'};

  auto eq = [](const std::basic_string<V, CT, Alloc>& l, std::span<T> r) {
    if (l.size() != r.size())
      return false;
    for (auto i = 0u; i < l.size(); ++i)
      if (l[i] != r[i])
	return false;
    return true;
  };

  std::basic_string<V, CT, Alloc> v;
  v.replace_with_range(v.end(), v.end(), Range(a, a));
  VERIFY( v.empty() );
  v.replace_with_range(v.end(), v.end(), Range(a, a+4));
  VERIFY( eq(v, {a, 4}) );
  v.replace_with_range(v.end(), v.end(), Range(a+4, a+9));
  VERIFY( eq(v, {a, 9}) );
  std::basic_string<V, CT, Alloc> s = v;
  v.replace_with_range(v.end(), v.end(), Range(a+9, a+20));
  VERIFY( eq(v, {a, 20}) );

  v.replace_with_range(v.begin()+10, v.begin()+20, Range(a+5, a+10));
  VERIFY( v.size() == 15 );
  v.replace_with_range(v.begin(), v.begin()+10, Range(a, a+5));
  VERIFY( eq(v, {a, 10}) );

  s = v;
  v.replace_with_range(v.begin(), v.begin()+4, Range(a, a+8));
  VERIFY( v.size() == 14 );
  v.replace_with_range(v.begin()+8, v.begin()+12, Range(a+8, a+16));
  VERIFY( v.size() == 18 );
  v.replace_with_range(v.begin()+16, v.begin()+18, Range(a+16, a+20));
  VERIFY( eq(v, {a, 20}) );
}

template<typename Range>
void
do_test_a()
{
  do_test<Range, std::allocator<char>>();
  do_test<Range, __gnu_test::SimpleAllocator<char>>();
  do_test<Range, std::allocator<wchar_t>>();
  do_test<Range, __gnu_test::SimpleAllocator<wchar_t>>();
}

bool
test_ranges()
{
  using namespace __gnu_test;

  do_test_a<test_forward_range<char>>();
  do_test_a<test_forward_sized_range<char>>();
  do_test_a<test_sized_range_sized_sent<char, forward_iterator_wrapper>>();

  do_test_a<test_input_range<char>>();
  do_test_a<test_input_sized_range<char>>();
  do_test_a<test_sized_range_sized_sent<char, input_iterator_wrapper>>();

  do_test_a<test_range<char, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range<char, input_iterator_wrapper_nocopy>>();
  do_test_a<test_sized_range_sized_sent<char, input_iterator_wrapper_nocopy>>();

  // Not lvalue-convertible to char
  struct C {
    C(char v) : val(v) { }
    operator char() && { return val; }
    bool operator==(char b) const { return b == val; }
    char val;
  };
  using rvalue_input_range = test_range<C, input_iterator_wrapper_rval>;
  do_test<rvalue_input_range, std::allocator<char>>();

  return true;
}

void
test_overlapping()
{
  std::string const s = "1234abcd";

  std::string c = s;
  c.replace_with_range(c.end(), c.end(), std::string_view(c));
  VERIFY( c == "1234abcd1234abcd" );

  c = s;
  c.replace_with_range(c.begin(), c.begin()+4, std::string_view(c).substr(4, 4));
  VERIFY( c == "abcdabcd" );

  c = s;
  c.replace_with_range(c.begin()+2, c.begin()+4, std::string_view(c).substr(0, 4));
  VERIFY( c == "121234abcd" );

  c = s;
  c.replace_with_range(c.begin()+2, c.begin()+2, std::string_view(c).substr(0, 4));
  VERIFY( c == "12123434abcd" );
}

constexpr bool
test_constexpr()
{
#if _GLIBCXX_USE_CXX11_ABI
  // XXX: this doesn't test the non-forward_range code paths are constexpr.
  do_test<std::string_view, std::allocator<char>>();
#endif // _GLIBCXX_USE_CXX11_ABI
  return true;
}

int main()
{
  test_ranges();
  test_overlapping();
  static_assert( test_constexpr() );
}
