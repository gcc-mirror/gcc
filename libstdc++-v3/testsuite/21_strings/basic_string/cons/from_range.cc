// { dg-do run { target c++23 } }

#include <string>

#if __cpp_lib_containers_ranges != 202202L
# error "Feature-test macro __cpp_lib_containers_ranges has wrong value in <string>"
#endif

#include <span>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_allocator.h>

void
test_deduction_guide(char* p)
{
  __gnu_test::test_input_range<char> r(nullptr, nullptr);
  std::basic_string v(std::from_range, r);
  static_assert(std::is_same_v<decltype(v), std::string>);

  using Alloc = __gnu_test::SimpleAllocator<char>;
  Alloc alloc;
  std::basic_string v2(std::from_range, r, alloc);
  static_assert(std::is_same_v<decltype(v2), std::basic_string<char, std::char_traits<char>, Alloc>>);

  __gnu_test::test_input_range<wchar_t> wr(nullptr, nullptr);
  std::basic_string w(std::from_range, wr);
  static_assert(std::is_same_v<decltype(w), std::wstring>);

  using WAlloc = __gnu_test::SimpleAllocator<wchar_t>;
  WAlloc walloc;
  std::basic_string w2(std::from_range, wr, walloc);
  static_assert(std::is_same_v<decltype(w2), std::basic_string<wchar_t, std::char_traits<wchar_t>, WAlloc>>);
}

template<typename Range, typename Alloc>
constexpr void
do_test(Alloc alloc)
{
  // The basic_string's value_type.
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

  std::basic_string<V, CT, Alloc> v0(std::from_range, Range(a, a+0));
  VERIFY( v0.empty() );
  VERIFY( v0.get_allocator() == Alloc() );

  std::basic_string<V, CT, Alloc> v4(std::from_range, Range(a, a+4));
  VERIFY( eq(v4, {a, 4}) );
  VERIFY( v4.get_allocator() == Alloc() );

  std::basic_string<V, CT, Alloc> v9(std::from_range, Range(a, a+9), alloc);
  VERIFY( eq(v9, {a, 9}) );
  VERIFY( v9.get_allocator() == alloc );

  std::basic_string<V, CT, Alloc> v20(std::from_range, Range(a, a+20), alloc);
  VERIFY( eq(v20, {a, 20}) );
  VERIFY( v20.get_allocator() == alloc );
}

template<typename Range>
void
do_test_a()
{
  do_test<Range>(std::allocator<char>());
  do_test<Range>(__gnu_test::uneq_allocator<char>(42));
  do_test<Range>(std::allocator<wchar_t>());
  do_test<Range>(__gnu_test::uneq_allocator<wchar_t>(42));
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
  do_test<rvalue_input_range>(std::allocator<char>());

  return true;
}

constexpr bool
test_constexpr()
{
#if _GLIBCXX_USE_CXX11_ABI
  // XXX: this doesn't test the non-forward_range code paths are constexpr.
  do_test<std::string_view>(std::allocator<char>());
#endif // _GLIBCXX_USE_CXX11_ABI
  return true;
}

int main()
{
  test_ranges();
  static_assert( test_constexpr() );
}
