// { dg-do run }

#include <algorithm>
#include <cstddef> // std::byte
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

// PR libstdc++/88545 made std::find use memchr as an optimization.
// This test verifies that it didn't change any semantics.

template<typename C>
void
test_char()
{
  const C a[] = { (C)'a', (C)'b', (C)'c', (C)'d' };
  const C* end = a + sizeof(a);
  const C* res = std::find(a, end, a[0]);
  VERIFY( res == a );
  res = std::find(a, end, a[2]);
  VERIFY( res == a+2 );
  res = std::find(a, end, a[0] + 256);
  VERIFY( res == end );
  res = std::find(a, end, a[0] - 256);
  VERIFY( res == end );
  res = std::find(a, end, 256);
  VERIFY( res == end );

#ifdef __cpp_lib_ranges
  res = std::ranges::find(a, a[0]);
  VERIFY( res == a );
  res = std::ranges::find(a, a[2]);
  VERIFY( res == a+2 );
  res = std::ranges::find(a, a[0] + 256);
  VERIFY( res == end );
  res = std::ranges::find(a, a[0] - 256);
  VERIFY( res == end );
  res = std::ranges::find(a, 256);
  VERIFY( res == end );
#endif
}

// Trivial type of size 1, with custom equality.
struct S {
  bool operator==(const S&) const { return true; };
  char c;
};

// Trivial type of size 1, with custom equality.
enum E
#if __cplusplus >= 201103L
: unsigned char
#endif
{ e1 = 1, e255 = 255 };

bool operator==(E l, E r) { return (l % 3) == (r % 3); }

struct X { char c; };
bool operator==(X, char) { return false; }
bool operator==(char, X) { return false; }

bool operator==(E, char) { return false; }
bool operator==(char, E) { return false; }

void
test_non_characters()
{
  S s[3] = { {'a'}, {'b'}, {'c'} };
  S sx = {'x'};
  S* sres = std::find(s, s+3, sx);
  VERIFY( sres == s ); // memchr optimization would not find a match

  E e[3] = { E(1), E(2), E(3) };
  E* eres = std::find(e, e+3, E(4));
  VERIFY( eres == e ); // memchr optimization would not find a match

  char x[1] = { 'x' };
  X xx = { 'x' };
  char* xres = std::find(x, x+1, xx);
  VERIFY( xres == x+1 ); // memchr optimization would find a match
  xres = std::find(x, x+1, E('x'));
  VERIFY( xres == x+1 ); // memchr optimization would find a match

#ifdef __cpp_lib_byte
  std::byte b[] = { std::byte{0}, std::byte{1}, std::byte{2}, std::byte{3} };
  std::byte* bres = std::find(b, b+4, std::byte{4});
  VERIFY( bres == b+4 );
  bres = std::find(b, b+2, std::byte{3});
  VERIFY( bres == b+2 );
  bres = std::find(b, b+3, std::byte{3});
  VERIFY( bres == b+3 );
#endif

#ifdef __cpp_lib_ranges
  sres = std::ranges::find(s, sx);
  VERIFY( sres == s );

  eres = std::ranges::find(e, e+3, E(4));
  VERIFY( eres == e );

  // std::equality_comparable_with<X, char> is not satisfied, so can't do
  // std::ranges::find(x, xx)

  bres = std::ranges::find(b, std::byte{4});
  VERIFY( bres == b+4 );
  bres = std::ranges::find(b, b+2, std::byte{3});
  VERIFY( bres == b+2 );
  bres = std::ranges::find(b, std::byte{3});
  VERIFY( bres == b+3 );

  xres = std::find(x, x+1, xx);
  VERIFY( xres == std::ranges::end(x) );
  xres = std::find(x, x+1, E('x'));
  VERIFY( xres == std::ranges::end(x) );
#endif
}

#if __cpp_lib_ranges
void
test_pr115799c0(__gnu_test::test_contiguous_range<char> r)
{
  // Non-common range with integer-class type as difference_type.
  (void) std::ranges::find(r, 'a');
}
#endif

void
test_pr115799c2(__gnu_test::input_iterator_wrapper<char> i)
{
  // Non-contiguous range of character type.
  (void) std::find(i, i, 'a');
}

int main()
{
  test_char<char>();
  test_char<signed char>();
  test_char<unsigned char>();
  test_non_characters();

#if __cpp_lib_constexpr_algorithms
  static_assert( [] {
    char c[] = "abcd";
    return std::find(c, c+4, 'b') == c+1;
  }() );
#ifdef __cpp_lib_ranges
  static_assert( [] {
    char c[] = "abcd";
    return std::ranges::find(c, 'b') == c+1;
  }() );
#endif
#endif
}
