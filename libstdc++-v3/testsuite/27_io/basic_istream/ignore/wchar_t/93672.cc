// { dg-do run }

#include <sstream>
#include <limits>
#include <climits>
#include <testsuite_hooks.h>

// PR 93672 was a bug in std::istream that never affected std::wistream.
// This test ensures that the bug doesn't get introduced to std::wistream.
void
test_pr93672()
{
  std::wstring str = L".x..x.";
  str[1] = (wchar_t)-2;
  str[4] = (wchar_t)-3;
  std::wistringstream in(str);

  // This should find the character even on platforms where wchar_t is signed,
  // because the delimiter is correctly converted to the stream's int_type.
  in.ignore(100, std::char_traits<wchar_t>::to_int_type((wchar_t)-2));
  VERIFY( in.gcount() == 2 );
  VERIFY( ! in.eof() );

  // This also works, because std::char_traits<wchar_t>::to_int_type(wc) is
  // equivalent to (int_type)wc so using to_int_type isn't needed.
  in.ignore(100, (wchar_t)-3);
  VERIFY( in.gcount() == 3 );
  VERIFY( ! in.eof() );
}

int main()
{
  test_pr93672();
}
