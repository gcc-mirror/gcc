// { dg-additional-options "-lstdc++exp" { target { *-*-mingw* } } }
// { dg-do run { target c++23 } }
// { dg-require-fileio "" }

#include <ostream>
#include <spanstream>
#include <string_view>
#include <iostream>
#include <iomanip>
#include <testsuite_hooks.h>

void
test_println_blank_ostream()
{
  char buf[4];
  std::spanstream os(buf);
  std::println(os);
  std::string_view txt(os.span());
  VERIFY( txt == "\n" );
}

void
test_errors()
{
  // Failure to generate output is reported by setting badbit.
  std::stringstream in(std::ios::in);
  std::println(in); // No exception here.
  VERIFY(in.bad());
#ifdef __cpp_exceptions
  in.clear();
  in.exceptions(std::ios::badbit);
  try
  {
    std::println(in); // Should throw now.
    VERIFY(false);
  }
  catch (const std::ios::failure&)
  {
  }
#endif
}

int main()
{
  test_println_blank_ostream();
  test_errors();
}
