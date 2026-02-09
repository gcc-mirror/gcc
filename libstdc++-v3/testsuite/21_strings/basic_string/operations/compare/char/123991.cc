// { dg-do run { target c++17 } }

// Bug 123991 - std::string::compare crashes instead of throwing

#include <string>
#include <string_view>
#include <stdexcept>
#include <testsuite_hooks.h>

void
test_compare_3arg()
{
  std::string_view sv;
  std::string s;
  static_assert( ! noexcept(s.compare(0, 0, sv)) );
#ifdef __cpp_exceptions
  try
  {
    (void) s.compare(1, 0, sv);
    VERIFY(false);
  }
  catch (const std::out_of_range&)
  { }
#endif
}

void
test_compare_5arg()
{
  std::string_view sv;
  std::string s;
  static_assert( ! noexcept(s.compare(0, 0, sv, 0, 0)) );
#ifdef __cpp_exceptions
  try
  {
    (void) s.compare(1, 0, sv, 0, 0);
    VERIFY(false);
  }
  catch (const std::out_of_range&)
  { }

  try
  {
    (void) s.compare(0, 0, sv, 1, 0);
    VERIFY(false);
  }
  catch (const std::out_of_range&)
  { }
#endif
}

int main()
{
  test_compare_3arg();
  test_compare_5arg();
}
