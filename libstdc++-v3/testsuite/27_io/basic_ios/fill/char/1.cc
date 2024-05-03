// { dg-do run }

#include <ios>
#include <locale>
#include <streambuf>
#include <testsuite_hooks.h>

typedef char C;

struct tabby_mctype : std::ctype<C>
{
  C do_widen(char c) const { return c == ' ' ? '\t' : c; }

  const char*
  do_widen(const char* lo, const char* hi, C* to) const
  {
    while (lo != hi)
      *to++ = do_widen(*lo++);
    return hi;
  }
};

void
test01()
{
  std::basic_ios<C> out(0);
  std::locale loc(std::locale(), new tabby_mctype);
  out.imbue(loc);
  VERIFY( out.fill() == ' ' ); // Imbuing a new locale doesn't affect fill().
  out.fill('*');
  VERIFY( out.fill() == '*' ); // This will be cached now.
  out.imbue(std::locale());
  VERIFY( out.fill() == '*' ); // Imbuing a new locale doesn't affect fill().
}

void
test02()
{
  std::locale loc(std::locale(), new tabby_mctype);
  std::locale::global(loc);
  std::basic_ios<C> out(0);
  VERIFY( out.fill() == '\t' );
  out.imbue(std::locale::classic());
  VERIFY( out.fill() == '\t' ); // Imbuing a new locale doesn't affect fill().
  out.fill('*');
  VERIFY( out.fill() == '*' );  // This will be cached now.
  out.imbue(std::locale());
  VERIFY( out.fill() == '*' );  // Imbuing a new locale doesn't affect fill().
}

void
test03()
{
  // This function tests a libstdc++ extension: if no ctype<char_type> facet
  // is present when the stream is initialized, a fill character will not be
  // cached. Calling fill() will obtain a fill character from the locale each
  // time it's called.
  typedef signed char C2;
  std::basic_ios<C2> out(0);
#if __cpp_exceptions
  try {
    (void) out.fill(); // No ctype<signed char> in the locale.
    VERIFY( false );
  } catch (...) {
  }
#endif
  out.fill('*');
  VERIFY( out.fill() == '*' ); // This will be cached now.
  out.imbue(std::locale());
  VERIFY( out.fill() == '*' ); // Imbuing a new locale doesn't affect fill().
}

int main()
{
  test01();
  test02();
  test03();
}
