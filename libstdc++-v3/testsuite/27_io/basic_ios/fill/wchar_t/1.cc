// { dg-do run }

#include <ios>
#include <locale>
#include <streambuf>
#include <testsuite_hooks.h>

typedef wchar_t C;

struct tabby_mctype : std::ctype<C>
{
  C do_widen(char c) const { return c == ' ' ? L'\t' : c; }

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
  VERIFY( out.fill() == L' ' ); // Imbuing a new locale doesn't affect fill().
  out.fill(L'*');
  VERIFY( out.fill() == L'*' ); // This will be cached now.
  out.imbue(std::locale());
  VERIFY( out.fill() == L'*' ); // Imbuing a new locale doesn't affect fill().
}

void
test02()
{
  std::locale loc(std::locale(), new tabby_mctype);
  std::locale::global(loc);
  std::basic_ios<C> out(0);
  VERIFY( out.fill() == L'\t' );
  out.imbue(std::locale::classic());
  VERIFY( out.fill() == L'\t' ); // Imbuing a new locale doesn't affect fill().
  out.fill(L'*');
  VERIFY( out.fill() == L'*' );  // This will be cached now.
  out.imbue(std::locale());
  VERIFY( out.fill() == L'*' );  // Imbuing a new locale doesn't affect fill().
}

int main()
{
  test01();
  test02();
}
