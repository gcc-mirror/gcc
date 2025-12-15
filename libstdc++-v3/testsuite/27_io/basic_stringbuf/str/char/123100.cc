// { dg-do run }

#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  const int n = 20;
  const char data[n] = "abcde";
  char buf[n] = "0123456789";
  std::string expected("abcde56789\0\0\0\0\0\0\0\0\0\0", n);

  std::ostringstream out;
  out.rdbuf()->pubsetbuf(buf, n);
  out << data;
  VERIFY( out.str() == expected );
  VERIFY( out.str() == expected );
#if __cplusplus >= 201103L
  VERIFY( std::move(out).str() == expected );
#if __cplusplus >= 202002L && _GLIBCXX_USE_CXX11_ABI
  expected.clear();
#endif
  VERIFY( out.str() == expected );
  VERIFY( std::move(out).str() == expected );
#endif
}

void
test02()
{
  const int n = 20;
  const char data[n] = "abcde";
  char buf[n] = "0123456789";
  std::string expected("abcde56789\0\0\0\0\0\0\0\0\0\0", n);

  std::ostringstream out;
  out << std::string(n * 2, 'a');
  VERIFY( out.str() == std::string(n * 2, 'a') );
  out.rdbuf()->pubsetbuf(buf, n);
  out << data; // writes 6 chars
  VERIFY( out.str() == expected );
  VERIFY( out.str() == expected );
#if __cplusplus >= 201103L
  VERIFY( std::move(out).str() == expected );
#if __cplusplus >= 202002L && _GLIBCXX_USE_CXX11_ABI
  expected.clear();
#endif
  VERIFY( out.str() == expected );
  VERIFY( std::move(out).str() == expected );
#endif
}

int main()
{
  test01();
  test02();
}
