// { dg-do run { target c++11 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }

#include <thread>

#include <ostream>
#include <format>

void
test_no_includes(std::ostream& out)
{
  std::thread::id i{};
  // Check stream insertion works without including <sstream>:
  out << i;
#if __cpp_lib_formatters >= 202302
  // PR libstdc++/115099 - compilation error: format thread::id
  // Verify we can use std::thread::id with std::format without <sstream>:
  (void) std::format("{}", i);
#ifdef _GLIBCXX_USE_WCHAR_T
  (void) std::format(L"{}", i);
#endif
#endif
}

#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  std::ostringstream out;
  std::thread::id i{}, j{};
  out << i;
  auto s0 = out.str();
  VERIFY( s0 == "thread::id of a non-executing thread" );
  out.str("");
  out << j;
  VERIFY( out.str() == s0 );

  std::thread t1([]{});
  j = t1.get_id();
  out.str("");
  out << j;
  auto s1 = out.str();
  VERIFY( s1 != s0 );
  auto j2 = j;
  out.str("");
  out << j2;
  VERIFY( out.str() == s1 );

  std::thread t2([]{});
  j2 = t2.get_id();
  out.str("");
  out << j2;
  auto s2 = out.str();
  VERIFY( s2 != s0 );
  VERIFY( s2 != s1 );

#ifdef _GLIBCXX_USE_WCHAR_T
  std::wostringstream wout;
  wout << i;
  auto ws0 = wout.str();
  wout.str(L"");
  wout << j;
  auto ws1 = wout.str();
  wout.str(L"");
  wout << j2;
  auto ws2 = wout.str();
  wout.str(L"");

  wout << s0.c_str() << ' ' << s1.c_str() << ' ' << s2.c_str();
  VERIFY( wout.str() == (ws0 + L' ' + ws1 + L' ' + ws2) );
#endif

  t1.join();
  t2.join();
}

void
test02()
{
#if __cpp_lib_formatters >= 202302

  static_assert( std::is_default_constructible_v<std::formatter<std::thread::id, char>> );

  std::thread t1([]{});
  std::thread t2([]{});
  std::ostringstream out;
  std::thread::id i{};
  std::thread::id j = t1.get_id();
  std::thread::id k = t2.get_id();
  out << i << ' ' << j << ' ' << k;
  VERIFY( std::format("{} {} {}", i, j, k) == out.str() );

  out.str("");
  out << j;
  auto s1 = out.str();
  auto len = s1.size();
  out.str("");

  std::string s2;
  // with width
  s2 = std::format("{0:{1}}", j, len + 2);
  VERIFY( s2 == ("  " + s1) );
  // with align + width
  s2 = std::format("{0:>{1}}", j, len + 2);
  VERIFY( s2 == ("  " + s1) );
  s2 = std::format("{0:<{1}}", j, len + 2);
  VERIFY( s2 == (s1 + "  ") );
  // with fill-and-align + width
  s2 = std::format("{0:x^{1}}", j, len + 5);
  VERIFY( s2 == ("xx" + s1 + "xxx") );

#ifdef _GLIBCXX_USE_WCHAR_T
  static_assert( std::is_default_constructible_v<std::formatter<std::thread::id, wchar_t>> );
  auto ws1 = std::format(L"{}", j);
  VERIFY( ws1.length() == len );
#endif

  t1.join();
  t2.join();
#elif __cplusplus >= 202302L
# error "Feature-test macro for formatters has wrong value in <thread>"
#endif
}

int main()
{
  test01();
  test02();
}
