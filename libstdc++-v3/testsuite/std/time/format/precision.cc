// { dg-do run { target c++20 } }

#include <chrono>
#include <ranges>
#include <testsuite_hooks.h>

using namespace std::chrono;

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

template<typename _CharT>
void
test_empty()
{
  std::basic_string<_CharT> res;

  const duration<double> d(33.111222);
  res = std::format(WIDEN("{:.3}"), d);
  VERIFY( res == WIDEN("33.1112s") );
  res = std::format(WIDEN("{:.6}"), d);
  VERIFY( res == WIDEN("33.1112s") );
  res = std::format(WIDEN("{:.9}"), d);
  VERIFY( res == WIDEN("33.1112s") );

  // Uses ostream operator<<
  const duration<double, std::nano> nd = d;
  res = std::format(WIDEN("{:.3}"), nd);
  VERIFY( res == WIDEN("3.31112e+10ns") );
  res = std::format(WIDEN("{:.6}"), nd);
  VERIFY( res == WIDEN("3.31112e+10ns") );
  res = std::format(WIDEN("{:.9}"), nd);
  VERIFY( res == WIDEN("3.31112e+10ns") );
}

template<typename _CharT>
void
test_Q()
{
  std::basic_string<_CharT> res;

  const duration<double> d(7.111222);
  res = std::format(WIDEN("{:.3%Q}"), d);
  VERIFY( res == WIDEN("7.111222") );
  res = std::format(WIDEN("{:.6%Q}"), d);
  VERIFY( res == WIDEN("7.111222") );
  res = std::format(WIDEN("{:.9%Q}"), d);
  VERIFY( res == WIDEN("7.111222") );

  const duration<double, std::nano> nd = d;
  res = std::format(WIDEN("{:.3%Q}"), nd);
  VERIFY( res == WIDEN("7111222000") );
  res = std::format(WIDEN("{:.6%Q}"), nd);
  VERIFY( res == WIDEN("7111222000") );
  res = std::format(WIDEN("{:.9%Q}"), nd);
  VERIFY( res == WIDEN("7111222000") );
}

template<typename _CharT>
void
test_S()
{
  std::basic_string<_CharT> res;

  // Precision is ignored, but period affects output
  const duration<double> d(5.111222);
  res = std::format(WIDEN("{:.3%S}"), d);
  VERIFY( res == WIDEN("05") );
  res = std::format(WIDEN("{:.6%S}"), d);
  VERIFY( res == WIDEN("05") );
  res = std::format(WIDEN("{:.9%S}"), d);
  VERIFY( res == WIDEN("05") );

  const duration<double, std::milli> md = d;
  res = std::format(WIDEN("{:.3%S}"), md);
  VERIFY( res == WIDEN("05.111") );
  res = std::format(WIDEN("{:.6%S}"), md);
  VERIFY( res == WIDEN("05.111") );
  res = std::format(WIDEN("{:.9%S}"), md);
  VERIFY( res == WIDEN("05.111") );

  const duration<double, std::nano> nd = d;
  res = std::format(WIDEN("{:.3%S}"), nd);
  VERIFY( res == WIDEN("05.111222000") );
  res = std::format(WIDEN("{:.6%S}"), nd);
  VERIFY( res == WIDEN("05.111222000") );
  res = std::format(WIDEN("{:.9%S}"), nd);
  VERIFY( res == WIDEN("05.111222000") );
}

template<typename CharT>
void
test_all()
{
  test_empty<CharT>();
  test_Q<CharT>();
  test_S<CharT>();
}

int main()
{
  test_all<char>();

#ifdef _GLIBCXX_USE_WCHAR_T
  test_all<wchar_t>();
#endif // _GLIBCXX_USE_WCHAR_T
}
