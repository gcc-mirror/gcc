// { dg-do run { target c++20 } }

#include <chrono>
#include <ranges>
#include <testsuite_hooks.h>

using namespace std::chrono;

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(CharT, S)

template<typename CharT>
void
test_empty()
{
  std::basic_string<CharT> res;

  const duration<double> d(33.111222);
  res = std::format(WIDEN("{:}"), d);
  VERIFY( res == WIDEN("33.1112s") );
  res = std::format(WIDEN("{:.0}"), d);
  VERIFY( res == WIDEN("33.1112s") );
  res = std::format(WIDEN("{:.3}"), d);
  VERIFY( res == WIDEN("33.1112s") );
  res = std::format(WIDEN("{:.6}"), d);
  VERIFY( res == WIDEN("33.1112s") );
  res = std::format(WIDEN("{:.9}"), d);
  VERIFY( res == WIDEN("33.1112s") );

  // Uses ostream operator<<
  const duration<double, std::nano> nd = d;
  res = std::format(WIDEN("{:}"), nd);
  VERIFY( res == WIDEN("3.31112e+10ns") );
  res = std::format(WIDEN("{:.0}"), nd);
  VERIFY( res == WIDEN("3.31112e+10ns") );
  res = std::format(WIDEN("{:.3}"), nd);
  VERIFY( res == WIDEN("3.31112e+10ns") );
  res = std::format(WIDEN("{:.6}"), nd);
  VERIFY( res == WIDEN("3.31112e+10ns") );
  res = std::format(WIDEN("{:.9}"), nd);
  VERIFY( res == WIDEN("3.31112e+10ns") );
}

template<typename CharT>
void
test_Q()
{
  std::basic_string<CharT> res;

  const duration<double> d(7.111222);
  res = std::format(WIDEN("{:%Q}"), d);
  VERIFY( res == WIDEN("7.111222") );
  res = std::format(WIDEN("{:.0%Q}"), d);
  VERIFY( res == WIDEN("7.111222") );
  res = std::format(WIDEN("{:.3%Q}"), d);
  VERIFY( res == WIDEN("7.111222") );
  res = std::format(WIDEN("{:.6%Q}"), d);
  VERIFY( res == WIDEN("7.111222") );
  res = std::format(WIDEN("{:.9%Q}"), d);
  VERIFY( res == WIDEN("7.111222") );

  duration<double, std::milli> md = d;
  res = std::format(WIDEN("{:%Q}"), md);
  VERIFY( res == WIDEN("7111.222") );
  res = std::format(WIDEN("{:.0%Q}"), md);
  VERIFY( res == WIDEN("7111.222") );
  res = std::format(WIDEN("{:.3%Q}"), md);
  VERIFY( res == WIDEN("7111.222") );
  res = std::format(WIDEN("{:.6%Q}"), md);
  VERIFY( res == WIDEN("7111.222") );
  res = std::format(WIDEN("{:.9%Q}"), md);
  VERIFY( res == WIDEN("7111.222") );

  const duration<double, std::nano> nd = d;
  res = std::format(WIDEN("{:%Q}"), nd);
  VERIFY( res == WIDEN("7111222000") );
  res = std::format(WIDEN("{:.0%Q}"), nd);
  VERIFY( res == WIDEN("7111222000") );
  res = std::format(WIDEN("{:.3%Q}"), nd);
  VERIFY( res == WIDEN("7111222000") );
  res = std::format(WIDEN("{:.6%Q}"), nd);
  VERIFY( res == WIDEN("7111222000") );
  res = std::format(WIDEN("{:.9%Q}"), nd);
  VERIFY( res == WIDEN("7111222000") );
}

template<typename CharT>
void
test_S_fp()
{
  std::basic_string<CharT> res;

  // Precision is ignored, but period affects output
  duration<double> d(5.111222);
  res = std::format(WIDEN("{:%S}"), d);
  VERIFY( res == WIDEN("05") );
  res = std::format(WIDEN("{:.0%S}"), d);
  VERIFY( res == WIDEN("05") );
  res = std::format(WIDEN("{:.3%S}"), d);
  VERIFY( res == WIDEN("05") );
  res = std::format(WIDEN("{:.6%S}"), d);
  VERIFY( res == WIDEN("05") );
  res = std::format(WIDEN("{:.9%S}"), d);
  VERIFY( res == WIDEN("05") );

  duration<double, std::milli> md = d;
  res = std::format(WIDEN("{:%S}"), md);
  VERIFY( res == WIDEN("05.111") );
  res = std::format(WIDEN("{:.0%S}"), md);
  VERIFY( res == WIDEN("05.111") );
  res = std::format(WIDEN("{:.3%S}"), md);
  VERIFY( res == WIDEN("05.111") );
  res = std::format(WIDEN("{:.6%S}"), md);
  VERIFY( res == WIDEN("05.111") );
  res = std::format(WIDEN("{:.9%S}"), md);
  VERIFY( res == WIDEN("05.111") );

  duration<double, std::micro> ud = d;
  res = std::format(WIDEN("{:%S}"), ud);
  VERIFY( res == WIDEN("05.111222") );
  res = std::format(WIDEN("{:.0%S}"), ud);
  VERIFY( res == WIDEN("05.111222") );
  res = std::format(WIDEN("{:.3%S}"), ud);
  VERIFY( res == WIDEN("05.111222") );
  res = std::format(WIDEN("{:.6%S}"), ud);
  VERIFY( res == WIDEN("05.111222") );
  res = std::format(WIDEN("{:.9%S}"), ud);
  VERIFY( res == WIDEN("05.111222") );

  duration<double, std::nano> nd = d;
  res = std::format(WIDEN("{:%S}"), nd);
  VERIFY( res == WIDEN("05.111222000") );
  res = std::format(WIDEN("{:.0%S}"), nd);
  VERIFY( res == WIDEN("05.111222000") );
  res = std::format(WIDEN("{:.3%S}"), nd);
  VERIFY( res == WIDEN("05.111222000") );
  res = std::format(WIDEN("{:.6%S}"), nd);
  VERIFY( res == WIDEN("05.111222000") );
  res = std::format(WIDEN("{:.9%S}"), nd);
  VERIFY( res == WIDEN("05.111222000") );

  duration<double, std::pico> pd = d;
  res = std::format(WIDEN("{:%S}"), pd);
  VERIFY( res == WIDEN("05.111222000000") );
  res = std::format(WIDEN("{:.0%S}"), pd);
  VERIFY( res == WIDEN("05.111222000000") );
  res = std::format(WIDEN("{:.3%S}"), pd);
  VERIFY( res == WIDEN("05.111222000000") );
  res = std::format(WIDEN("{:.6%S}"), pd);
  VERIFY( res == WIDEN("05.111222000000") );
  res = std::format(WIDEN("{:.9%S}"), pd);
  VERIFY( res == WIDEN("05.111222000000") );
}

template<typename CharT>
void
test_S_int()
{
  std::basic_string<CharT> res;
  const nanoseconds src(7'000'012'345);

  auto d = floor<seconds>(src);
  res = std::format(WIDEN("{:%S}"), d);
  VERIFY( res == WIDEN("07") );

  auto md = floor<milliseconds>(src);
  res = std::format(WIDEN("{:%S}"), md);
  VERIFY( res == WIDEN("07.000") );

  auto ud = floor<microseconds>(src);
  res = std::format(WIDEN("{:%S}"), ud);
  VERIFY( res == WIDEN("07.000012") );

  auto nd = floor<nanoseconds>(src);
  res = std::format(WIDEN("{:%S}"), nd);
  VERIFY( res == WIDEN("07.000012345") );

  using picoseconds = duration<unsigned long long, std::pico>;
  auto pd = floor<picoseconds>(src);
  res = std::format(WIDEN("{:%S}"), pd);
  VERIFY( res == WIDEN("07.000012345000") );
}

template<typename CharT>
void
test_all()
{
  test_empty<CharT>();
  test_Q<CharT>();
  test_S_int<CharT>();
  test_S_fp<CharT>();
}

int main()
{
  test_all<char>();

#ifdef _GLIBCXX_USE_WCHAR_T
  test_all<wchar_t>();
#endif // _GLIBCXX_USE_WCHAR_T
}
