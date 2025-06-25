// { dg-do run { target c++20 } }

#include <chrono>
#include <testsuite_hooks.h>

using namespace std::chrono;

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

template<typename _CharT, typename ChronoType>
void
test(const ChronoType& ct)
{
  std::basic_string<_CharT> res;
  
  res = std::format(WIDEN("{:%% %t %n  more text}"), ct);
  VERIFY( res == WIDEN("% \t \n  more text") );

  res = std::format(WIDEN("{:7%% %t %n}"), ct);
  VERIFY( res == WIDEN("% \t \n  ") );

  res = std::format(WIDEN("{:>6%% %t %n}"), ct);
  VERIFY( res == WIDEN(" % \t \n") );

  res = std::format(WIDEN("{:+>7%% %t %n}"), ct);
  VERIFY( res == WIDEN("++% \t \n") );

  res = std::format(WIDEN("{:=^7%% %t %n}"), ct);
  VERIFY( res == WIDEN("=% \t \n=") );
}

template<typename CharT>
void
test_all()
{
  test<CharT>(20s);
  test<CharT>(10d);
  test<CharT>(Monday);
  test<CharT>(2020y/January/8);
  test<CharT>(local_days(2020y/January/8));
  test<CharT>(sys_days(2020y/January/8) + 13h + 10min + 5s);
#if _GLIBCXX_USE_CXX11_ABI || ! _GLIBCXX_USE_DUAL_ABI
  test<CharT>(sys_info());
  test<CharT>(local_info());
#endif
}

int main()
{
  test_all<char>();

#ifdef _GLIBCXX_USE_WCHAR_T
  test_all<wchar_t>();
#endif // _GLIBCXX_USE_WCHAR_T
}
