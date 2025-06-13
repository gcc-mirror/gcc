// { dg-do run { target c++23 } }
// { dg-options "-fexec-charset=UTF-8" }
// { dg-timeout-factor 2 }

#include <algorithm>
#include <chrono>
#include <testsuite_hooks.h>

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(CharT, S)

using namespace std::chrono;

template<typename CharT>
void
test_year()
{
  std::basic_string<CharT> res;

  res = std::format(WIDEN("{:%Y}"), year(0));
  VERIFY( res == WIDEN("0000") );
  res = std::format(WIDEN("{:%C}"), year(0));
  VERIFY( res == WIDEN("00") );
  res = std::format(WIDEN("{:%y}"), year(0));
  VERIFY( res == WIDEN("00") );

  res = std::format(WIDEN("{:%Y}"), year(5));
  VERIFY( res == WIDEN("0005") );
  res = std::format(WIDEN("{:%C}"), year(5));
  VERIFY( res == WIDEN("00") );
  res = std::format(WIDEN("{:%y}"), year(5));
  VERIFY( res == WIDEN("05") );
  res = std::format(WIDEN("{:%Y}"), year(-5));
  VERIFY( res == WIDEN("-0005") );
  res = std::format(WIDEN("{:%C}"), year(-5));
  VERIFY( res == WIDEN("-01") );
  res = std::format(WIDEN("{:%y}"), year(-5));
  VERIFY( res == WIDEN("05") );

  res = std::format(WIDEN("{:%Y}"), year(213));
  VERIFY( res == WIDEN("0213") );
  res = std::format(WIDEN("{:%C}"), year(213));
  VERIFY( res == WIDEN("02") );
  res = std::format(WIDEN("{:%y}"), year(213));
  VERIFY( res == WIDEN("13") );
  res = std::format(WIDEN("{:%Y}"), year(-213));
  VERIFY( res == WIDEN("-0213") );
  res = std::format(WIDEN("{:%C}"), year(-213));
  VERIFY( res == WIDEN("-03") );
  res = std::format(WIDEN("{:%y}"), year(-213));
  VERIFY( res == WIDEN("13") );

  res = std::format(WIDEN("{:%Y}"), year(7100));
  VERIFY( res == WIDEN("7100") );
  res = std::format(WIDEN("{:%C}"), year(7100));
  VERIFY( res == WIDEN("71") );
  res = std::format(WIDEN("{:%y}"), year(7100));
  VERIFY( res == WIDEN("00") );
  res = std::format(WIDEN("{:%Y}"), year(-7100));
  VERIFY( res == WIDEN("-7100") );
  res = std::format(WIDEN("{:%C}"), year(-7100));
  VERIFY( res == WIDEN("-71") );
  res = std::format(WIDEN("{:%y}"), year(-7100));
  VERIFY( res == WIDEN("00") );

  res = std::format(WIDEN("{:%Y}"), year(12101));
  VERIFY( res == WIDEN("12101") );
  res = std::format(WIDEN("{:%C}"), year(12101));
  VERIFY( res == WIDEN("121") );
  res = std::format(WIDEN("{:%y}"), year(12101));
  VERIFY( res == WIDEN("01") );
  res = std::format(WIDEN("{:%Y}"), year(-12101));
  VERIFY( res == WIDEN("-12101") );
  res = std::format(WIDEN("{:%C}"), year(-12101));
  VERIFY( res == WIDEN("-122") );
  res = std::format(WIDEN("{:%y}"), year(-12101));
  VERIFY( res == WIDEN("01") );
}

template<typename CharT>
void
test_month()
{
  std::basic_string<CharT> res;

  res = std::format(WIDEN("{:%m}"), month(5));
  VERIFY( res == WIDEN("05") );
  res = std::format(WIDEN("{:%m}"), month(50));
  VERIFY( res == WIDEN("50") );
  res = std::format(WIDEN("{:%m}"), month(127));
  VERIFY( res == WIDEN("127") );
  res = std::format(WIDEN("{:%m}"), month(254));
  VERIFY( res == WIDEN("254") );
}

template<typename CharT>
void
test_day()
{
  std::basic_string<CharT> res;

  res = std::format(WIDEN("{:%d}"), day(3));
  VERIFY( res == WIDEN("03") );
  res = std::format(WIDEN("{:%d}"), day(22));
  VERIFY( res == WIDEN("22") );
  res = std::format(WIDEN("{:%d}"), day(100));
  VERIFY( res == WIDEN("100") );
  res = std::format(WIDEN("{:%d}"), day(207));
  VERIFY( res == WIDEN("207") );

  res = std::format(WIDEN("{:%e}"), day(5));
  VERIFY( res == WIDEN(" 5") );
  res = std::format(WIDEN("{:%e}"), day(99));
  VERIFY( res == WIDEN("99") );
  res = std::format(WIDEN("{:%e}"), day(183));
  VERIFY( res == WIDEN("183") );
  res = std::format(WIDEN("{:%e}"), day(214));
  VERIFY( res == WIDEN("214") );
}

template<typename CharT>
void
test_date()
{
  std::basic_string<CharT> res;

  res = std::format(WIDEN("{:%F}"), year(-22)/month(10)/day(20));
  VERIFY( res == WIDEN("-0022-10-20") );
  res = std::format(WIDEN("{:%D}"), year(-22)/month(10)/day(20));
  VERIFY( res == WIDEN("10/20/22") );

  res = std::format(WIDEN("{:%F}"), year(-2020)/month(123)/day(44));
  VERIFY( res == WIDEN("-2020-123-44") );
  res = std::format(WIDEN("{:%D}"), year(-2020)/month(123)/day(44));
  VERIFY( res == WIDEN("123/44/20") );

  res = std::format(WIDEN("{:%F}"), year(-23404)/month(99)/day(223));
  VERIFY( res == WIDEN("-23404-99-223") );
  res = std::format(WIDEN("{:%D}"), year(-23404)/month(99)/day(223));
  VERIFY( res == WIDEN("99/223/04") );

  res = std::format(WIDEN("{:%F}"), year(10000)/month(220)/day(100));
  VERIFY( res == WIDEN("10000-220-100") );
  res = std::format(WIDEN("{:%D}"), year(10000)/month(220)/day(100));
  VERIFY( res == WIDEN("220/100/00") );
}

template<typename CharT>
void
test_weekday()
{
  std::basic_string<CharT> res;

  res = std::format(WIDEN("{:%w}"), weekday(0));
  VERIFY( res == WIDEN("0") );
  res = std::format(WIDEN("{:%u}"), weekday(0));
  VERIFY( res == WIDEN("7") );

  res = std::format(WIDEN("{:%w}"), weekday(7));
  VERIFY( res == WIDEN("0") );
  res = std::format(WIDEN("{:%u}"), weekday(7));
  VERIFY( res == WIDEN("7") );

  res = std::format(WIDEN("{:%w}"), weekday(8));
  VERIFY( res == WIDEN("8") );
  res = std::format(WIDEN("{:%u}"), weekday(8));
  VERIFY( res == WIDEN("8") );

  res = std::format(WIDEN("{:%w}"), weekday(10));
  VERIFY( res == WIDEN("10") );
  res = std::format(WIDEN("{:%u}"), weekday(10));
  VERIFY( res == WIDEN("10") );

  res = std::format(WIDEN("{:%w}"), weekday(76));
  VERIFY( res == WIDEN("76") );
  res = std::format(WIDEN("{:%u}"), weekday(76));
  VERIFY( res == WIDEN("76") );

  res = std::format(WIDEN("{:%w}"), weekday(100));
  VERIFY( res == WIDEN("100") );
  res = std::format(WIDEN("{:%u}"), weekday(100));
  VERIFY( res == WIDEN("100") );

  res = std::format(WIDEN("{:%w}"), weekday(202));
  VERIFY( res == WIDEN("202") );
  res = std::format(WIDEN("{:%u}"), weekday(202));
  VERIFY( res == WIDEN("202") );
}

template<typename CharT>
void
test_hour()
{
  std::basic_string<CharT> res;

  res = std::format(WIDEN("{:%H}"), 0h + 5min + 6s);
  VERIFY( res == WIDEN("00") );
  res = std::format(WIDEN("{:%R}"), 0h + 5min + 6s);
  VERIFY( res == WIDEN("00:05") );
  res = std::format(WIDEN("{:%T}"), 0h + 5min + 6s);
  VERIFY( res == WIDEN("00:05:06") );
  res = std::format(WIDEN("{:%I}"), 0h + 5min + 6s);
  VERIFY( res == WIDEN("12") );
  res = std::format(WIDEN("{:%p}"), 0h + 5min + 6s);
  VERIFY( res == WIDEN("AM") );

  res = std::format(WIDEN("{:%H}"), 7h + 15min + 6s);
  VERIFY( res == WIDEN("07") );
  res = std::format(WIDEN("{:%R}"), 7h + 15min + 6s);
  VERIFY( res == WIDEN("07:15") );
  res = std::format(WIDEN("{:%T}"), 7h + 15min + 6s);
  VERIFY( res == WIDEN("07:15:06") );
  res = std::format(WIDEN("{:%I}"), 7h + 15min + 6s);
  VERIFY( res == WIDEN("07") );
  res = std::format(WIDEN("{:%p}"), 7h + 15min + 6s);
  VERIFY( res == WIDEN("AM") );

  res = std::format(WIDEN("{:%H}"), 15h + 55min + 26s);
  VERIFY( res == WIDEN("15") );
  res = std::format(WIDEN("{:%R}"), 15h + 55min + 26s);
  VERIFY( res == WIDEN("15:55") );
  res = std::format(WIDEN("{:%T}"), 15h + 55min + 26s);
  VERIFY( res == WIDEN("15:55:26") );
  res = std::format(WIDEN("{:%I}"), 15h + 55min + 26s);
  VERIFY( res == WIDEN("03") );
  res = std::format(WIDEN("{:%p}"), 15h + 55min + 26s);
  VERIFY( res == WIDEN("PM") );

  res = std::format(WIDEN("{:%H}"), 50h + 33min + 37s);
  VERIFY( res == WIDEN("50") );
  res = std::format(WIDEN("{:%R}"), 50h + 33min + 37s);
  VERIFY( res == WIDEN("50:33") );
  res = std::format(WIDEN("{:%T}"), 50h + 33min + 37s);
  VERIFY( res == WIDEN("50:33:37") );
  res = std::format(WIDEN("{:%I}"), 50h + 33min + 37s);
  VERIFY( res == WIDEN("02") );
  res = std::format(WIDEN("{:%p}"), 50h + 33min + 37s);
  VERIFY( res == WIDEN("AM") );

  res = std::format(WIDEN("{:%H}"), 100h + 21min + 48s);
  VERIFY( res == WIDEN("100") );
  res = std::format(WIDEN("{:%R}"), 100h + 21min + 48s);
  VERIFY( res == WIDEN("100:21") );
  res = std::format(WIDEN("{:%T}"), 100h + 21min + 48s);
  VERIFY( res == WIDEN("100:21:48") );
  res = std::format(WIDEN("{:%I}"), 100h + 21min + 48s);
  VERIFY( res == WIDEN("04") );
  res = std::format(WIDEN("{:%p}"), 100h + 21min + 48s);
  VERIFY( res == WIDEN("AM") );

  res = std::format(WIDEN("{:%H}"), 228h + 45min + 33s);
  VERIFY( res == WIDEN("228") );
  res = std::format(WIDEN("{:%R}"), 228h + 45min + 33s);
  VERIFY( res == WIDEN("228:45") );
  res = std::format(WIDEN("{:%T}"), 228h + 45min + 33s);
  VERIFY( res == WIDEN("228:45:33") );
  res = std::format(WIDEN("{:%I}"), 228h + 4min + 33s);
  VERIFY( res == WIDEN("12") );
  res = std::format(WIDEN("{:%p}"), 228h + 4min + 33s);
  VERIFY( res == WIDEN("PM") );

  res = std::format(WIDEN("{:%H}"), 1024h + 3min);
  VERIFY( res == WIDEN("1024") );
  res = std::format(WIDEN("{:%R}"), 1024h + 3min);
  VERIFY( res == WIDEN("1024:03") );
  res = std::format(WIDEN("{:%T}"), 1024h + 3min);
  VERIFY( res == WIDEN("1024:03:00") );
  res = std::format(WIDEN("{:%I}"), 1024h + 3min);
  VERIFY( res == WIDEN("04") );
  res = std::format(WIDEN("{:%p}"), 1024h + 3min);
  VERIFY( res == WIDEN("PM") );

  res = std::format(WIDEN("{:%H}"), 2039h);
  VERIFY( res == WIDEN("2039") );
  res = std::format(WIDEN("{:%R}"), 2039h);
  VERIFY( res == WIDEN("2039:00") );
  res = std::format(WIDEN("{:%T}"), 2039h);
  VERIFY( res == WIDEN("2039:00:00") );
  res = std::format(WIDEN("{:%I}"), 2039h);
  VERIFY( res == WIDEN("11") );
  res = std::format(WIDEN("{:%p}"), 2039h);
  VERIFY( res == WIDEN("PM") );

  res = std::format(WIDEN("{:%H}"), 22111h + 59min + 59s);
  VERIFY( res == WIDEN("22111") );
  res = std::format(WIDEN("{:%R}"), 22111h + 59min + 59s);
  VERIFY( res == WIDEN("22111:59") );
  res = std::format(WIDEN("{:%T}"), 22111h + 59min + 59s);
  VERIFY( res == WIDEN("22111:59:59") );
  res = std::format(WIDEN("{:%I}"), 22111h + 59min + 59s);
  VERIFY( res == WIDEN("07") );
  res = std::format(WIDEN("{:%p}"), 22111h + 59min + 59s);
  VERIFY( res == WIDEN("AM") );

  res = std::format(WIDEN("{:%H}"), -22111h - 59min - 59s);
  VERIFY( res == WIDEN("-22111") );
  res = std::format(WIDEN("{:%R}"), -22111h - 59min - 59s);
  VERIFY( res == WIDEN("-22111:59") );
  res = std::format(WIDEN("{:%T}"), -22111h - 59min - 59s);
  VERIFY( res == WIDEN("-22111:59:59") );
  res = std::format(WIDEN("{:%I}"), -22111h - 59min - 59s);
  VERIFY( res == WIDEN("-07") );
  res = std::format(WIDEN("{:%p}"), -22111h - 59min - 59s);
  VERIFY( res == WIDEN("AM") );
}

int main()
{
  test_year<char>();
  test_month<char>();
  test_day<char>();
  test_date<char>();
  test_weekday<char>();
  test_hour<char>();

#ifdef _GLIBCXX_USE_WCHAR_T
  test_year<wchar_t>();
  test_month<wchar_t>();
  test_day<wchar_t>();
  test_date<wchar_t>();
  test_weekday<wchar_t>();
  test_hour<wchar_t>();
#endif // _GLIBCXX_USE_WCHAR_T
}
