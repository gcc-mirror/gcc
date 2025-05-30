// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

using namespace std::chrono;

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

template<typename _CharT>
void
test_padding()
{
  std::basic_string<_CharT> res;

  res = std::format(WIDEN("{:5}"), day(2));
  VERIFY( res == WIDEN("02   ") );

  res = std::format(WIDEN("{:>6}"), weekday(4));
  VERIFY( res == WIDEN("   Thu") );

  res = std::format(WIDEN("{:^7}"), month(3));
  VERIFY( res == WIDEN("  Mar  ") );

  res = std::format(WIDEN("{:-<4}"), day(30));
  VERIFY( res == WIDEN("30--") );

  res = std::format(WIDEN("{:+>30}"), weekday(9));
  VERIFY( res == WIDEN("++++++9 is not a valid weekday") );

  res = std::format(WIDEN("{:=^27}"), month(16));
  VERIFY( res == WIDEN("==16 is not a valid month==") );
}

template<typename T, typename _CharT>
void verify(const T& t, const _CharT* str)
{
  std::basic_string<_CharT> res;

  res = std::format(WIDEN("{}"), t);
  VERIFY( res == str );

  std::basic_stringstream<_CharT> os;
  os << t;
  res = std::move(os).str();
  VERIFY( res == str );
}

template<typename _CharT>
void
test_day()
{
  verify( day(0),   WIDEN("00 is not a valid day") );
  verify( day(1),   WIDEN("01") );
  verify( day(10),  WIDEN("10") );
  verify( day(32),  WIDEN("32 is not a valid day") );
  verify( day(110), WIDEN("110 is not a valid day") );
  verify( day(255), WIDEN("255 is not a valid day") );
}

template<typename _CharT>
void
test_month()
{
  verify( month(0),   WIDEN("0 is not a valid month") );
  verify( month(1),   WIDEN("Jan") );
  verify( month(10),  WIDEN("Oct") );
  verify( month(32),  WIDEN("32 is not a valid month") );
  verify( month(110), WIDEN("110 is not a valid month") );
  verify( month(100), WIDEN("100 is not a valid month") );
  verify( month(110), WIDEN("110 is not a valid month") );
  verify( month(255), WIDEN("255 is not a valid month") );
}

template<typename _CharT>
void
test_year()
{
  verify( year(-32768), WIDEN("-32768 is not a valid year") );
  verify( year(-32767), WIDEN("-32767") );
  verify( year(-67),    WIDEN( "-0067") );
  verify( year(-1),     WIDEN( "-0001") );
  verify( year(0),      WIDEN(  "0000") );
  verify( year(1),      WIDEN(  "0001") );
  verify( year(123),    WIDEN(  "0123") );
  verify( year(2025),   WIDEN(  "2025") );
  verify( year(32767),  WIDEN( "32767") );
}

template<typename _CharT>
void
test_weekday()
{
  verify( weekday(0),   WIDEN("Sun") );
  verify( weekday(2),   WIDEN("Tue") );
  verify( weekday(6),   WIDEN("Sat") );
  verify( weekday(7),   WIDEN("Sun") );
  verify( weekday(9),   WIDEN("9 is not a valid weekday") );
  verify( weekday(32),  WIDEN("32 is not a valid weekday") );
  verify( weekday(110), WIDEN("110 is not a valid weekday") );
  verify( weekday(255), WIDEN("255 is not a valid weekday") );
}

template<typename _CharT>
void
test_weekday_indexed()
{
  verify( weekday(0)[0],   WIDEN("Sun[0 is not a valid index]") );
  verify( weekday(2)[1],   WIDEN("Tue[1]") );
  verify( weekday(6)[5],   WIDEN("Sat[5]") );
  verify( weekday(7)[6],   WIDEN("Sun[6 is not a valid index]") );
  verify( weekday(7)[12],  WIDEN("Sun[12 is not a valid index]") );
  verify( weekday(5)[117], WIDEN("Fri[117 is not a valid index]") );
  verify( weekday(7)[255], WIDEN("Sun[255 is not a valid index]") );
  verify( weekday(9)[1],   WIDEN("9 is not a valid weekday[1]") );
  verify( weekday(32)[7],  WIDEN("32 is not a valid weekday[7 is not a valid index]") );
}

template<typename _CharT>
void
test_weekday_last()
{
  verify( weekday(0)[last], WIDEN("Sun[last]") );
  verify( weekday(9)[last], WIDEN("9 is not a valid weekday[last]") );
}

template<typename _CharT>
void
test_month_day()
{
  verify( month(1)/30,  WIDEN("Jan/30") );
  verify( month(3)/32,  WIDEN("Mar/32 is not a valid day") );
  verify( month(13)/30, WIDEN("13 is not a valid month/30") );
  verify( month(13)/32, WIDEN("13 is not a valid month/32 is not a valid day") );
}

template<typename _CharT>
void
test_month_day_last()
{
  verify( month(1)/last, WIDEN("Jan/last") );
  verify( month(14)/last, WIDEN("14 is not a valid month/last") );
}

template<typename _CharT>
void
test_month_weekday()
{
  verify( month(1)/weekday(2)[1],
	  WIDEN("Jan/Tue[1]") );
  verify( month(3)/weekday(9)[2],
	  WIDEN("Mar/9 is not a valid weekday[2]") );
  verify( month(13)/weekday(1)[7],
	  WIDEN("13 is not a valid month/Mon[7 is not a valid index]") );
  verify( month(13)/weekday(10)[3],
	  WIDEN("13 is not a valid month/10 is not a valid weekday[3]") );
  verify( month(13)/weekday(130)[0],
	  WIDEN("13 is not a valid month/130 is not a valid weekday[0 is not a valid index]") );
}

template<typename _CharT>
void
test_month_weekday_last()
{
  verify( month(1)/weekday(2)[last],
	  WIDEN("Jan/Tue[last]") );
  verify( month(3)/weekday(9)[last],
	  WIDEN("Mar/9 is not a valid weekday[last]") );
  verify( month(13)/weekday(1)[last],
	  WIDEN("13 is not a valid month/Mon[last]") );
  verify( month(13)/weekday(10)[last],
	  WIDEN("13 is not a valid month/10 is not a valid weekday[last]") );
}

template<typename _CharT>
void
test_year_month()
{
  verify( year(2024)/month(1),
	  WIDEN("2024/Jan") );
  verify( year(2025)/month(14),
	  WIDEN("2025/14 is not a valid month") );
  verify( year(-32768)/month(2),
	  WIDEN("-32768 is not a valid year/Feb") );
  verify( year(-32768)/month(0),
	  WIDEN("-32768 is not a valid year/0 is not a valid month") );
}

template<typename _CharT>
void
test_year_month_day()
{
  verify( year(2024)/month(1)/30,
	  WIDEN("2024-01-30") );
  verify( year(-100)/month(14)/1,
	  // Should be -0100-14-01
	  WIDEN("-100-14-01 is not a valid date") );
  verify( year(2025)/month(11)/100,
	  // Should be 2025-11-100 ?
	  WIDEN("2025-11-99 is not a valid date") );
  verify( year(-32768)/month(2)/10,
	  WIDEN("-32768-02-10 is not a valid date") );
  verify( year(-32768)/month(212)/10,
	  // Should be 32768-212-10?
	  WIDEN("-32768-84-10 is not a valid date") );
  verify( year(-32768)/month(2)/105,
          // Should be 32768-02-99?
	  WIDEN("-32768-02-99 is not a valid date") );
  verify( year(-32768)/month(14)/55,
	  WIDEN("-32768-14-55 is not a valid date") );
}

template<typename _CharT>
void
test_year_month_last()
{
  verify( year(2024)/month(1)/last,
	  WIDEN("2024/Jan/last") );
  verify( year(2025)/month(14)/last,
	  WIDEN("2025/14 is not a valid month/last") );
  verify( year(-32768)/month(2)/last,
	  WIDEN("-32768 is not a valid year/Feb/last") );
  verify( year(-32768)/month(0)/last,
	  WIDEN("-32768 is not a valid year/0 is not a valid month/last") );
}

template<typename _CharT>
void
test_year_month_weekday()
{
  verify( year(2024)/month(1)/weekday(2)[1],
	  WIDEN("2024/Jan/Tue[1]") );
  verify( year(-1)/month(3)/weekday(9)[2],
	  WIDEN("-0001/Mar/9 is not a valid weekday[2]") );
  verify( year(-32768)/month(13)/weekday(1)[7],
	  WIDEN("-32768 is not a valid year/13 is not a valid month/Mon[7 is not a valid index]") );
  verify( year(-100)/month(13)/weekday(10)[3],
	  WIDEN("-0100/13 is not a valid month/10 is not a valid weekday[3]") );
  verify( year(-32768)/month(13)/weekday(130)[0],
	  WIDEN("-32768 is not a valid year/13 is not a valid month/130 is not a valid weekday[0 is not a valid index]") );
}

template<typename _CharT>
void
test_year_month_weekday_last()
{
  verify( year(2024)/month(1)/weekday(2)[last],
	  WIDEN("2024/Jan/Tue[last]") );
  verify( year(-1)/month(3)/weekday(9)[last],
	  WIDEN("-0001/Mar/9 is not a valid weekday[last]") );
  verify( year(-32768)/month(13)/weekday(1)[last],
	  WIDEN("-32768 is not a valid year/13 is not a valid month/Mon[last]") );
  verify( year(-100)/month(13)/weekday(10)[last],
	  WIDEN("-0100/13 is not a valid month/10 is not a valid weekday[last]") );
  verify( year(-32768)/month(13)/weekday(130)[last],
	  WIDEN("-32768 is not a valid year/13 is not a valid month/130 is not a valid weekday[last]") );
}

template<typename CharT>
void
test_calendar()
{
  test_day<CharT>();
  test_month<CharT>();
  test_year<CharT>();

  test_weekday<CharT>();
  test_weekday_indexed<CharT>();
  test_weekday_last<CharT>();

  test_month_day<CharT>();
  test_month_day_last<CharT>();
  test_month_weekday<CharT>();
  test_month_weekday_last<CharT>();

  test_year_month<CharT>();
  test_year_month_day<CharT>();
  test_year_month_last<CharT>();
  test_year_month_weekday<CharT>();
  test_year_month_weekday_last<CharT>();
}

template<typename CharT>
void
test_all()
{
  test_padding<CharT>();
  test_calendar<CharT>();
}

int main()
{
  test_all<char>();

#ifdef _GLIBCXX_USE_WCHAR_T
  test_all<wchar_t>();
#endif // _GLIBCXX_USE_WCHAR_T
}
