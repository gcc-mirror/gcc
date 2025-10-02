// { dg-do run { target c++20 } }
// { dg-require-effective-target hosted }
// { dg-timeout-factor 5 }

#include <chrono>
#include <ranges>
#include <sstream>
#include <testsuite_hooks.h>
#include "custom_rep.h"

using namespace std::chrono;


template<typename CharT, typename T>
void
test_no_empty_spec()
{
  try
  {
    T t{};

    if constexpr (std::is_same_v<CharT, char>)
      (void)std::vformat("{}", std::make_format_args(t));
#ifdef _GLIBCXX_USE_WCHAR_T
    else
      (void)std::vformat(L"{}", std::make_wformat_args(t));
#endif // _GLIBCXX_USE_WCHAR_T
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
    VERIFY(true);
  }
}

template<typename T, typename CharT>
void verify(const T& t, std::basic_string_view<CharT> str)
{
  std::basic_string<CharT> res;

  res = std::format(WIDEN("{}"), t);
  VERIFY( res == str );

  std::basic_stringstream<CharT> os;
  os << t;
  res = std::move(os).str();
  VERIFY( res == str );
}

template<typename T, typename CharT>
void verify(const T& t, const CharT* str)
{ verify(t, std::basic_string_view<CharT>(str)); }

template<typename CharT>
void
test_padding()
{
  std::basic_string<CharT> res;

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

using deciseconds = duration<seconds::rep, std::deci>;

template<typename CharT>
void
test_duration()
{
  std::basic_string<CharT> res;

  const milliseconds di(40);
  verify( di, WIDEN("40ms") );
  res = std::format(WIDEN("{:>6}"), di);
  VERIFY( res == WIDEN("  40ms") );

  verify( -di, WIDEN("-40ms") );
  res = std::format(WIDEN("{:>6}"), -di);
  VERIFY( res == WIDEN(" -40ms") );
}

template<typename CharT>
void
test_duration_fp()
{
  std::basic_string<CharT> res;

  const duration<double> df(11.22);
  verify( df, WIDEN("11.22s") );
  res = std::format(WIDEN("{:=^12}"), df);
  VERIFY( res == WIDEN("===11.22s===") );

  verify( -df, WIDEN("-11.22s") );
  res = std::format(WIDEN("{:=^12}"), -df);
  VERIFY( res == WIDEN("==-11.22s===") );

  // precision accepted but ignored
  res = std::format(WIDEN("{:.6}"), df);
  VERIFY( res == WIDEN("11.22s") );
}

template<typename CharT>
void
test_duration_cust()
{
  std::basic_string<CharT> res;
  const duration<char, std::ratio<1, 10>> charRep(123);
  verify( charRep, WIDEN("123ds") );

  // +asLong returns long, so formatted as long
  const duration<Rep<long>> asLong(20);
  verify( asLong, WIDEN("20s") );
  res = std::format(WIDEN("{:>6}"), asLong);
  VERIFY( res == WIDEN("   20s") );

  verify( -asLong, WIDEN("-20s") );
  res = std::format(WIDEN("{:>6}"), -asLong);
  VERIFY( res == WIDEN("  -20s") );

  res = std::format(WIDEN("{:%Q}"), asLong);
  VERIFY( res == WIDEN("20") );
  res = std::format(WIDEN("{:+<7%Q}"), asLong);
  VERIFY( res == WIDEN("20+++++") );

  // +asRep returns Rep<>, so formatted as Rep<>
  const duration<Rep<>> asRep(10);
  verify( asRep, WIDEN("10[via <<]s") );
  res = std::format(WIDEN("{:=^15}"), asRep);
  VERIFY( res == WIDEN("==10[via <<]s==") );

  verify( -asRep, WIDEN("-10[via <<]s") );
  res = std::format(WIDEN("{:=^15}"), -asRep);
  VERIFY( res == WIDEN("=-10[via <<]s==") );

  res = std::format(WIDEN("{:%Q}"), asRep);
  VERIFY( res == WIDEN("10[via format]") );
  res = std::format(WIDEN("{:=^18%Q}"), asRep);
  VERIFY( res == WIDEN("==10[via format]==") );

  const duration<Rep<>, std::milli> milliRep(10);
  verify( milliRep, WIDEN("10[via <<]ms") );
  res = std::format(WIDEN("{:=^15}"), milliRep);
  VERIFY( res == WIDEN("=10[via <<]ms==") );

  verify( -milliRep, WIDEN("-10[via <<]ms") );
  res = std::format(WIDEN("{:=^15}"), -milliRep);
  VERIFY( res == WIDEN("=-10[via <<]ms=") );

  res = std::format(WIDEN("{:%Q}"), milliRep);
  VERIFY( res == WIDEN("10[via format]") );
  res = std::format(WIDEN("{:=^18%Q}"), milliRep);
  VERIFY( res == WIDEN("==10[via format]==") );
}

template<typename Ratio, typename Rep, typename Period>
constexpr auto
hms(const duration<Rep, Period>& d)
{
  using Dur = duration<Rep, typename Ratio::period>;
  return hh_mm_ss<Dur>(duration_cast<Dur>(d));
}

template<typename CharT>
void
test_hh_mm_ss()
{
  auto dt = 22h + 24min + 54s + 111222333ns;
  verify( hms<nanoseconds>(dt),
	  WIDEN("22:24:54.111222333") );
  verify( hms<microseconds>(dt),
	  WIDEN("22:24:54.111222") );
  verify( hms<milliseconds>(dt),
	  WIDEN("22:24:54.111") );
  verify( hms<deciseconds>(dt),
	  WIDEN("22:24:54.1") );
  verify( hms<seconds>(dt),
	  WIDEN("22:24:54") );
  verify( hms<minutes>(dt),
	  WIDEN("22:24:00") );
  verify( hms<hours>(dt),
	  WIDEN("22:00:00") );
  verify( hms<nanoseconds>(-dt),
	  WIDEN("-22:24:54.111222333") );
  verify( hms<microseconds>(-dt),
	  WIDEN("-22:24:54.111222") );
  verify( hms<milliseconds>(-dt),
	  WIDEN("-22:24:54.111") );
  verify( hms<deciseconds>(-dt),
	  WIDEN("-22:24:54.1") );
  verify( hms<seconds>(-dt),
	  WIDEN("-22:24:54") );
  verify( hms<minutes>(-dt),
	  WIDEN("-22:24:00") );
  verify( hms<hours>(-dt),
	  WIDEN("-22:00:00") );

  verify( hms<nanoseconds>(-dt),
	  WIDEN("-22:24:54.111222333") );

  dt += 300h;
  verify( hms<nanoseconds>(dt),
	  WIDEN("322:24:54.111222333") );
  verify( hms<nanoseconds>(-dt),
	  WIDEN("-322:24:54.111222333") );

  dt += 14000h;
  verify( hms<nanoseconds>(dt),
	  WIDEN("14322:24:54.111222333") );
  verify( hms<nanoseconds>(-dt),
	  WIDEN("-14322:24:54.111222333") );
}

template<typename CharT>
void
test_hh_mm_ss_fp()
{
  duration<double> dt = 22h + 24min + 54s + 111222333ns;
  // period controls number of subseconds
  verify( hms<nanoseconds>(dt),
	  WIDEN("22:24:54.111222333") );
  verify( hms<microseconds>(dt),
	  WIDEN("22:24:54.111222") );
  verify( hms<milliseconds>(dt),
	  WIDEN("22:24:54.111") );
  verify( hms<deciseconds>(dt),
	  WIDEN("22:24:54.1") );
  verify( hms<seconds>(dt),
	  WIDEN("22:24:54") );
  verify( hms<nanoseconds>(-dt),
	  WIDEN("-22:24:54.111222333") );
  verify( hms<microseconds>(-dt),
	  WIDEN("-22:24:54.111222") );
  verify( hms<milliseconds>(-dt),
	  WIDEN("-22:24:54.111") );
  verify( hms<deciseconds>(-dt),
	  WIDEN("-22:24:54.1") );
  verify( hms<seconds>(-dt),
	  WIDEN("-22:24:54") );

  // but hour and minutes are preserved
  verify( hms<minutes>(dt),
	  WIDEN("22:24:54") );
  verify( hms<hours>(dt),
	  WIDEN("22:24:54") );
  verify( hms<minutes>(-dt),
	  WIDEN("-22:24:54") );
  verify( hms<hours>(-dt),
	  WIDEN("-22:24:54") );
}

template<typename CharT>
void
test_hh_mm_ss_cust()
{
  const duration<char, deciseconds::period> charRep(123);
  verify( hms<deciseconds>(charRep),
	  WIDEN("00:00:12.3") );
  verify( hms<seconds>(charRep),
	  WIDEN("00:00:12") );

  auto dt = 22h + 24min + 54s + 123ms;
  // +plus returns long, so formatted as long
  const duration<Rep<long>, std::milli> asLong(dt.count());
  verify( hms<milliseconds>(asLong),
	  WIDEN("22:24:54.123") );
  verify( hms<deciseconds>(asLong),
	  WIDEN("22:24:54.1") );
  verify( hms<seconds>(asLong),
	  WIDEN("22:24:54") );
  verify( hms<milliseconds>(-asLong),
	  WIDEN("-22:24:54.123") );
  verify( hms<deciseconds>(-asLong),
	  WIDEN("-22:24:54.1") );
  verify( hms<seconds>(-asLong),
	  WIDEN("-22:24:54") );

  // +asRep returns Rep<>, so formatted as Rep<>
  const duration<Rep<>, std::milli> asRep(dt.count());
  verify( hms<milliseconds>(asRep),
	  WIDEN("22:24:54.123") );
  verify( hms<deciseconds>(asRep),
	  WIDEN("22:24:54.1") );
  verify( hms<seconds>(asLong),
	  WIDEN("22:24:54") );
  verify( hms<milliseconds>(-asLong),
	  WIDEN("-22:24:54.123") );
  verify( hms<deciseconds>(-asLong),
	  WIDEN("-22:24:54.1") );
  verify( hms<seconds>(-asLong),
	  WIDEN("-22:24:54") );
}

template<typename CharT>
void
test_durations()
{
  test_duration<CharT>();
  test_duration_fp<CharT>();
  test_duration_cust<CharT>();

  test_hh_mm_ss<CharT>();
  test_hh_mm_ss_fp<CharT>();
  test_hh_mm_ss_cust<CharT>();
}

template<typename CharT>
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

template<typename CharT>
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

template<typename CharT>
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

template<typename CharT>
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

template<typename CharT>
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

template<typename CharT>
void
test_weekday_last()
{
  verify( weekday(0)[last], WIDEN("Sun[last]") );
  verify( weekday(9)[last], WIDEN("9 is not a valid weekday[last]") );
}

template<typename CharT>
void
test_month_day()
{
  verify( month(1)/30,  WIDEN("Jan/30") );
  verify( month(3)/32,  WIDEN("Mar/32 is not a valid day") );
  verify( month(13)/30, WIDEN("13 is not a valid month/30") );
  verify( month(13)/32, WIDEN("13 is not a valid month/32 is not a valid day") );
}

template<typename CharT>
void
test_month_day_last()
{
  verify( month(1)/last, WIDEN("Jan/last") );
  verify( month(14)/last, WIDEN("14 is not a valid month/last") );
}

template<typename CharT>
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

template<typename CharT>
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

template<typename CharT>
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

template<typename CharT>
void
test_year_month_day()
{
  verify( year(2024)/month(1)/30,
	  WIDEN("2024-01-30") );
  verify( year(-100)/month(14)/1,
	  WIDEN("-0100-14-01 is not a valid date") );
  verify( year(2025)/month(11)/100,
	  WIDEN("2025-11-100 is not a valid date") );
  verify( year(-32768)/month(2)/10,
	  WIDEN("-32768-02-10 is not a valid date") );
  verify( year(-32768)/month(212)/10,
	  WIDEN("-32768-212-10 is not a valid date") );
  verify( year(-32768)/month(2)/105,
	  WIDEN("-32768-02-105 is not a valid date") );
  verify( year(-32768)/month(14)/55,
	  WIDEN("-32768-14-55 is not a valid date") );
}

template<typename CharT>
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

template<typename CharT>
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

template<typename CharT>
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

template<typename Clock, typename Dur, typename Dur2>
constexpr auto
wall_cast(const local_time<Dur2>& tp)
{
  using TP = time_point<Clock, std::common_type_t<Dur, days>>;
  if constexpr (std::is_same_v<Clock, utc_clock> || std::is_same_v<Clock, file_clock>)
    return clock_cast<Clock>(wall_cast<system_clock, Dur>(tp));
  else if constexpr (std::is_same_v<Clock, tai_clock>)
    return TP(floor<Dur>(tp.time_since_epoch()) + days(4383));
  else if constexpr (std::is_same_v<Clock, gps_clock>)
    return TP(floor<Dur>(tp.time_since_epoch()) - days(3657));
  else // system_clock, local_t
    return time_point<Clock, Dur>(floor<Dur>(tp.time_since_epoch()));
}

using decadays = duration<days::rep, std::ratio_multiply<std::deca, days::period>>;
using kilodays = duration<days::rep, std::ratio_multiply<std::kilo, days::period>>;

template<typename CharT, typename Clock, bool CustomizedOstream>
void
test_time_point()
{
  std::basic_string<CharT> res;

  const auto lt = local_days(2024y/March/22) + 13h + 24min + 54s + 111222333ns;
  auto strip_time = [](std::basic_string_view<CharT> sv)
  { return CustomizedOstream ? sv.substr(0, 10) : sv; };

  verify( wall_cast<Clock, nanoseconds>(lt),
	  WIDEN("2024-03-22 13:24:54.111222333") );
  verify( wall_cast<Clock, microseconds>(lt),
	  WIDEN("2024-03-22 13:24:54.111222") );
  verify( wall_cast<Clock, milliseconds>(lt),
	  WIDEN("2024-03-22 13:24:54.111") );
  verify( wall_cast<Clock, seconds>(lt),
	  WIDEN("2024-03-22 13:24:54") );
  verify( wall_cast<Clock, minutes>(lt),
	  WIDEN("2024-03-22 13:24:00") );
  verify( wall_cast<Clock, hours>(lt),
	  WIDEN("2024-03-22 13:00:00") );
  verify( wall_cast<Clock, days>(lt),
	  strip_time(WIDEN("2024-03-22 00:00:00")) );
  verify( wall_cast<Clock, decadays>(lt),
	  strip_time(WIDEN("2024-03-18 00:00:00")) );
  verify( wall_cast<Clock, kilodays>(lt),
	  strip_time(WIDEN("2022-01-08 00:00:00")) );

  if constexpr (!CustomizedOstream)
  {
    verify( wall_cast<Clock, duration<double>>(lt),
	    WIDEN("2024-03-22 13:24:54") );
    verify( wall_cast<Clock, years>(lt),
	    WIDEN("2024-01-01 02:16:48") );
  }
  else
  {
    test_no_empty_spec<CharT, time_point<Clock, duration<double>>>();
    test_no_empty_spec<CharT, time_point<Clock, years>>();
  }
}

template<typename CharT>
void
test_leap_second()
{
  std::basic_string<CharT> res;

  const auto st = sys_days(2012y/June/30) + 23h + 59min + 59s + 111222333ns;
  auto tp = clock_cast<utc_clock>(st);
  tp += 1s;

  verify( floor<nanoseconds>(tp),
	  WIDEN("2012-06-30 23:59:60.111222333") );
  verify( floor<microseconds>(tp),
	  WIDEN("2012-06-30 23:59:60.111222") );
  verify( floor<milliseconds>(tp),
	  WIDEN("2012-06-30 23:59:60.111") );
  verify( floor<seconds>(tp),
	  WIDEN("2012-06-30 23:59:60") );
}

#if _GLIBCXX_USE_CXX11_ABI || !_GLIBCXX_USE_DUAL_ABI
template<typename Dur, typename Dur2>
auto
make_zoned(const sys_time<Dur2>& st, const time_zone* tz)
{ return zoned_time<Dur>(tz, floor<Dur>(st)); }

template<typename CharT>
void
test_zoned_time()
{
  const auto st = sys_days(2024y/March/22) + 13h + 24min + 54s + 111222333ns;
  const time_zone* tz = locate_zone("Europe/Sofia");
  VERIFY( tz != nullptr );

  verify( make_zoned<nanoseconds>(st, tz),
	  WIDEN("2024-03-22 15:24:54.111222333 EET") );
  verify( make_zoned<microseconds>(st, tz),
	  WIDEN("2024-03-22 15:24:54.111222 EET") );
  verify( make_zoned<milliseconds>(st, tz),
	  WIDEN("2024-03-22 15:24:54.111 EET") );
  verify( make_zoned<seconds>(st, tz),
	  WIDEN("2024-03-22 15:24:54 EET") );
  verify( make_zoned<minutes>(st, tz),
	  WIDEN("2024-03-22 15:24:00 EET") );
  verify( make_zoned<hours>(st, tz),
	  WIDEN("2024-03-22 15:00:00 EET") );
  verify( make_zoned<days>(st, tz),
	  WIDEN("2024-03-22 02:00:00 EET") );
  verify( make_zoned<decadays>(st, tz),
	  WIDEN("2024-03-18 02:00:00 EET") );
  verify( make_zoned<kilodays>(st, tz),
	  WIDEN("2022-01-08 02:00:00 EET") );
}
#endif

template<typename Dur, typename Dur2>
auto
local_fmt(const local_time<Dur2>& lt, std::string* zone)
{ return local_time_format(floor<Dur>(lt), zone); }

template<typename CharT>
void
test_local_time_format()
{
  std::basic_string<CharT> res;

  std::string abbrev = "Zone";
  const auto lt = local_days(2024y/March/22) + 13h + 24min + 54s + 111222333ns;

  res = std::format(WIDEN("{}"), local_fmt<nanoseconds>(lt, &abbrev));
  VERIFY( res == WIDEN("2024-03-22 13:24:54.111222333 Zone") );
  res = std::format(WIDEN("{}"), local_fmt<microseconds>(lt, &abbrev));
  VERIFY( res == WIDEN("2024-03-22 13:24:54.111222 Zone") );
  res = std::format(WIDEN("{}"), local_fmt<milliseconds>(lt, &abbrev));
  VERIFY( res == WIDEN("2024-03-22 13:24:54.111 Zone") );
  res = std::format(WIDEN("{}"), local_fmt<seconds>(lt, &abbrev));
  VERIFY( res == WIDEN("2024-03-22 13:24:54 Zone") );
  res = std::format(WIDEN("{}"), local_fmt<minutes>(lt, &abbrev));
  VERIFY( res == WIDEN("2024-03-22 13:24:00 Zone") );
  res = std::format(WIDEN("{}"), local_fmt<hours>(lt, &abbrev));
  VERIFY( res == WIDEN("2024-03-22 13:00:00 Zone") );
  res = std::format(WIDEN("{}"), local_fmt<days>(lt, &abbrev));
  VERIFY( res == WIDEN("2024-03-22 00:00:00 Zone") );
  res = std::format(WIDEN("{}"), local_fmt<decadays>(lt, &abbrev));
  VERIFY( res == WIDEN("2024-03-18 00:00:00 Zone") );
  res = std::format(WIDEN("{}"), local_fmt<kilodays>(lt, &abbrev));
  VERIFY( res == WIDEN("2022-01-08 00:00:00 Zone") );
}

template<typename CharT>
void
test_time_points()
{
  test_time_point<CharT, local_t, true>();
  test_time_point<CharT, system_clock, true>();
  test_time_point<CharT, utc_clock, false>();
  test_time_point<CharT, tai_clock, false>();
  test_time_point<CharT, gps_clock, false>();
  test_time_point<CharT, file_clock, false>();
  test_leap_second<CharT>();
#if _GLIBCXX_USE_CXX11_ABI || !_GLIBCXX_USE_DUAL_ABI
  test_zoned_time<CharT>();
#endif
  test_local_time_format<CharT>();

}

#if _GLIBCXX_USE_CXX11_ABI || !_GLIBCXX_USE_DUAL_ABI
template<typename CharT>
void
test_sys_info()
{
  const sys_info si
  {
    sys_days(2024y/March/22) + 2h,
    sys_days(2025y/April/11) + 23h + 15min + 10s,
    2h + 13min + 4s,
    15min,
    "Zone"
  };
  const std::basic_string_view<CharT> txt
    = WIDEN("[2024-03-22 02:00:00,2025-04-11 23:15:10,02:13:04,15min,Zone]");

  verify( si, txt );

  std::basic_string<CharT> res;
  std::basic_string_view<CharT> sv;

  sv = res = std::format(WIDEN("{:65}"), si);
  VERIFY( sv.ends_with(WIDEN("    ")) );
  sv.remove_suffix(4);
  VERIFY( sv == txt );

  sv = res = std::format(WIDEN("{:=^67}"), si);
  VERIFY( sv.starts_with(WIDEN("===")) );
  VERIFY( sv.ends_with(WIDEN("===")) );
  sv.remove_prefix(3);
  sv.remove_suffix(3);
  VERIFY( sv == txt );
}

template<typename CharT>
void test_local_info()
{
  using String = std::basic_string<CharT>;
  using StringView = std::basic_string_view<CharT>;

  const sys_info s1
  {
    sys_days(2015y/September/11) + 2h,
    sys_days(2016y/March/13) + 2h,
    -5h,
    0h,
    "EET"
  };
  const sys_info s2
  {
    sys_days(2016y/March/13) + 2h,
    sys_days(2015y/September/15) + 2h,
    -4h,
    1h,
    "EDT"
  };

  const StringView single
    = WIDEN("[2015-09-11 02:00:00,2016-03-13 02:00:00,-05:00:00,0min,EET]");
  const StringView both
    = WIDEN(" local time between "
	    "[2015-09-11 02:00:00,2016-03-13 02:00:00,-05:00:00,0min,EET]"
	    " and "
	    "[2016-03-13 02:00:00,2015-09-15 02:00:00,-04:00:00,60min,EDT]");

  const local_info l1{local_info::nonexistent, s1, s2};
  auto exp = WIDEN("[nonexistent") + String(both) + WIDEN("]");
  verify( l1, StringView(exp) );

  const local_info l2{local_info::ambiguous, s1, s2};
  exp = WIDEN("[ambiguous") + String(both) + WIDEN("]");
  verify( l2, StringView(exp) );

  const local_info l3{local_info::unique, s1, s1};
  exp = WIDEN("[") + String(single) + WIDEN("]");
  verify( l3, StringView(exp) );

  String res;
  StringView sv;

  sv = res = std::format(WIDEN("{:65}"), l3);
  VERIFY( sv.ends_with(WIDEN("   ")) );
  sv.remove_suffix(3);
  VERIFY( sv == exp );

  sv = res = std::format(WIDEN("{:=^67}"), l3);
  VERIFY( sv.starts_with(WIDEN("==")) );
  VERIFY( sv.ends_with(WIDEN("===")) );
  sv.remove_prefix(2);
  sv.remove_suffix(3);
  VERIFY( sv == exp );
}

template<typename CharT>
void
test_infos()
{
  test_sys_info<CharT>();
  test_local_info<CharT>();
}
#endif

template<typename CharT>
void
test_all()
{
  test_padding<CharT>();
  test_durations<CharT>();
  test_calendar<CharT>();
  test_time_points<CharT>();
#if _GLIBCXX_USE_CXX11_ABI || !_GLIBCXX_USE_DUAL_ABI
  test_infos<CharT>();
#endif
}

int main()
{
  test_all<char>();

#ifdef _GLIBCXX_USE_WCHAR_T
  test_all<wchar_t>();
#endif // _GLIBCXX_USE_WCHAR_T
}
