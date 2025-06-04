// { dg-do run { target c++20 } }
// { dg-require-effective-target hosted }
// { dg-timeout-factor 2 }

#include <chrono>
#include <ranges>
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

template<typename Ret = void>
struct Rep
{
  using Return
    = std::conditional_t<std::is_void_v<Ret>, Rep, Ret>;

  Rep(long v = 0) : val(v) {}

  operator long() const
  { return val; }

  Return
  operator+() const
  { return val; }

  Rep
  operator-() const
  { return -val; }

  friend Rep
  operator+(Rep lhs, Rep rhs)
  { return lhs.val + rhs.val; }

  friend Rep
  operator-(Rep lhs, Rep rhs)
  { return lhs.val - rhs.val; }

  friend Rep
  operator*(Rep lhs, Rep rhs)
  { return lhs.val * rhs.val; }

  friend Rep
  operator/(Rep lhs, Rep rhs)
  { return lhs.val / rhs.val; }

  friend auto operator<=>(Rep, Rep) = default;

  template<typename _CharT>
  friend std::basic_ostream<_CharT>&
  operator<<(std::basic_ostream<_CharT>& os, const Rep& t)
  { return os << t.val << WIDEN("[via <<]"); }

  long val;
};

template<typename Ret, typename Other>
  requires std::is_integral_v<Other>
struct std::common_type<Rep<Ret>, Other>
{
  using type = Rep<Ret>;
};

template<typename Ret, typename Other>
  requires std::is_integral_v<Other>
struct std::common_type<Other, Rep<Ret>>
  : std::common_type<Rep<Ret>, Other>
{ };

template<typename Ret>
struct std::numeric_limits<Rep<Ret>>
  : std::numeric_limits<long>
{ };

template<typename Ret, typename _CharT>
struct std::formatter<Rep<Ret>, _CharT>
  : std::formatter<long, _CharT>
{
  template<typename Out>
  typename std::basic_format_context<Out, _CharT>::iterator
  format(const Rep<Ret>& t, std::basic_format_context<Out, _CharT>& ctx) const
  {
    constexpr std::basic_string_view<_CharT> suffix = WIDEN("[via format]");
    auto out = std::formatter<long, _CharT>::format(t.val, ctx);
    return std::ranges::copy(suffix, out).out;
  }
};

using deciseconds = duration<seconds::rep, std::deci>;

template<typename _CharT>
void
test_duration()
{
  std::basic_string<_CharT> res;

  const milliseconds di(40);
  verify( di, WIDEN("40ms") );
  res = std::format(WIDEN("{:>6}"), di);
  VERIFY( res == WIDEN("  40ms") );

  verify( -di, WIDEN("-40ms") );
  res = std::format(WIDEN("{:>6}"), -di);
  VERIFY( res == WIDEN(" -40ms") );

  const duration<double> df(11.22);
  verify( df, WIDEN("11.22s") );
  res = std::format(WIDEN("{:=^12}"), df);
  VERIFY( res == WIDEN("===11.22s===") );

  verify( -df, WIDEN("-11.22s") );
  res = std::format(WIDEN("{:=^12}"), -df);
  VERIFY( res == WIDEN("==-11.22s===") );
}

template<typename _CharT>
void
test_duration_cust()
{
  std::basic_string<_CharT> res;
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

template<typename _CharT>
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

template<typename _CharT>
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
	  WIDEN("22:24:54.123[via format]") );
  verify( hms<deciseconds>(asLong),
	  WIDEN("22:24:54.1[via format]") );
  verify( hms<seconds>(asLong),
	  WIDEN("22:24:54") );
  verify( hms<milliseconds>(-asLong),
	  WIDEN("-22:24:54.123[via format]") );
  verify( hms<deciseconds>(-asLong),
	  WIDEN("-22:24:54.1[via format]") );
  verify( hms<seconds>(-asLong),
	  WIDEN("-22:24:54") );

  // +asRep returns Rep<>, so formatted as Rep<>
  const duration<Rep<>, std::milli> asRep(dt.count());
  verify( hms<milliseconds>(asRep),
	  WIDEN("22:24:54.123[via format]") );
  verify( hms<deciseconds>(asRep),
	  WIDEN("22:24:54.1[via format]") );
  verify( hms<seconds>(asLong),
	  WIDEN("22:24:54") );
  verify( hms<milliseconds>(-asLong),
	  WIDEN("-22:24:54.123[via format]") );
  verify( hms<deciseconds>(-asLong),
	  WIDEN("-22:24:54.1[via format]") );
  verify( hms<seconds>(-asLong),
	  WIDEN("-22:24:54") );
}

template<typename CharT>
void
test_durations()
{
  test_duration<CharT>();
  test_duration_cust<CharT>();

  test_hh_mm_ss<CharT>();
  test_hh_mm_ss_cust<CharT>();
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
  test_durations<CharT>();
  test_calendar<CharT>();
}

int main()
{
  test_all<char>();

#ifdef _GLIBCXX_USE_WCHAR_T
  test_all<wchar_t>();
#endif // _GLIBCXX_USE_WCHAR_T
}
