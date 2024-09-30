// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using namespace std::chrono;
  std::stringstream ss;
  ss << sys_seconds{0s} << '\n';                // 1970-01-01 00:00:00
  ss << sys_seconds{946'684'800s} << '\n';      // 2000-01-01 00:00:00
  ss << sys_seconds{946'688'523s} << '\n';      // 2000-01-01 01:02:03
  std::string s1, s2, s3;
  std::getline(ss, s1);
  std::getline(ss, s2);
  std::getline(ss, s3);
  VERIFY( s1 == "1970-01-01 00:00:00" );
  VERIFY( s2 == "2000-01-01 00:00:00" );
  VERIFY( s3 == "2000-01-01 01:02:03" );
}

template<typename T>
concept stream_insertable
  = requires (std::ostream& out, const T& t) { out << t; };

// operator<<(ostream&, const sys_time<D>&) is constrained to not
// allow floating-point types or periods of days or greater.
using fp_sys_time = std::chrono::sys_time<std::chrono::duration<float>>;
static_assert( !stream_insertable<fp_sys_time> );

// But there is an overload for sys_days.
static_assert( stream_insertable<std::chrono::sys_days> );

void
test_format()
{
  using namespace std::chrono_literals;
  std::chrono::sys_time<std::chrono::milliseconds> t(1671470785708ms);

  // Every conversion specifier is valid for a sys_time except %q and %Q.

  std::string s = std::format("{:%a | %A | %b | %B | %c"
			      " | %C | %d | %D | %e | %F | %g | %G | %h"
			      " | %H | %I | %j | %m | %M | %p | %r | %R"
			      " | %S | %T | %u | %U | %V | %w | %W | %x"
			      " | %X | %y | %Y | %z | %Z}", t);
  VERIFY( s == "Mon | Monday | Dec | December | Mon Dec 19 17:26:25 2022"
	       " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
	       " | 17 | 05 | 353 | 12 | 26 | PM | 05:26:25 PM | 17:26"
	       " | 25.708 | 17:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
	       " | 17:26:25 | 22 | 2022 | +0000 | UTC" );

  std::wstring ws = std::format(L"{:%a | %A | %b | %B | %c"
				 " | %C | %d | %D | %e | %F | %g | %G | %h"
				 " | %H | %I | %j | %m | %M | %p | %r | %R"
				 " | %S | %T | %u | %U | %V | %w | %W | %x"
				 " | %X | %y | %Y | %z | %Z}", t);
  VERIFY( ws == L"Mon | Monday | Dec | December | Mon Dec 19 17:26:25 2022"
		 " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
		 " | 17 | 05 | 353 | 12 | 26 | PM | 05:26:25 PM | 17:26"
		 " | 25.708 | 17:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
		 " | 17:26:25 | 22 | 2022 | +0000 | UTC" );

  auto st = std::chrono::time_point_cast<std::chrono::seconds>(t);
  auto loc = std::locale::classic();
  auto smod = std::format(loc, "{:%Ec %EC %Od %Oe %OH %OI %Om %OM %OS %Ou %OU"
			       " %Ow %OW %Ex %EX %Oy %Ey %EY %Ez %Oz}", t);
  s = std::format("{:%c %C %d %e %H %I %m %M %S %u %U"
		  " %w %W %x %X %y %y %Y +00:00 +00:00}",
		  st);
  VERIFY( smod == s );

  s = std::format("{}", t);
  VERIFY( s == "2022-12-19 17:26:25.708" );
  s = std::format("{0:} {0:=<21}", st);
  VERIFY( s == "2022-12-19 17:26:25 2022-12-19 17:26:25==" );
}

void
test_parse()
{
  using namespace std::chrono;
  sys_seconds tp, expected = sys_days(2023y/July/24) + 13h + 05min;

  std::istringstream is("24-hour time: 2023-07-24 13:05");
  VERIFY( is >> parse("24-hour time: %Y-%m-%d %H:%M", tp) );
  VERIFY( ! is.eof() );
  VERIFY( tp == expected );

  tp = {};
  is.clear();
  is.str("12-hour time: 2023-07-24 1.05 PM ");
  VERIFY( is >> parse("12-hour time: %F %I.%M %p", tp) );
  VERIFY( ! is.eof() );
  VERIFY( tp == expected );

  tp = {};
  is.clear();
  is.str("2023-07-24 14:05 +01");
  VERIFY( is >> parse("%F %H:%M %z", tp) ); // %z is used even without offset
  VERIFY( is.eof() );
  VERIFY( tp == expected );

  tp = {};
  minutes offset{};
  is.clear();
  is.str("2023-07-24 15:35 0230");
  VERIFY( is >> parse("%F %H:%M %z", tp, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == expected );

  tp = {};
  std::string abbrev;
  is.clear();
  is.str("2023-07-24 08:05 -5:00 EST EST");
  VERIFY( is >> parse("%F %H:%M %Ez %Z %Z", tp, abbrev) );
  VERIFY( is.eof() );
  VERIFY( tp == expected );
  VERIFY( abbrev == "EST" );

  tp = {};
  abbrev = {};
  offset = {};
  is.clear();
  is.str("2023-07-24 07:05 -06:00 ABC/+123/-456/_=");
  VERIFY( is >> parse("%F %H:%M %Ez %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == expected );
  VERIFY( offset == -360min );
  VERIFY( abbrev == "ABC/+123/-456/_" );

  tp = sys_seconds(99s);
  offset = 99min;
  is.clear();
  is.str("-02:00 ");
  VERIFY( ! (is >> parse("%Ez ", tp, offset)) );
  VERIFY( is.fail() );
  VERIFY( tp == sys_seconds(99s) ); // tp is only updated on successful parse.
  VERIFY( offset == 99min ); // offset is only updated on successful parse.

  tp = sys_seconds(99s);
  abbrev = "99";
  is.clear();
  is.str("GMT ");
  VERIFY( ! (is >> parse("%Z ", tp, abbrev)) );
  VERIFY( is.fail() );
  VERIFY( tp == sys_seconds(99s) ); // tp is only updated on successful parse.
  VERIFY( abbrev == "99" ); // abbrev is only updated on successful parse.
}

int main()
{
  test_ostream();
  test_format();
  test_parse();
}
