// { dg-do run { target c++20 } }
// { dg-require-effective-target cxx11_abi }
// { dg-timeout-factor 2 }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using namespace std::chrono;
  std::stringstream ss;
  zoned_time<seconds> zt("America/New_York", sys_seconds{946'706'523s});
  ss << zt;
  VERIFY( ss.str() == "2000-01-01 01:02:03 EST" );
}

void
test_format()
{
  using namespace std::chrono;
  sys_time<milliseconds> t(1671470785708ms);
  auto zone = "America/New_York";
  zoned_time<milliseconds> zt(zone, t);

  // Every conversion specifier is valid for a sys_time except %q and %Q.

  std::string s = std::format("{:%a | %A | %b | %B | %c"
			      " | %C | %d | %D | %e | %F | %g | %G | %h"
			      " | %H | %I | %j | %m | %M | %p | %r | %R"
			      " | %S | %T | %u | %U | %V | %w | %W | %x"
			      " | %X | %y | %Y | %z | %Z}", zt);
  VERIFY( s == "Mon | Monday | Dec | December | Mon Dec 19 12:26:25 2022"
	       " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
	       " | 12 | 12 | 353 | 12 | 26 | PM | 12:26:25 PM | 12:26"
	       " | 25.708 | 12:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
	       " | 12:26:25 | 22 | 2022 | -0500 | EST" );

#ifdef _GLIBCXX_USE_WCHAR_T
  std::wstring ws = std::format(L"{:%a | %A | %b | %B | %c"
				 " | %C | %d | %D | %e | %F | %g | %G | %h"
				 " | %H | %I | %j | %m | %M | %p | %r | %R"
				 " | %S | %T | %u | %U | %V | %w | %W | %x"
				 " | %X | %y | %Y | %z | %Z}", zt);
  VERIFY( ws == L"Mon | Monday | Dec | December | Mon Dec 19 12:26:25 2022"
		 " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
		 " | 12 | 12 | 353 | 12 | 26 | PM | 12:26:25 PM | 12:26"
		 " | 25.708 | 12:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
		 " | 12:26:25 | 22 | 2022 | -0500 | EST" );
#endif

  auto loc = std::locale::classic();
  auto smod = std::format(loc, "{:%Ec %EC %Od %Oe %OH %OI %Om %OM %OS %Ou %OU"
			       " %Ow %OW %Ex %EX %Oy %Ey %EY %Ez %Oz}", zt);
  s = std::format("{:%c %C %d %e %H %I %m %M %S %u %U"
		  " %w %W %x %X %y %y %Y -05:00 -05:00}",
		  zoned_time<seconds>(zone, time_point_cast<seconds>(t)));
  VERIFY( smod == s );

  s = std::format("{}", zt);
  VERIFY( s == "2022-12-19 12:26:25.708 EST" );
  s = std::format("{1:=>30}", 1, zt);
  VERIFY( s == "===2022-12-19 12:26:25.708 EST" );
#ifdef _GLIBCXX_USE_WCHAR_T
  ws = std::format(L"{:+^34}", zoned_time<microseconds>(zone, t));
  VERIFY( ws == L"++2022-12-19 12:26:25.708000 EST++" );
#endif

  // LWG 4124. Cannot format zoned_time with resolution coarser than seconds
  s = std::format("{}", zoned_time<minutes>(zone, time_point_cast<minutes>(t)));
  VERIFY( s == "2022-12-19 12:26:00 EST" );
}

int main()
{
  test_ostream();
  test_format();
}
