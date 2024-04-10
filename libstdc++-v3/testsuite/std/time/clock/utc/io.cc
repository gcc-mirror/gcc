// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using std::ostringstream;
  using namespace std::chrono;

  auto t = sys_days{July/1/2015} - 500ms;
  auto u = clock_cast<utc_clock>(t);

  std::string_view results[] = {
    "2015-06-30 23:59:59.500 UTC",
    "2015-06-30 23:59:59.750 UTC",
    "2015-06-30 23:59:60.000 UTC",
    "2015-06-30 23:59:60.250 UTC",
    "2015-06-30 23:59:60.500 UTC",
    "2015-06-30 23:59:60.750 UTC",
    "2015-07-01 00:00:00.000 UTC",
    "2015-07-01 00:00:00.250 UTC",
  };

  for (auto result : results)
  {
    ostringstream out;
    out << u << " UTC";
    VERIFY( out.str() == result );
    u += 250ms;
  }
}

void
test_format()
{
  using namespace std::chrono_literals;
  std::chrono::utc_time<std::chrono::milliseconds> t(1671470812708ms);

  // Every conversion specifier is valid for a utc_time except %q and %Q.
  std::string_view specs = "aAbBcCdDeFgGhHIjmMpqQrRSTuUVwWxXyYzZ";
  std::string_view badspecs = "qQ";

  std::ostringstream ss;
  std::wostringstream wss;
  const auto& ct = std::use_facet<std::ctype<wchar_t>>(wss.getloc());

  for (char c : specs)
  {
    char fmt[] = { '{', ':', '%', c, '}' };
    try
    {
      ss << std::vformat(std::string_view(fmt, 5),
			 std::make_format_args(t));
      ss << " | ";
      // The call above should throw for any conversion-spec in badspecs:
      VERIFY(badspecs.find(c) == badspecs.npos);

    }
    catch (const std::format_error& e)
    {
      VERIFY(badspecs.find(c) != badspecs.npos);
      std::string_view s = e.what();
      // Libstdc++-specific message:
      VERIFY(s.find("format argument does not contain the information "
		    "required by the chrono-specs") != s.npos);
    }

    wchar_t wfmt[] = { L'{', L':', L'%', ct.widen(c), L'}' };
    try
    {
      wss << std::vformat(std::wstring_view(wfmt, 5),
			  std::make_wformat_args(t));
      wss << L" | ";
      // The call above should throw for any conversion-spec in badspecs:
      VERIFY(badspecs.find(c) == badspecs.npos);
    }
    catch (const std::format_error& e)
    {
      VERIFY(badspecs.find(c) != badspecs.npos);
      std::string_view s = e.what();
      // Libstdc++-specific message:
      VERIFY(s.find("format argument does not contain the information "
		    "required by the chrono-specs") != s.npos);
    }
  }

  std::string s = ss.str();
  VERIFY( s == "Mon | Monday | Dec | December | Mon Dec 19 17:26:25 2022"
	       " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
	       " | 17 | 05 | 353 | 12 | 26 | PM | 05:26:25 PM | 17:26"
	       " | 25.708 | 17:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
	       " | 17:26:25 | 22 | 2022 | +0000 | UTC | " );

  std::wstring ws = wss.str();
  VERIFY( ws == L"Mon | Monday | Dec | December | Mon Dec 19 17:26:25 2022"
		 " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
		 " | 17 | 05 | 353 | 12 | 26 | PM | 05:26:25 PM | 17:26"
		 " | 25.708 | 17:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
		 " | 17:26:25 | 22 | 2022 | +0000 | UTC | " );

  std::chrono::utc_seconds leap(1483228800s + 26s); // 1 Jan 2017
  s = std::format("{:%T}", leap - 1s);
  VERIFY( s == "23:59:59" );
  s = std::format("{:%T}", leap);
  VERIFY( s == "23:59:60" );
  s = std::format("{:%T}", leap + 10ms);
  VERIFY( s == "23:59:60.010" );

  s = std::format("{:%T}", leap + 1s);
  VERIFY( s == "00:00:00" );

  // PR libstdc++/113500
  s = std::format("{}", leap + 100ms + 2.5s);
  VERIFY( s == "2017-01-01 00:00:01.600");
}

void
test_parse()
{
  using namespace std::chrono;
  const sys_seconds expected = sys_days(2023y/August/9) + 20h + 44min + 3s;
  utc_seconds tp;

  minutes offset;
  std::string abbrev;
  std::istringstream is("23 2210 21:44:3 +1 BST#");
  VERIFY( is >> parse("%y %j0 %4H:%5M:%6S %Oz %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<utc_clock>(expected) );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );

  tp = {};
  is.clear();
  is.str("20230809214403  0100  BST:");
  VERIFY( is >> parse("%Y%m%d%H%M%S %z %Z:", tp) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<utc_clock>(expected) );

  is.clear();
  is.str("2023-W32-3 20:44:03");
  VERIFY( is >> parse("%G-W%V-%u %T", tp) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<utc_clock>(expected) );
}

int main()
{
  test_ostream();
  test_format();
  test_parse();
}
