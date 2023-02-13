// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

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
  VERIFY( s == "Mon | Monday | Dec | December | Mon Dec 19 17:26:25.708 2022"
	       " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
	       " | 17 | 05 | 353 | 12 | 26 | PM | 05:26:25.708 PM | 17:26"
	       " | 25.708 | 17:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
	       " | 17:26:25.708 | 22 | 2022 | +0000 | UTC | " );

  std::wstring ws = wss.str();
  VERIFY( ws == L"Mon | Monday | Dec | December | Mon Dec 19 17:26:25.708 2022"
		 " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
		 " | 17 | 05 | 353 | 12 | 26 | PM | 05:26:25.708 PM | 17:26"
		 " | 25.708 | 17:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
		 " | 17:26:25.708 | 22 | 2022 | +0000 | UTC | " );

  std::chrono::utc_seconds leap(1483228800s + 26s); // 1 Jan 2017
  s = std::format("{:%T}", leap - 1s);
  VERIFY( s == "23:59:59" );
  s = std::format("{:%T}", leap);
  VERIFY( s == "23:59:60" );
  s = std::format("{:%T}", leap + 10ms);
  VERIFY( s == "23:59:60.010" );

  s = std::format("{:%T}", leap + 1s);
  VERIFY( s == "00:00:00" );
}

int main()
{
  test_ostream();
  test_format();
}
