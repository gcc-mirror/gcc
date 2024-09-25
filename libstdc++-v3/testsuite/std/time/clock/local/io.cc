// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <format>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using namespace std::chrono;

  auto lt = local_time<seconds>(1722198122s);

  std::ostringstream ss;
  ss << lt;
  auto s = ss.str();
  ss.str("");
  ss.clear();
  ss << sys_time<seconds>{lt.time_since_epoch()};
  auto s2 = ss.str();
  VERIFY( s == s2 );
}

void
test_format()
{
  using std::format;
  using namespace std::chrono;

  auto lt = local_seconds(1722198122s);

#if __cpp_exceptions
  auto args = std::make_format_args(lt);
  try
  {
    (void) std::vformat("{:%Z}", args);
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
  }
  try
  {
    (void) std::vformat("{:%z}", args);
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
  }
#endif

  auto s = format("{0:%F %T %a}", lt);
  VERIFY( s == "2024-07-28 20:22:02 Sun" );

  s = format("{}", lt);
  VERIFY( s == "2024-07-28 20:22:02" );

  // Test formatting for chrono::local_time_format and local-time-format-t too:
  auto ltf = local_time_format(lt);
  s = std::format("{:%F %T %a %b}", ltf);
  VERIFY( s == "2024-07-28 20:22:02 Sun Jul" );
#if __cpp_exceptions
  try
  {
    (void) std::format("{:%Z}", ltf);
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
  }
  try
  {
    (void) std::format("{:%z}", ltf);
    VERIFY(false);
  }
  catch (const std::format_error&)
  {
  }
#endif
  std::string abbrev = "FOO";
  seconds off = -3600s;
  ltf = local_time_format(lt, &abbrev, &off);
  s = std::format("{}", ltf);
  VERIFY( s == "2024-07-28 20:22:02 FOO" );
  s = std::format("{:%Z %T %F %z %Ez}", ltf);
  __builtin_puts(s.c_str());
  VERIFY( s == "FOO 20:22:02 2024-07-28 -0100 -01:00" );

  s = std::format("{}", local_seconds{});
  VERIFY( s == "1970-01-01 00:00:00" );
}

void
test_parse()
{
  using namespace std::chrono;
  const sys_seconds expected = sys_days(2023y/August/9) + 21h + 44min;
  local_seconds tp;

  minutes offset;
  std::string abbrev;
  std::istringstream is("2023-8-9 21:44 +1 BST#"); // Not adjusted for offset.
  VERIFY( is >> parse("%F %R %Oz %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == local_seconds(expected.time_since_epoch()) );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );

  // Test round trip
  std::stringstream ss;
  ss << local_seconds{expected.time_since_epoch()} << " X 0123";
  VERIFY( ss >> parse("%F %T %Z %z", tp, abbrev, offset) );
  VERIFY( ! ss.eof() );
  VERIFY( tp == local_seconds{expected.time_since_epoch()} );
  VERIFY( abbrev == "X" );
  VERIFY( offset == (1h + 23min) );

  ss.str("");
  ss << local_seconds{};
  VERIFY( ss >> parse("%F %T", tp) );
  VERIFY( tp.time_since_epoch() == 0s );
}

int main()
{
  test_ostream();
  test_parse();
}
