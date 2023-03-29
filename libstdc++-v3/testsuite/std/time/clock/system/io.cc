// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

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
  VERIFY( s == "Mon | Monday | Dec | December | Mon Dec 19 17:26:25.708 2022"
	       " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
	       " | 17 | 05 | 353 | 12 | 26 | PM | 05:26:25.708 PM | 17:26"
	       " | 25.708 | 17:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
	       " | 17:26:25.708 | 22 | 2022 | +0000 | UTC" );

  std::wstring ws = std::format(L"{:%a | %A | %b | %B | %c"
				 " | %C | %d | %D | %e | %F | %g | %G | %h"
				 " | %H | %I | %j | %m | %M | %p | %r | %R"
				 " | %S | %T | %u | %U | %V | %w | %W | %x"
				 " | %X | %y | %Y | %z | %Z}", t);
  VERIFY( ws == L"Mon | Monday | Dec | December | Mon Dec 19 17:26:25.708 2022"
		 " | 20 | 19 | 12/19/22 | 19 | 2022-12-19 | 22 | 2022 | Dec"
		 " | 17 | 05 | 353 | 12 | 26 | PM | 05:26:25.708 PM | 17:26"
		 " | 25.708 | 17:26:25.708 | 1 | 51 | 51 | 1 | 51 | 12/19/22"
		 " | 17:26:25.708 | 22 | 2022 | +0000 | UTC" );
}

int main()
{
  test_ostream();
  test_format();
}
