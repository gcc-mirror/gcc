// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }
// { dg-xfail-run-if "no weak override on AIX" { powerpc-ibm-aix* } }

#include <chrono>
#include <fstream>
#include <testsuite_hooks.h>

static bool override_used = false;

namespace __gnu_cxx
{
  const char* zoneinfo_dir_override() {
    override_used = true;
    return "./";
  }
}

void
test_load_leapseconds()
{
  std::ofstream("leapseconds") << R"(
# These are all the real leap seconds as of 2022:
Leap	1972	Jun	30	23:59:60	+	S
Leap	1972	Dec	31	23:59:60	+	S
Leap	1973	Dec	31	23:59:60	+	S
Leap	1974	Dec	31	23:59:60	+	S
Leap	1975	Dec	31	23:59:60	+	S
Leap	1976	Dec	31	23:59:60	+	S
Leap	1977	Dec	31	23:59:60	+	S
Leap	1978	Dec	31	23:59:60	+	S
Leap	1979	Dec	31	23:59:60	+	S
Leap	1981	Jun	30	23:59:60	+	S
Leap	1982	Jun	30	23:59:60	+	S
Leap	1983	Jun	30	23:59:60	+	S
Leap	1985	Jun	30	23:59:60	+	S
Leap	1987	Dec	31	23:59:60	+	S
Leap	1989	Dec	31	23:59:60	+	S
Leap	1990	Dec	31	23:59:60	+	S
Leap	1992	Jun	30	23:59:60	+	S
Leap	1993	Jun	30	23:59:60	+	S
Leap	1994	Jun	30	23:59:60	+	S
Leap	1995	Dec	31	23:59:60	+	S
Leap	1997	Jun	30	23:59:60	+	S
Leap	1998	Dec	31	23:59:60	+	S
Leap	2005	Dec	31	23:59:60	+	S
Leap	2008	Dec	31	23:59:60	+	S
Leap	2012	Jun	30	23:59:60	+	S
Leap	2015	Jun	30	23:59:60	+	S
Leap	2016	Dec	31	23:59:60	+	S
# These are fake leap seconds for testing purposes:
Leap	2093	Jun	30	23:59:59	-	S
Leap	2093	Dec	31	23:59:60	+	S
)";

  const auto& db = std::chrono::get_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.

  using namespace std::chrono;
  // XXX update this value if the number of hardcoded leap seconds changes:
  VERIFY( db.leap_seconds.size() == 29 );

  auto i = db.leap_seconds.end() - 2;

  VERIFY( i[0].date() == sys_days(2093y/July/1) - 1s );
  VERIFY( i[0].value() == -1s );

  VERIFY( i[1].date() == sys_days(2094y/January/1) );
  VERIFY( i[1].value() == 1s );
}

int main()
{
  test_load_leapseconds();
}
