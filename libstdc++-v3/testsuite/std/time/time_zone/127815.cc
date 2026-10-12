// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }

#include <chrono>
#include <fstream>
#include <testsuite_hooks.h>

namespace __gnu_cxx
{
  // libstdc++ hook: read tzdata.zi from this directory instead of the system's.
  const char* zoneinfo_dir_override() { return "."; }
}

static void
test_pr127815()
{
  using namespace std::chrono;
  const sys_seconds ymin{sys_days{year::min() / January / 1}};

  static int version = 0;

  for (const char* name : {"Plain", "Test"})
    for (sys_seconds t : {sys_seconds::min(), ymin - 1s})
    {
      std::ofstream("tzdata.zi")
	<< "# version test" << ++version << "\n"
	   "R X 2000 ma - Mar lastSu 2 1 D\n"
	   "R X 2000 ma - O lastSu 2 0 S\n"
	   "Z Test -6 X T%sT\n"
	   "Z Plain -6 - PST\n";

      sys_info info = reload_tzdb().locate_zone(name)->get_info(t);
      VERIFY(info.abbrev == name[0] + std::string("ST"));
    }
}

int main()
{
  test_pr127815();
}
