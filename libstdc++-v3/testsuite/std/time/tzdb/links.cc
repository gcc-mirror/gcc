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

using namespace std::chrono;

void
test_link_chains()
{
  std::ofstream("tzdata.zi") << R"(# version test_1
Link  Greenwich  G_M_T
Link  Etc/GMT    Greenwich
Zone  Etc/GMT  0  -  GMT
Zone  A_Zone   1  -  ZON
Link  A_Zone L1
Link  L1 L2
Link  L2 L3
Link  L3 L4
Link  L4 L5
Link  L5 L6
Link  L3 L7
)";

  const auto& db = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db.version == "test_1" );

  // Simple case of a link with a zone as its target.
  VERIFY( locate_zone("Greenwich")->name() == "Etc/GMT" );
  // Chains of links, where the target may be another link.
  VERIFY( locate_zone("G_M_T")->name() == "Etc/GMT" );
  VERIFY( locate_zone("L1")->name() == "A_Zone" );
  VERIFY( locate_zone("L2")->name() == "A_Zone" );
  VERIFY( locate_zone("L3")->name() == "A_Zone" );
  VERIFY( locate_zone("L4")->name() == "A_Zone" );
  VERIFY( locate_zone("L5")->name() == "A_Zone" );
  VERIFY( locate_zone("L6")->name() == "A_Zone" );
  VERIFY( locate_zone("L7")->name() == "A_Zone" );
}

void
test_bad_links()
{
  // The zic(8) man page says
  // > the behavior is unspecified if multiple zone or link lines
  // > define the same name"
  // For libstdc++ the expected behaviour is described and tested below.
  std::ofstream("tzdata.zi") << R"(# version test_2
Zone A_Zone   1  -  ZA
Zone B_Zone   2  -  ZB
Link A_Zone B_Zone
Link B_Zone C_Link
Link C_Link D_Link
Link D_Link E_Link
)";

  const auto& db2 = reload_tzdb();
  VERIFY( override_used ); // If this fails then XFAIL for the target.
  VERIFY( db2.version == "test_2" );

  // The standard requires locate_zone(name) to search for a zone first,
  // so this finds the zone B_Zone, not the link that points to zone A_Zone.
  VERIFY( locate_zone("B_Zone")->name() == "B_Zone" );
  // And libstdc++ does the same at every step when following chained links:
  VERIFY( locate_zone("C_Link")->name() == "B_Zone" );
  VERIFY( locate_zone("D_Link")->name() == "B_Zone" );
  VERIFY( locate_zone("E_Link")->name() == "B_Zone" );

  // The zic(8) man page says
  // > the behavior is unspecified if a chain of one or more links
  // > does not terminate in a Zone name.
  // For libstdc++ we throw std::runtime_error if locate_zone finds an
  // unterminated chain, including the case of a chain that includes a cycle.
  std::ofstream("tzdata.zi") << R"(# version test_3
Zone A_Zone   1  -  ZON
Link A_Zone GoodLink
Link No_Zone BadLink
Link LinkSelf LinkSelf
Link LinkSelf Link1
Link Link1 Link2
Link Cycle2_A Cycle2_B
Link Cycle2_B Cycle2_A
Link Cycle3_A Cycle3_B
Link Cycle3_B Cycle3_C
Link Cycle3_C Cycle3_A
Link Cycle3_C Cycle3_D
Link Cycle4_A Cycle4_B
Link Cycle4_B Cycle4_C
Link Cycle4_C Cycle4_D
Link Cycle4_D Cycle4_A
)";

  const auto& db3 = reload_tzdb();
  VERIFY( db3.version == "test_3" );

  // Lookup for valid links should still work even if other links are bad.
  VERIFY( locate_zone("GoodLink")->name() == "A_Zone" );

#if __cpp_exceptions
  try {
    locate_zone("BadLink");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("cannot locate zone: BadLink") );
  }

  // LinkSelf forms a link cycle with itself.
  try {
    locate_zone("LinkSelf");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: LinkSelf") );
  }

  // Any chain that leads to LinkSelf reaches a cycle.
  try {
    locate_zone("Link1");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Link1") );
  }

  try {
    locate_zone("Link2");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Link2") );
  }

  // Cycle2_A and Cycle2_B form a cycle of length two.
  try {
    locate_zone("Cycle2_A");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Cycle2_A") );
  }

  try {
    locate_zone("Cycle2_B");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Cycle2_B") );
  }

  // Cycle3_A, Cycle3_B and Cycle3_C form a cycle of length three.
  try {
    locate_zone("Cycle3_A");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Cycle3_A") );
  }

  try {
    locate_zone("Cycle3_B");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Cycle3_B") );
  }

  try {
    locate_zone("Cycle3_C");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Cycle3_C") );
  }

  // Cycle3_D isn't part of the cycle, but it leads to it.
  try {
    locate_zone("Cycle3_D");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Cycle3_D") );
  }

  // Cycle4_* links form a cycle of length four.
  try {
    locate_zone("Cycle4_A");
    VERIFY( false );
  } catch (const std::runtime_error& e) {
    std::string_view what(e.what());
    VERIFY( what.ends_with("link cycle: Cycle4_A") );
  }
#endif
}

int main()
{
  test_link_chains();
  test_bad_links();
}
