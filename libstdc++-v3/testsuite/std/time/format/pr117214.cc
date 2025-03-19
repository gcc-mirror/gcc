// { dg-do run { target c++20 } }
// { dg-require-namedlocale "aa_DJ.UTF-8" }
// { dg-require-namedlocale "ar_SA.UTF-8" }
// { dg-require-namedlocale "ca_AD.UTF-8" }
// { dg-require-namedlocale "az_IR.UTF-8" }
// { dg-require-namedlocale "my_MM.UTF-8" }

#include <chrono>
#include <locale>
#include <testsuite_hooks.h>

void
test_c()
{
  const char *test_locales[] = {
    "aa_DJ.UTF-8",
    "ar_SA.UTF-8",
    "ca_AD.UTF-8",
    "az_IR.UTF-8",
    "my_MM.UTF-8",
  };
  std::chrono::sys_seconds t{std::chrono::seconds{1}};

  for (auto locale_name : test_locales)
  {
    auto s = std::format(std::locale(locale_name), "{:L%c}", t);
    VERIFY( !s.empty() );
  }
}

#include <stdlib.h>
#include <time.h>

extern "C++" int tzset(int);

template<typename T = void>
void maybe_set_tz()
{
#ifdef _GLIBCXX_HAVE_SETENV
  // True if a function 'T tzset()' has been declared.
  if constexpr (requires (T (*p)()) { p = &tzset; })
  {
    setenv("TZ", "GMT0", 1);
    tzset(); // Set C library's global time zone to GMT0
  }
#endif
}

void
test_c_zoned()
{
#if _GLIBCXX_USE_CXX11_ABI
  maybe_set_tz();

  using namespace std::chrono;
  auto time = sys_days(2025y/March/19) + 15h;
  zoned_time z("America/New_York", time);
  auto s = std::format(std::locale("aa_DJ.UTF-8"), "{:L%c}", z);
  VERIFY( s.find("11:00") != s.npos );

  // We should not print an incorrect time zone.
  VERIFY( s.find("GMT") == s.npos );
  VERIFY( s.find("UTC") == s.npos );

#ifdef _GLIBCXX_HAVE_STRUCT_TM_TM_ZONE
  // When tm.tm_zone is supported, we do print the correct time zone.
  VERIFY( s.find(z.get_info().abbrev) != s.npos );
#endif
#endif
}

void
test_c_local()
{
  maybe_set_tz();

  std::locale("aa_DJ.UTF-8");
  using namespace std::chrono;
  auto time = local_days(2025y/March/19) + 11h;
  auto s = std::format(std::locale("aa_DJ.UTF-8"), "{:L%c}", time);
  // __builtin_printf("%s\n", s.c_str()); return;
  VERIFY( s.find("11:00") != s.npos );

  // We should not print any time zone.
  VERIFY( s.find("GMT") == s.npos );
  VERIFY( s.find("UTC") == s.npos );

#if _GLIBCXX_USE_CXX11_ABI
  zoned_time z(system_clock::now());
  VERIFY( s.find(z.get_info().abbrev) == s.npos );
#endif
}

int main()
{
  test_c();
  test_c_zoned();
  test_c_local();
}
