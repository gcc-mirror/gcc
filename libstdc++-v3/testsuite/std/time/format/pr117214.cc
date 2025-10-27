// { dg-do run { target c++20 } }
// { dg-require-namedlocale "aa_DJ.UTF-8" }
// { dg-require-namedlocale "ar_SA.UTF-8" }
// { dg-require-namedlocale "ca_AD.UTF-8" }
// { dg-require-namedlocale "az_IR.UTF-8" }
// { dg-require-namedlocale "my_MM.UTF-8" }

#include <chrono>
#include <locale>
#include <span>
#include <testsuite_hooks.h>


template<typename ChronoType>
void
test_locale_formats(const ChronoType& t,
		    std::span<const char* const> test_specifiers)
{
  const char *test_locales[] = {
    "aa_DJ.UTF-8",
    "ar_SA.UTF-8",
    "ca_AD.UTF-8",
    "az_IR.UTF-8",
    "my_MM.UTF-8",
  };

  auto format_args = std::make_format_args(t);
  for (auto locale_name : test_locales)
  {
    std::locale loc(locale_name);
    for (auto specifier : test_specifiers)
    {
      auto s = std::vformat(loc, specifier, format_args);
      VERIFY( !s.empty() );
    }
  }
}

void
test_locale_formats()
{
  using namespace std::chrono;

  const char* test_specifiers[] = {
    "{:L%x}", "{:L%Ex}",
    "{:L%c}", "{:L%Ec}",
    "{:L%X}", "{:L%EX}",
    "{:L%r}",
  };
  auto date_time_specifiers = std::span(test_specifiers);
  auto date_specifiers = date_time_specifiers.subspan(0, 2);
  auto time_specifiers = date_time_specifiers.subspan(4);

  auto ymd = 2020y/November/12d;
  test_locale_formats(ymd, date_specifiers);

  auto tod = 25h + 10min + 12s;
  test_locale_formats(tod, time_specifiers);

  auto tp = sys_days(ymd) + tod;
  test_locale_formats(tp, date_time_specifiers);
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
  test_locale_formats();
  test_c_zoned();
  test_c_local();
}
