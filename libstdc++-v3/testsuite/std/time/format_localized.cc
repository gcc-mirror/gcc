// { dg-do run { target c++20 } }
// { dg-require-namedlocale "es_ES.ISO8859-1" }
// { dg-require-namedlocale "fr_FR.ISO8859-1" }
// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-namedlocale "en_US.ISO8859-15" }
// { dg-require-namedlocale "en_US.UTF-8" }

// P2419R2
// Clarify handling of encodings in localized formatting of chrono types

// Localized date-time strings such as "février" should be converted to UTF-8
// if the locale uses a different encoding.

#include <chrono>
#include <format>
#include <stdio.h>
#include <testsuite_hooks.h>

void
test_ru()
{
  bool ok = false;
#if __cpp_exceptions
  std::locale loc;
  try
  {
    loc = std::locale("ru_UA.KOI8-U");
    ok = true;
  }
  catch (const std::runtime_error&)
  {
    try
    {
      loc = std::locale("ru_RU.KOI8-R");
      ok = true;
    }
    catch (const std::runtime_error&)
    {
    }
  }
#endif
  if (ok)
  {
    auto s = std::format(loc, "День недели: {:L}", std::chrono::Monday);
    VERIFY( s == "День недели: Пн" || s == "День недели: пн" );
  }
  else
    puts("NOTE: test_ru(): skipped unsupported locales");
}

void
test_es()
{
  std::locale loc(ISO_8859(1,es_ES));
  auto s = std::format(loc, "Día de la semana: {:L%A %a}",
		       std::chrono::Wednesday);
  if (s.back() == '.') // FreeBSD has this in the %a string
    s.pop_back();
  VERIFY( s == "Día de la semana: miércoles mié" );
}

void
test_fr()
{
  std::locale loc(ISO_8859(1,fr_FR));
  auto s = std::format(loc, "Six mois après {0:L%b}, c'est {1:L%B}.",
		       std::chrono::February, std::chrono::August);
  VERIFY( s == "Six mois après févr., c'est août." );
}

void
test_en()
{
  using namespace std::chrono;

  for (auto l : {ISO_8859(1,en_US), ISO_8859(15,en_US), "en_US.UTF-8", "C"})
    {
      std::locale loc(l);
      auto s = std::format(loc, "{:L%b %B %a %A}", sys_days(2024y/July/30));
      VERIFY( s == "Jul July Tue Tuesday" );
    }
}

int main()
{
  test_ru();
  test_es();
  test_fr();
  test_en();
}
