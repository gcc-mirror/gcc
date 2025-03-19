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
#include <locale>
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

void
test_locale_imbued()
{
  // Custom time_put facet which returns %b string for %Om.
  // The %b string will come from io.getloc() which should be
  // the formatting locale using by std::format.
  struct TimePut : std::time_put<char>
  {
    iter_type
    do_put(iter_type out, std::ios_base& io, char_type fill, const tm* t,
	   char format, char modifier) const override
    {
      if (format == 'm' && modifier == 'O')
	format = 'b';
      return std::time_put<char>::do_put(out, io, fill, t, format, 0);
    }
  };

  auto m = std::chrono::March;

  std::locale fr(ISO_8859(1,fr_FR));
  std::locale fr2(fr, new TimePut);
  auto s1 = std::format(fr2, "{:L%Om}", m); // should be %b in fr_FR locale
  VERIFY( s1 == std::format(fr, "{:L}", m) );

  std::locale es(ISO_8859(1,es_ES));
  std::locale es2(es, new TimePut);
  auto s2 = std::format(es2, "{:L%Om}", m); // should be %b in es_ES locale
  VERIFY( s2 == std::format(es, "{:L}", m) );
}

int main()
{
  test_ru();
  test_es();
  test_fr();
  test_en();
  test_locale_imbued();
}
