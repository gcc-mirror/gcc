// { dg-do run }

#include <locale>
#include <testsuite_hooks.h>

void
test_pr108323()
{
  std::locale named = std::locale::classic();
  std::locale unnamed = named.combine<std::ctype<char> >(named);

  // Bug libstdc++/108323 - combine does not change the locale name
  VERIFY( unnamed.name() == "*" );
}

void
test_lwg2295()
{
  std::locale named = std::locale::classic();
  std::locale unnamed(named, &std::use_facet<std::ctype<char> >(named));
  VERIFY( unnamed.name() == "*" );

  // LWG 2295. Locale name when the provided Facet is a nullptr
  std::locale loc(named, (std::ctype<char>*)0);
  VERIFY( loc.name() != "*" );
  VERIFY( loc.name() == named.name() );
}

void
test_lwg3676()
{
  std::locale named = std::locale::classic();
  std::locale unnamed = named.combine<std::ctype<char> >(named);
  std::locale combo;

  // LWG 3676. Name of locale composed using std::locale::none

  combo = std::locale(named, named, std::locale::numeric);
  VERIFY( combo.name() != "*" );
  combo = std::locale(named, named, std::locale::none);
  VERIFY( combo.name() != "*" );
  combo = std::locale(named, unnamed, std::locale::numeric);
  VERIFY( combo.name() == "*" );
  combo = std::locale(named, unnamed, std::locale::none);
  VERIFY( combo.name() != "*" );
  combo = std::locale(unnamed, named, std::locale::numeric);
  VERIFY( combo.name() == "*" );
  combo = std::locale(unnamed, named, std::locale::none);
  VERIFY( combo.name() == "*" );
  combo = std::locale(unnamed, unnamed, std::locale::numeric);
  VERIFY( combo.name() == "*" );
  combo = std::locale(unnamed, unnamed, std::locale::none);
  VERIFY( combo.name() == "*" );
}

int main()
{
  test_pr108323();
  test_lwg2295();
  test_lwg3676();
}
