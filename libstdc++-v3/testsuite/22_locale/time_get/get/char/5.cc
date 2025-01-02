// { dg-do run { target c++11} }

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

int main()
{
  using Facet = std::time_get<char>;
  const Facet& fac = std::use_facet<Facet>(std::locale::classic());
  std::istringstream ss("Fri Jul 5 14:58:21 2019");
  std::ios::iostate err = std::ios::goodbit;
  std::tm tm = {};
  fac.get(ss, Facet::iter_type(), ss, err, &tm, 'c');
  VERIFY( err == std::ios::eofbit );
  VERIFY( tm.tm_year == 119 );
  VERIFY( tm.tm_mon == 6 );
  VERIFY( tm.tm_mday == 5 );
  VERIFY( tm.tm_wday == 5 );
  VERIFY( tm.tm_hour == 14 );
  VERIFY( tm.tm_min == 58 );
  VERIFY( tm.tm_sec == 21 );
  ss.clear();
  ss.seekg(0);
  ss.str(ss.str() + " non-whitespace after the datetime");
  err = std::ios::goodbit;
  tm = std::tm();
  fac.get(ss, Facet::iter_type(), ss, err, &tm, 'c', 'E');
  VERIFY( err == std::ios::goodbit );
  VERIFY( tm.tm_year == 119 );
  VERIFY( tm.tm_mon == 6 );
  VERIFY( tm.tm_mday == 5 );
  VERIFY( tm.tm_wday == 5 );
  VERIFY( tm.tm_hour == 14 );
  VERIFY( tm.tm_min == 58 );
  VERIFY( tm.tm_sec == 21 );
}
