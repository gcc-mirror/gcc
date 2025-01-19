// { dg-do run }

// Bug libstdc++/105857 codecvt::do_length causes unexpected buffer overflow

#include <locale>
#include <testsuite_hooks.h>

int main()
{
  std::string s(1050, 'a');
  typedef std::codecvt<wchar_t, char, std::mbstate_t> Cvt;
  const Cvt& cvt = std::use_facet<Cvt>(std::locale());
  const char* from = s.c_str();
  const char* from_end = s.c_str() + s.size();
  std::size_t max = std::size_t(-1) / 4 + 2;
  std::mbstate_t state = std::mbstate_t();
  int n = cvt.length(state, from, from_end, max);
  VERIFY( n == s.size() );
  n = cvt.length(state, from, from_end, 2);
  VERIFY( n == 2 );
}
