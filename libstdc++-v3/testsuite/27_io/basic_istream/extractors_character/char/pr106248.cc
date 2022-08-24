// { dg-do run }

#include <sstream>
#include <testsuite_hooks.h>

void
test_pr106248()
{
  char buf[5] = {'x', 'x', 'x', 'x', 'x'};
  std::string s("  four");
  std::istringstream in(s);
  in >> buf;
#if __cplusplus >= 202002L
  // Extraction stops because buffer is full.
  VERIFY( in.good() );
#else
  // PR libstdc++/106248
  // Extraction stops because all input has been consumed and eofbit is set.
  VERIFY( in.eof() );
#endif
  // Extracted string must be null-terminated.
  VERIFY( buf[4] == '\0' );
  VERIFY( std::string(buf) == "four" );

  in.clear();
  in.str(s);
  for (int i = 0; i < 5; ++i)
    s[i] = 'x';

  in.width(5);
  in >> buf;
  // Extraction stops due to field width, eofbit not set.
  VERIFY( in.good() );
  VERIFY( std::string(buf) == "four" );
}

int main()
{
  test_pr106248();
}
