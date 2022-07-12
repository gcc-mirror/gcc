// { dg-do run }

#include <sstream>
#include <testsuite_hooks.h>

void
test_pr106248()
{
  wchar_t buf[5] = {L'x', L'x', L'x', L'x', L'x'};
  std::wstring s(L"  four");
  std::wistringstream in(s);
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
  VERIFY( buf[4] == L'\0' );
  VERIFY( std::wstring(buf) == L"four" );

  in.clear();
  in.str(s);
  for (int i = 0; i < 5; ++i)
    s[i] = L'x';

  in.width(5);
  in >> buf;
  // Extraction stops due to field width, eofbit not set.
  VERIFY( in.good() );
  VERIFY( std::wstring(buf) == L"four" );
}

int main()
{
  test_pr106248();
}
