#ifndef _GLIBCXX_TESTSUITE_STRING_H
#define _GLIBCXX_TESTSUITE_STRING_H

#if defined(_GLIBCXX_DEBUG) && defined(_GLIBCXX_TEST_DEBUG_STRING)
# include <debug/string>
namespace __gnu_test
{
  using __gnu_debug::string;
  using __gnu_debug::wstring;
}
#else
# include <string>
namespace __gnu_test
{
  using std::string;
  using std::wstring;
}
#endif

#endif
