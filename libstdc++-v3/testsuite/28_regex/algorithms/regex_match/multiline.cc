// { dg-do run { target c++11 } }
#include <regex>
#include <testsuite_hooks.h>

#if __cplusplus >= 201703L || !defined __STRICT_ANSI__
static_assert( std::regex_constants::multiline == std::regex::multiline );
static_assert( std::regex_constants::__multiline == std::regex::multiline );
#else
namespace test { constexpr int multiline = 0; }
namespace check {
  using namespace test;
  using namespace std::regex_constants;
  int ml = multiline;
}
#endif

void
test01()
{
  using namespace std::regex_constants;

  std::regex ml{"^a.$", __multiline};
  VERIFY( ml.flags() == __multiline );
  VERIFY(!std::regex_search("abx\nxab", ml));
  VERIFY(std::regex_search("x\nab", ml));
  VERIFY(std::regex_search("ab\n", ml));
  VERIFY(std::regex_search("x\nab\nx", ml));

  ml.assign("a$\n^b$\n^c", ECMAScript|__multiline);
  VERIFY( ml.flags() == ECMAScript|__multiline );
  VERIFY( regex_search("a\nb\nc", ml) );

  ml.assign("a$\n^b$\n^c", ECMAScript|__multiline|icase);
  VERIFY( ml.flags() == ECMAScript|__multiline|icase );
  VERIFY( regex_search("A\nB\nC", ml) );
}

void
test_pr102480()
{
  using namespace std::regex_constants;

  std::regex re("^a");
  std::regex reml("^a", __multiline);
  VERIFY( std::regex_match("\na" + 1, re));
  VERIFY( std::regex_match("\na" + 1, reml));
  // PR libstdc++/102480
  VERIFY(!std::regex_match("\na" + 1, re, match_prev_avail));
  VERIFY( std::regex_match("\na" + 1, reml, match_prev_avail));
  VERIFY(!std::regex_match("\na" + 1, re, match_not_bol));
  VERIFY(!std::regex_match("\na" + 1, re, match_prev_avail|match_not_bol));
  VERIFY( std::regex_match("\na" + 1, reml, match_prev_avail|match_not_bol));
  VERIFY(!std::regex_match("\ra" + 1, re, match_prev_avail));
  VERIFY( std::regex_match("\ra" + 1, reml, match_prev_avail));
  VERIFY(!std::regex_match("xa" + 1, re, match_prev_avail));
  VERIFY(!std::regex_match("xa" + 1, reml, match_prev_avail));

  std::regex bre("^a", basic|__multiline);
  VERIFY(std::regex_match("\na" + 1, bre));
  VERIFY(!std::regex_match("\na" + 1, bre, match_not_bol));
  // multiline is ignored for any grammar except ECMAScript,
  // so none of the following should match even though
  // match_prev_avail is set and *--first == '\n'.
  VERIFY(!std::regex_match("\na" + 1, bre, match_prev_avail));
  VERIFY(!std::regex_match("\na" + 1, bre, match_prev_avail|match_not_bol));
  VERIFY(!std::regex_match("\ra" + 1, bre, match_prev_avail));
  VERIFY(!std::regex_match("xa" + 1, bre, match_prev_avail));
}

int main()
{
  test01();
  test_pr102480();
}
