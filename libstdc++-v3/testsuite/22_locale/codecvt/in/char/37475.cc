#include <locale>
#include <testsuite_hooks.h>

void
test_pr37475()
{
  typedef std::codecvt<char, char, std::mbstate_t> test_type;
  const test_type& cvt = std::use_facet<test_type>(std::locale::classic());
  const char from = 'a';
  const char* from_next;
  char to = 0;
  char* to_next;
  std::mbstate_t st = std::mbstate_t();
  std::codecvt_base::result res
    = cvt.in(st, &from, &from+1, from_next, &to, &to, to_next);

  VERIFY( res == std::codecvt_base::noconv );
}

int main()
{
  test_pr37475();
}
