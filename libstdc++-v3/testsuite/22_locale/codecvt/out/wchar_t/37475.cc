#include <locale>
#include <assert.h>

void
test_pr37475()
{
  typedef std::codecvt<wchar_t, char, std::mbstate_t> test_type;
  const test_type& cvt = std::use_facet<test_type>(std::locale::classic());
  const wchar_t from = L'a';
  const wchar_t* from_next;
  char to;
  char* to_next;
  std::mbstate_t st = std::mbstate_t();
  std::codecvt_base::result res
    = cvt.out(st, &from, &from+1, from_next, &to, &to, to_next);

  assert( res == std::codecvt_base::partial );
}

int main()
{
  test_pr37475();
}
