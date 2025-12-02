// { dg-do run { target c++26 } }

#include <string_view>
#include <testsuite_hooks.h>

#if __STDC_HOSTED__
#include <stdexcept>
#endif

void test01() {
  typedef std::wstring_view::size_type csize_type;
  typedef std::wstring_view::const_reference cref;
  typedef std::wstring_view::reference ref;
  csize_type csz01;

  const wchar_t str_lit01[] = L"rockaway, pacifica";
  const std::wstring_view str01(str_lit01);
  std::wstring_view str02;

  csz01 = str01.size();
  str02 = str01.subview(0, 1);
  VERIFY(str02 == L"r");
  str02 = str01.subview(10);
  VERIFY(str02 == L"pacifica");

#if __STDC_HOSTED__
  try {
    str02 = str01.subview(csz01 + 1);
    VERIFY(false);
  } catch (std::out_of_range &fail) {
    VERIFY(true);
  } catch (...) {
    VERIFY(false);
  }

  try {
    str02 = str01.subview(csz01);
    VERIFY(str02.size() == 0);
    VERIFY(str02.begin() == str01.end());
    VERIFY(true);
  } catch (...) {
    VERIFY(false);
  }
#endif // HOSTED
}

int main() {
  test01();

  return 0;
}
