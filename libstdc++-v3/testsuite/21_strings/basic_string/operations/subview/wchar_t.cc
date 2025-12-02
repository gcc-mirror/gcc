// { dg-do run { target c++26 } }

#include <stdexcept>
#include <string>
#include <string_view>
#include <testsuite_hooks.h>

void test01(void) {
  typedef std::wstring::size_type csize_type;
  typedef std::wstring::const_reference cref;
  typedef std::wstring::reference ref;
  csize_type csz01;

  const wchar_t str_lit01[] = L"rockaway, pacifica";
  const std::wstring str01(str_lit01);
  std::wstring_view str02;

  csz01 = str01.size();
  str02 = str01.subview(0, 1);
  VERIFY(str02 == L"r");
  str02 = str01.subview(10);
  VERIFY(str02 == L"pacifica");

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
  } catch (std::out_of_range &fail) {
    VERIFY(false);
  } catch (...) {
    VERIFY(false);
  }
}

int main() {
  test01();
  return 0;
}
