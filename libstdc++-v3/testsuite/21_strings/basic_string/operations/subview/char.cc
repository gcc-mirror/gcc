// { dg-do run { target c++26 } }

#include <stdexcept>
#include <string>
#include <string_view>
#include <testsuite_hooks.h>

void test01(void) {
  typedef std::string::size_type csize_type;
  typedef std::string::const_reference cref;
  typedef std::string::reference ref;
  csize_type csz01;

  const char str_lit01[] = "rockaway, pacifica";
  const std::string str01(str_lit01);
  std::string_view str02;

  csz01 = str01.size();
  str02 = str01.subview(0, 1);
  VERIFY(str02 == "r");
  str02 = str01.subview(10);
  VERIFY(str02 == "pacifica");

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
