// { dg-do run { target c++26 } }

#include <istream>
#include <sstream>
#include <string>
#include <testsuite_hooks.h>

void test01() {
  std::istringstream in("\xF0\x9F\xA4\xA1 Clown Face");
  in.ignore(100, '\xA1');
  VERIFY(in.gcount() == 4);
  VERIFY(in.peek() == ' ');

  std::string str;
  in >> str;
  VERIFY(str == "Clown");
}

int main() {
  test01();
  return 0;
}
