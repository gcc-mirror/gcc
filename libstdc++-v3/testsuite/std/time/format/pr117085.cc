// { dg-do run { target c++20 } }
// { dg-require-namedlocale "fr_FR.ISO8859-1" }

#include <chrono>
#include <locale>
#include <testsuite_hooks.h>

void
test_c()
{
  std::locale::global(std::locale(ISO_8859(1,fr_FR)));
  auto s = std::format("{:L%c}", std::chrono::sys_seconds());
  VERIFY( ! s.starts_with("Thu") );
}

int main()
{
  test_c();
}
