// { dg-do run { target c++20 } }
// { dg-require-namedlocale "aa_DJ.UTF-8" }
// { dg-require-namedlocale "ar_SA.UTF-8" }
// { dg-require-namedlocale "ca_AD.UTF-8" }
// { dg-require-namedlocale "az_IR.UTF-8" }
// { dg-require-namedlocale "my_MM.UTF-8" }

#include <chrono>
#include <locale>
#include <testsuite_hooks.h>

void
test_c()
{
  const char *test_locales[] = {
    "aa_DJ.UTF-8",
    "ar_SA.UTF-8",
    "ca_AD.UTF-8",
    "az_IR.UTF-8",
    "my_MM.UTF-8",
  };
  std::chrono::sys_seconds t{std::chrono::seconds{1}};

  for (auto locale_name : test_locales)
  {
    auto s = std::format(std::locale(locale_name), "{:L%c}", t);
    VERIFY( !s.empty() );
  }
}

int main()
{
  test_c();
}
