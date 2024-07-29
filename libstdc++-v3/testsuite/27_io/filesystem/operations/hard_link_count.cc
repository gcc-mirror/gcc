// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

namespace fs = std::filesystem;

void test01()
{
  // PR libstdc++/113663

  fs::path p1 = __gnu_test::nonexistent_path();
  VERIFY( !fs::exists(p1) );

  __gnu_test::scoped_file f1(p1);
  VERIFY( fs::exists(p1) );

  VERIFY( fs::hard_link_count(p1) == 1 );

  fs::path p2 = __gnu_test::nonexistent_path();
  VERIFY( !fs::exists(p2) );

  fs::create_hard_link(p1, p2);
  __gnu_test::scoped_file f2(p2, __gnu_test::scoped_file::adopt_file);
  VERIFY( fs::exists(p2) );

  VERIFY( fs::hard_link_count(p1) == 2 );
  VERIFY( fs::hard_link_count(p2) == 2 );
}

int
main()
{
  test01();
}
