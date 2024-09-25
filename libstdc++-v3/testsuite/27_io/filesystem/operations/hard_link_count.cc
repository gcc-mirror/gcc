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

void
test02()
{
  std::error_code ec;
  fs::path p1 = __gnu_test::nonexistent_path();
  VERIFY( fs::hard_link_count(p1, ec) == -1 );
  VERIFY( ec == std::errc::no_such_file_or_directory );

#if __cpp_exceptions
  try {
    fs::hard_link_count(p1); // { dg-warning "ignoring return value" }
    VERIFY( false );
  } catch (const fs::filesystem_error& e) {
    VERIFY( e.path1() == p1 );
    VERIFY( e.path2().empty() );
  }
#endif

  __gnu_test::scoped_file f1(p1);
  fs::hard_link_count(f1.path, ec);  // { dg-warning "ignoring return value" }
  VERIFY( !ec ); // Should be cleared on success.
}

int
main()
{
  test01();
  test02();
}
