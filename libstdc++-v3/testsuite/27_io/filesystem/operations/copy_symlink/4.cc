// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

// Test copying into an empty path

namespace fs = std::filesystem;

void
test01()
{
  std::error_code ec, ec2;
  __gnu_test::scoped_file f, f2;

  auto p = __gnu_test::nonexistent_path();
  create_symlink(f.path, p);

  fs::path empty;
  copy_symlink(p, empty, ec);
  VERIFY( ec );

  try
    {
      copy_symlink(p, empty);
    }
  catch (const std::filesystem::filesystem_error& ex)
    {
      ec2 = ex.code();
      VERIFY( ex.path1() == p );
      VERIFY( ex.path2() == empty );
    }
  VERIFY( ec2 == ec );

  remove(p);
}

int
main()
{
  test01();
}
