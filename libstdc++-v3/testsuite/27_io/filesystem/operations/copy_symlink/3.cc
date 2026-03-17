// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

// Test copying from a non-symlink

namespace fs = std::filesystem;

void
test_copy_from_non_symlink(const fs::path& from)
{
  std::error_code ec, ec2;
  auto p = __gnu_test::nonexistent_path();
  copy_symlink(from, p, ec);
  VERIFY( ec );
  VERIFY( !is_symlink(p) );

  try
    {
      copy_symlink(from, p);
    }
  catch (const fs::filesystem_error& ex)
    {
      ec2 = ex.code();
      VERIFY( ex.path1() == from );
      VERIFY( ex.path2() == p );
    }
  VERIFY( ec2 == ec );
  VERIFY( !is_symlink(p) );
}

void
test01()
{
  __gnu_test::scoped_file f;
  test_copy_from_non_symlink(f.path);
  test_copy_from_non_symlink(__gnu_test::nonexistent_path());
}

int
main()
{
  test01();
}
