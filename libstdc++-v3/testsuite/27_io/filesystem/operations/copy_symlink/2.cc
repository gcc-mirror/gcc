// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

// Test copying into an existing file

void
test01()
{
  std::error_code ec, ec2;
  __gnu_test::scoped_file f, f2;

  auto p = __gnu_test::nonexistent_path();
  create_symlink(f.path, p);

  copy_symlink(p, f2.path, ec);
  VERIFY( ec );
  VERIFY( !is_symlink(f2.path) );

  try
    {
      copy_symlink(p, f2.path);
    }
  catch (const std::filesystem::filesystem_error& ex)
    {
      ec2 = ex.code();
      VERIFY( ex.path1() == p );
      VERIFY( ex.path2() == f2.path );
    }
  VERIFY( ec2 == ec );
  VERIFY( !is_symlink(f2.path) );

  remove(p);
}

int
main()
{
  test01();
}
