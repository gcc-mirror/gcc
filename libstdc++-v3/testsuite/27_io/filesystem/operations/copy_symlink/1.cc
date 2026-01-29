// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

// Test successful copies

namespace fs = std::filesystem;

void
test_successful_copy(const fs::path& p,
		     bool (*is_some_file_type)(const fs::path&))
{
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;

  auto to1 = read_symlink(p);

  auto p2 = __gnu_test::nonexistent_path();
  ec = bad_ec;
  copy_symlink(p, p2, ec);
  VERIFY( !ec );
  VERIFY( exists(symlink_status(p2)) );
  VERIFY( is_symlink(p2) );
  VERIFY( is_some_file_type(p2) );
  auto to2 = read_symlink(p2);
  VERIFY( to1 == to2 );

  // Copy again without ec
  remove(p2);
  copy_symlink(p, p2);
  VERIFY( exists(symlink_status(p2)) );
  VERIFY( is_symlink(p2) );
  VERIFY( is_some_file_type(p2) );
  to2 = read_symlink(p2);
  VERIFY( to1 == to2 );

  remove(p);
  remove(p2);
}

void
test01()
{
  __gnu_test::scoped_file f;
  auto p = __gnu_test::nonexistent_path();
  create_symlink(f.path, p);
  test_successful_copy(p, fs::is_regular_file);

  auto dir = __gnu_test::nonexistent_path();
  create_directory(dir);
  create_directory_symlink(dir, p);
  test_successful_copy(p, fs::is_directory);
  remove_all(dir);
}

int
main()
{
  test01();
}
