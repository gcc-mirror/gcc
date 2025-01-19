// { dg-do run { target c++17 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <testsuite_fs.h>
#include <testsuite_hooks.h>

#if defined(_GLIBCXX_HAVE_SYS_STAT_H) && defined(_GLIBCXX_HAVE_SYS_TYPES_H)
# include <sys/types.h>
# include <sys/stat.h>  // mkfifo
#endif

namespace fs = std::filesystem;

void
test_pr118158()
{
#if defined(_GLIBCXX_HAVE_SYS_STAT_H) && defined(_GLIBCXX_HAVE_SYS_TYPES_H) \
  && defined(S_IWUSR) && defined(S_IRUSR)
  auto p1 = __gnu_test::nonexistent_path();
  auto p2 = __gnu_test::nonexistent_path();
  auto p3 = __gnu_test::nonexistent_path();
  const std::error_code bad_ec = make_error_code(std::errc::invalid_argument);
  std::error_code ec;
  bool result;

  VERIFY( ! ::mkfifo(p1.c_str(), S_IWUSR | S_IRUSR) );
  __gnu_test::scoped_file f1(p1, __gnu_test::scoped_file::adopt_file);

  // Special file is equivalent to itself.
  VERIFY( equivalent(p1, p1) );
  VERIFY( equivalent(p1, p1, ec) );
  VERIFY( ! ec );

  VERIFY( ! ::mkfifo(p2.c_str(), S_IWUSR | S_IRUSR) );
  __gnu_test::scoped_file f2(p2, __gnu_test::scoped_file::adopt_file);

  ec = bad_ec;
  // Distinct special files are not equivalent.
  VERIFY( ! equivalent(p1, p2, ec) );
  VERIFY( ! ec );

  // Non-existent paths are always an error.
  VERIFY( ! equivalent(p1, p3, ec) );
  VERIFY( ec == std::errc::no_such_file_or_directory );
  ec = bad_ec;
  VERIFY( ! equivalent(p3, p2, ec) );
  VERIFY( ec == std::errc::no_such_file_or_directory );

  // Special file is not equivalent to regular file.
  __gnu_test::scoped_file f3(p3);
  ec = bad_ec;
  VERIFY( ! equivalent(p1, p3, ec) );
  VERIFY( ! ec );
#endif
}

int
main()
{
  test_pr118158();
}
