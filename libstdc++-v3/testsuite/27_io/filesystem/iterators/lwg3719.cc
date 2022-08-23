// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-require-filesystem-ts "" }

#include <filesystem>
#include <iterator>
#include <testsuite_hooks.h>

// LWG 3719. Directory iterators should be usable with default sentinel

void
test_dir_iter()
{
  std::filesystem::directory_iterator d0;
  VERIFY( d0 == std::default_sentinel );
  std::filesystem::directory_iterator d1(".");
  VERIFY( d1 != std::default_sentinel );

  static_assert( noexcept(d0 == std::default_sentinel) );
  static_assert( noexcept(d0 != std::default_sentinel) );
}

void
test_recursive_dir_iter()
{
  std::filesystem::recursive_directory_iterator d0;
  VERIFY( d0 == std::default_sentinel );
  std::filesystem::recursive_directory_iterator d1(".");
  VERIFY( d1 != std::default_sentinel );

  static_assert( noexcept(d0 == std::default_sentinel) );
  static_assert( noexcept(d0 != std::default_sentinel) );
}

int main()
{
  test_dir_iter();
  test_recursive_dir_iter();
}
