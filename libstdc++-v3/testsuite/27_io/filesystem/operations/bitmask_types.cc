// { dg-do compile { target c++17 } }
#include <filesystem>

namespace fs = std::filesystem;

void
test_copy_options()
{
  using T = fs::copy_options;
  constexpr T val = T::none;
  constexpr T all = ((val | val) & val) ^ ~val;
  static_assert( noexcept(((val | val) & val) ^ ~val) );
  val & val; // { dg-warning "ignoring return value" }
  val | val; // { dg-warning "ignoring return value" }
  val ^ val; // { dg-warning "ignoring return value" }
  ~val;      // { dg-warning "ignoring return value" }
}

void
test_perms()
{
  using T = fs::perms;
  constexpr T val = T::none;
  constexpr T all = ((val | val) & val) ^ ~val;
  static_assert( noexcept(((val | val) & val) ^ ~val) );
  val & val; // { dg-warning "ignoring return value" }
  val | val; // { dg-warning "ignoring return value" }
  val ^ val; // { dg-warning "ignoring return value" }
  ~val;      // { dg-warning "ignoring return value" }
}

void
test_perm_options()
{
  using T = fs::perm_options;
  constexpr T val = T::replace;
  constexpr T all = ((val | val) & val) ^ ~val;
  static_assert( noexcept(((val | val) & val) ^ ~val) );
  val & val; // { dg-warning "ignoring return value" }
  val | val; // { dg-warning "ignoring return value" }
  val ^ val; // { dg-warning "ignoring return value" }
  ~val;      // { dg-warning "ignoring return value" }
}

void
test_directory_options()
{
  using T = fs::directory_options;
  constexpr T val = T::none;
  constexpr T all = ((val | val) & val) ^ ~val;
  static_assert( noexcept(((val | val) & val) ^ ~val) );
  val & val; // { dg-warning "ignoring return value" }
  val | val; // { dg-warning "ignoring return value" }
  val ^ val; // { dg-warning "ignoring return value" }
  ~val;      // { dg-warning "ignoring return value" }
}
