// { dg-do compile { target c++26 } }

// Bug 121024
// ranges::destroy and ranges::destroy_n do not end lifetime of trivial types

#include <memory>

consteval bool is_within_lifetime(const auto* p) noexcept
{
  return __builtin_constant_p(*p);
}

template<typename T>
struct Buf
{
  constexpr Buf() : p(std::allocator<T>().allocate(2)) { }
  constexpr ~Buf() { std::allocator<T>().deallocate(p, 2); }
  T* p;
};

template<typename T>
consteval bool
test_destroy()
{
  Buf<T> buf;
  std::uninitialized_value_construct(buf.p, buf.p + 2);
  std::destroy(buf.p, buf.p + 2);
  return not is_within_lifetime(buf.p) && not is_within_lifetime(buf.p + 1);
}

template<typename T>
consteval bool
test_destroy_n()
{
  Buf<T> buf;
  std::uninitialized_value_construct_n(buf.p, 2);
  std::destroy_n(buf.p, 2);
  return not is_within_lifetime(buf.p) && not is_within_lifetime(buf.p + 1);
}

template<typename T>
consteval bool
test_ranges_destroy()
{
  Buf<T> buf;
  std::uninitialized_value_construct(buf.p, buf.p + 2);
  std::ranges::destroy(buf.p, buf.p + 2);
  return not is_within_lifetime(buf.p) && not is_within_lifetime(buf.p + 1);
}

template<typename T>
consteval bool
test_ranges_destroy_n()
{
  Buf<T> buf;
  std::uninitialized_value_construct_n(buf.p, 2);
  std::ranges::destroy_n(buf.p, 2);
  return not is_within_lifetime(buf.p) && not is_within_lifetime(buf.p + 1);
}

struct O
{
  constexpr O() { }
  constexpr ~O() { }
};

// These all fail for GCC because is_within_lifetime still returns true
// after the lifetime has been ended.
// { dg-xfail-if "PR c++/102284" { *-*-* } }
static_assert( test_destroy<int>() );
static_assert( test_destroy<O>() );
static_assert( test_destroy_n<int>() );
static_assert( test_destroy_n<O>() );
static_assert( test_ranges_destroy<int>() );
static_assert( test_ranges_destroy<O>() );
static_assert( test_ranges_destroy_n<int>() );
static_assert( test_ranges_destroy_n<O>() );
