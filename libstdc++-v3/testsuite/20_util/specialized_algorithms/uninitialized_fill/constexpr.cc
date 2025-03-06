// { dg-do compile { target c++26 } }

#include <algorithm>
#include <memory>
#include <span>
#include <string>
#include <vector>

template<typename T, typename U = T>
constexpr
bool
test01_impl(const U& value = U())
{
  static_assert(std::constructible_from<T, U>);
  //static_assert(std::equality_comparable_with<T, U>); // unique_ptr fails with nullptr_t

  constexpr std::size_t size = 42;
  std::allocator<T> alloc;
  T* ptr = alloc.allocate(size);

  auto check = [&]() -> bool
  {
    return std::all_of(ptr, ptr + size, [&](auto &&x) { return x == value; });
  };

  std::uninitialized_fill(ptr, ptr + size, value);
  if (!check())
    return false;
  std::destroy(ptr, ptr + size);

  std::uninitialized_fill_n(ptr, size, value);
  if (!check())
    return false;
  std::destroy_n(ptr, size);

  std::span<T> storage(ptr, ptr + size);
  std::ranges::uninitialized_fill(storage, value);
  if (!check())
    return false;
  std::ranges::destroy(storage);

  std::ranges::uninitialized_fill_n(ptr, size, value);
  if (!check())
    return false;
  std::ranges::destroy_n(ptr, size);

  alloc.deallocate(ptr, size);
  return true;
}

constexpr
bool
test01()
{
  return
    test01_impl<char>('\0') &&
    test01_impl<char>('x') &&
    test01_impl<int>(0) &&
    test01_impl<int>(42) &&
    test01_impl<double>(3.14) &&
#if _GLIBCXX_USE_CXX11_ABI
    test01_impl<std::string>() &&
    test01_impl<std::string>(std::string("test")) &&
#endif
    test01_impl<std::vector<int>>() &&
    test01_impl<std::vector<int>>({1, 2, 3, 4}) &&
    test01_impl<std::unique_ptr<int>>(nullptr);
}

static_assert(test01());
