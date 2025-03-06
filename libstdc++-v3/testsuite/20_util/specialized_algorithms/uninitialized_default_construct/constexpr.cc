// { dg-do compile { target c++26 } }

#include <algorithm>
#include <memory>
#include <span>
#include <string>
#include <vector>

template<typename T>
constexpr
bool
test01_impl()
{
  static_assert(std::default_initializable<T>);
  static_assert(std::equality_comparable<T>);

  constexpr std::size_t size = 42;
  std::allocator<T> alloc;
  T* ptr = alloc.allocate(size);

  auto check = [&]() -> bool
  {
    if constexpr (!std::is_trivially_default_constructible_v<T>)
      return std::all_of(ptr, ptr + size, [](auto &&x) { return x == T(); });
    else
      return true;
  };

  std::uninitialized_default_construct(ptr, ptr + size);
  if (!check())
    return false;
  std::destroy(ptr, ptr + size);

  std::uninitialized_default_construct_n(ptr, size);
  if (!check())
    return false;
  std::destroy_n(ptr, size);

  std::span<T> storage(ptr, ptr + size);
  std::ranges::uninitialized_default_construct(storage);
  if (!check())
    return false;
  std::ranges::destroy(storage);

  std::ranges::uninitialized_default_construct_n(ptr, size);
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
    test01_impl<char>() &&
    test01_impl<int>() &&
    test01_impl<double>() &&
#if _GLIBCXX_USE_CXX11_ABI
    test01_impl<std::string>() &&
#endif
    test01_impl<std::vector<int>>() &&
    test01_impl<std::unique_ptr<int>>();
}

static_assert(test01());
