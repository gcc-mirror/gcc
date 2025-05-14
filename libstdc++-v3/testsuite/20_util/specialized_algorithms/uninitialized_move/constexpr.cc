// { dg-do compile { target c++26 } }

#include <algorithm>
#include <memory>
#include <span>
#include <string>
#include <vector>

template<typename T>
constexpr
bool
test01_impl(std::vector<T> input)
{
  static_assert(std::move_constructible<T>);
  static_assert(std::equality_comparable<T>);

  const std::size_t input_size = input.size();
  std::allocator<T> alloc;
  T* ptr = alloc.allocate(input_size);

  std::uninitialized_move(input.begin(), input.end(), ptr);
  std::destroy(ptr, ptr + input_size);

  std::uninitialized_move_n(input.begin(), input_size, ptr);
  std::destroy_n(ptr, input_size);

  std::span<T> output(ptr, ptr + input_size);
  std::ranges::uninitialized_move(input, output);
  std::ranges::destroy(output);

  std::ranges::uninitialized_move_n(input.begin(), input_size, ptr, ptr + input_size);
  std::ranges::destroy_n(ptr, input_size);

  alloc.deallocate(ptr, input_size);
  return true;
}

constexpr
bool
test01()
{
  return
    test01_impl<char>({'a', 'b', 'c'}) &&
    test01_impl<int>({1, 2, 3, 4}) &&
    test01_impl<double>({1.0, 2.0, 3.0, 4.0}) &&
#if _GLIBCXX_USE_CXX11_ABI
    test01_impl<std::string>({"a", "b", "cc", "dddd", "eeeeeeeeeeeeeeee"}) &&
#endif
    test01_impl<std::vector<int>>({ {0}, {0, 1}, {0, 1, 2}}) &&
    test01_impl<std::unique_ptr<int>>(std::vector<std::unique_ptr<int>>(10));
}

static_assert(test01());
