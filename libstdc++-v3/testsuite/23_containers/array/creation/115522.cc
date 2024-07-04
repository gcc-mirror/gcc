// { dg-do compile { target c++20 } }

// PR libstdc++/115522 std::to_array no longer works for struct which is
// trivial but not default constructible

#include <array>

void
test_deleted_ctor()
{
  struct S
  {
    S() = delete;
    S(int) { }
  };

  S arr[1] = {{1}};
  auto arr1 = std::to_array(arr);
  auto arr2 = std::to_array(std::move(arr));
}

void
test_deleted_assignment()
{
  struct S
  {
    void operator=(const S&) = delete;
  };

  S arr[1] = {};
  auto a1 = std::to_array(arr);
  auto a2 = std::to_array(std::move(arr));
}
