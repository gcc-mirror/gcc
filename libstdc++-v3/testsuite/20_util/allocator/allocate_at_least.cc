// { dg-do run { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

struct X { int i = 0; };

template <typename T>
  struct A : std::allocator<T>
  {
    using Base = std::allocator<T>;

    std::allocation_result<T*, size_t>
    allocate_at_least(std::size_t n)
      { return { this->Base::allocate(2*n), 2*n }; }
  };

template <typename T>
  struct M : std::allocator<T>
  {
    using Base = std::allocator<T>;
    T* allocate_at_least(size_t n) = delete;

    T* keep;
    T* allocate(std::size_t n)
      {
	keep = this->Base::allocate(n);
	return keep;
      }
  };

int main()
{
  std::allocator<X> native;
  auto a1 = native.allocate_at_least(100);
  static_assert(std::is_same_v<decltype(a1), std::allocation_result<X*>>);
  VERIFY(a1.count == 100);
  native.deallocate(a1.ptr, a1.count);

  using std_traits = std::allocator_traits<std::allocator<X>>;
  auto a2 = std_traits::allocate_at_least(native, 100);
  static_assert(std::is_same_v<decltype(a2), std::allocation_result<X*>>);
  VERIFY(a2.count == 100);
  std_traits::deallocate(native, a2.ptr, a2.count);

  A<X> custom;
  auto a3 = custom.allocate_at_least(100);
  static_assert(std::is_same_v<decltype(a3), std::allocation_result<X*>>);
  VERIFY(a3.count == 200);
  custom.deallocate(a3.ptr, a3.count);

  using custom_traits = std::allocator_traits<A<X>>;
  auto a4 = custom_traits::allocate_at_least(custom, 100);
  static_assert(std::is_same_v<decltype(a4), std::allocation_result<X*>>);
  VERIFY(a4.count == 200);
  custom_traits::deallocate(custom, a4.ptr, a4.count);

  M<X> minimal;
  using minimal_traits = std::allocator_traits<M<X>>;
  auto a5 = minimal_traits::allocate_at_least(minimal, 100);
  static_assert(std::is_same_v<decltype(a5), std::allocation_result<X*>>);
  VERIFY(a5.count == 100);
  VERIFY(a5.ptr == minimal.keep);
  minimal_traits::deallocate(minimal, a5.ptr, a5.count);
}
