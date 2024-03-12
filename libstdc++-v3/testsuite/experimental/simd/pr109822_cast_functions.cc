// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <experimental/simd>

namespace stdx = std::experimental;

template <typename T, typename V>
  void
  test01()
  {
    using M = typename V::mask_type;
    [[maybe_unused]] auto x = to_fixed_size(V());
    [[maybe_unused]] auto k = to_fixed_size(M());
    if constexpr (stdx::simd<T>::size() == V::size())
      {
	[[maybe_unused]] auto xx = to_compatible(x);
	[[maybe_unused]] auto kk = to_compatible(k);
	x = to_fixed_size(xx);
	k = to_fixed_size(kk);
      }
    if constexpr (stdx::native_simd<T>::size() == V::size())
      {
	[[maybe_unused]] auto xx = to_native(x);
	[[maybe_unused]] auto kk = to_native(k);
	x = to_fixed_size(xx);
	k = to_fixed_size(kk);
      }
  }

template <typename T>
  void
  iterate_abis()
  {
    test01<T, stdx::simd<T, stdx::simd_abi::scalar>>();
    test01<T, stdx::simd<T>>();
    test01<T, stdx::native_simd<T>>();
    test01<T, stdx::fixed_size_simd<T, 3>>();
    test01<T, stdx::fixed_size_simd<T, stdx::simd_abi::max_fixed_size<T> - 4>>();
  }

int
main()
{
  iterate_abis<char>();
  iterate_abis<wchar_t>();
  iterate_abis<char16_t>();
  iterate_abis<char32_t>();

  iterate_abis<signed char>();
  iterate_abis<unsigned char>();
  iterate_abis<short>();
  iterate_abis<unsigned short>();
  iterate_abis<int>();
  iterate_abis<unsigned int>();
  iterate_abis<long>();
  iterate_abis<unsigned long>();
  iterate_abis<long long>();
  iterate_abis<unsigned long long>();
  iterate_abis<float>();
  iterate_abis<double>();
  iterate_abis<long double>();
}
