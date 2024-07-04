// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <experimental/simd>

template <typename T>
  void
  maybe_test()
  {
    using V = std::experimental::simd<T, std::experimental::simd_abi::_VecBuiltin<16>>;
    if constexpr (std::is_destructible_v<V>)
      {
	using V2 [[gnu::vector_size(16)]] = T;
	V x = {};
	V2 x2 = static_cast<V2>(x);
	x = static_cast<V>(x2);
	for (unsigned i = 0; i < V::size(); ++i)
	  {
	    if (x2[i] != 0)
	      __builtin_abort();
	  }
#ifdef __SSE__
	if constexpr (std::is_same_v<T, float>)
	  x = static_cast<V>(static_cast<__m128>(x));
	else if constexpr (std::is_same_v<T, double>)
	  x = static_cast<V>(static_cast<__m128d>(x));
	else if constexpr (std::is_integral_v<T>)
	  x = static_cast<V>(static_cast<__m128i>(x));
#elif __ALTIVEC__
	if constexpr (std::is_same_v<T, float>)
	  x = static_cast<V>(static_cast<__vector float>(x));
#ifdef __VSX__
	else if constexpr (std::is_same_v<T, double>)
	  x = static_cast<V>(static_cast<__vector double>(x));
#endif
	else if constexpr (std::is_integral_v<T> && sizeof(T) == sizeof(signed char)
			     && std::is_signed_v<T>)
	  x = static_cast<V>(static_cast<__vector signed char>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == sizeof(signed char))
	  x = static_cast<V>(static_cast<__vector unsigned char>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == sizeof(short)
			     && std::is_signed_v<T>)
	  x = static_cast<V>(static_cast<__vector signed short>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == sizeof(short))
	  x = static_cast<V>(static_cast<__vector unsigned short>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == sizeof(int)
			     && std::is_signed_v<T>)
	  x = static_cast<V>(static_cast<__vector signed int>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == sizeof(int))
	  x = static_cast<V>(static_cast<__vector unsigned int>(x));
#ifdef __VSX__
	else if constexpr (std::is_integral_v<T> && sizeof(T) == sizeof(long long)
			     && std::is_signed_v<T>)
	  x = static_cast<V>(static_cast<__vector signed long long>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == sizeof(long long))
	  x = static_cast<V>(static_cast<__vector unsigned long long>(x));
#endif
#elif __ARM_NEON
	if constexpr (std::is_same_v<T, float>)
	  x = static_cast<V>(static_cast<float32x4_t>(x));
#ifdef __aarch64__
	else if constexpr (std::is_same_v<T, double>)
	  x = static_cast<V>(static_cast<float64x2_t>(x));
#endif
	else if constexpr (std::is_integral_v<T> && sizeof(T) == 1 && std::is_signed_v<T>)
	  x = static_cast<V>(static_cast<int8x16_t>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == 1)
	  x = static_cast<V>(static_cast<uint8x16_t>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == 2 && std::is_signed_v<T>)
	  x = static_cast<V>(static_cast<int16x8_t>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == 2)
	  x = static_cast<V>(static_cast<uint16x8_t>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == 4 && std::is_signed_v<T>)
	  x = static_cast<V>(static_cast<int32x4_t>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == 4)
	  x = static_cast<V>(static_cast<uint32x4_t>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == 8 && std::is_signed_v<T>)
	  x = static_cast<V>(static_cast<int64x2_t>(x));
	else if constexpr (std::is_integral_v<T> && sizeof(T) == 8)
	  x = static_cast<V>(static_cast<uint64x2_t>(x));
#endif
      }
  }

int main()
{
  maybe_test<char>();
  maybe_test<wchar_t>();
  maybe_test<char16_t>();
  maybe_test<char32_t>();

  maybe_test<signed char>();
  maybe_test<unsigned char>();
  maybe_test<short>();
  maybe_test<unsigned short>();
  maybe_test<int>();
  maybe_test<unsigned int>();
  maybe_test<long>();
  maybe_test<unsigned long>();
  maybe_test<long long>();
  maybe_test<unsigned long long>();
  maybe_test<float>();
  maybe_test<double>();
  maybe_test<long double>();
}
