// { dg-do run { target c++20 } }

#include <format>
#include <testsuite_hooks.h>

template<typename T>
bool format_float()
{
    auto s = std::format("{:#} != {:<+7.3f}", (T)-0.0, (T)0.5);
    return s == "-0. != +0.500 ";
}

#if __cplusplus > 202002L
template<typename T>
concept formattable = std::formattable<T, char>;
#else
template<typename T>
concept formattable = std::semiregular<std::formatter<T, char>>;
#endif

void
test_float16()
{
#if __FLT16_DIG__
  if constexpr (formattable<_Float16>)
    VERIFY( format_float<_Float16>() );
  else
    std::puts("Cannot format _Float16 on this target");
#endif
}

void
test_float32()
{
#if __FLT32_DIG__
  if constexpr (formattable<_Float32>)
    VERIFY( format_float<_Float32>() );
  else
    std::puts("Cannot format _Float32 on this target");
#endif
}

void
test_float64()
{
#if __FLT64_DIG__
  if constexpr (formattable<_Float64>)
    VERIFY( format_float<_Float64>() );
  else
    std::puts("Cannot format _Float64 on this target");
#endif
}

void
test_float128()
{
#ifdef __SIZEOF_FLOAT128__
  if constexpr (formattable<__float128>)
    VERIFY( format_float<__float128>() );
  else
    std::puts("Cannot format __float128 on this target");
#endif
#if __FLT128_DIG__
  if constexpr (formattable<_Float128>)
    VERIFY( format_float<_Float128>() );
  else
    std::puts("Cannot format _Float128 on this target");
#endif
}

void
test_bfloat16()
{
#if __BFLT16_DIG__
  using bfloat16_t = decltype(0.0bf16);

  if constexpr (formattable<bfloat16_t>)
    VERIFY( format_float<bfloat16_t>() );
  else
    std::puts("Cannot format bfloat16_t on this target");
#endif
}

int main()
{
  test_float16();
  test_float32();
  test_float64();
  test_float128();
  test_bfloat16();
}
