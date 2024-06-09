// { dg-options "-std=c++17" }
// { dg-do compile { target x86_64-*-* } }
// { dg-require-effective-target c++17 }
// { dg-additional-options "-march=x86-64-v3" { target x86_64-*-* } }
// { dg-require-cmath "" }
// { dg-final { scan-assembler-times "vperm(q|pd)\[\\t \]+\\\$144" 1 } }

#include <experimental/simd>

namespace stdx = std::experimental;

using T = std::uint64_t;
using V = stdx::simd<T, stdx::simd_abi::_VecBuiltin<32>>;
using V1 = stdx::simd<T, stdx::simd_abi::scalar>;

V perm(V data)
{
  auto [carry, _] = stdx::split<3, 1>(data);
  return concat(V1(), carry);
}
