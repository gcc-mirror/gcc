// { dg-do compile { target c++26 } }
#include <mdspan>

#include <cstdint>

template<typename OffsetType, typename ExtentType, typename StrideType>
  constexpr bool
  test_invalid()
  {
    auto s1 = std::strided_slice(OffsetType{}, ExtentType{}, StrideType{}); // { dg-error "required from" }
    return true;
  }

static_assert(test_invalid<double, int, int>()); // { dg-error "required from" }
static_assert(test_invalid<int, double, int>()); // { dg-error "required from" }
static_assert(test_invalid<int, int, double>()); // { dg-error "required from" }
static_assert(test_invalid<double, double, double>()); // { dg-error "required from" }

// { dg-prune-output "static assertion failed" }
