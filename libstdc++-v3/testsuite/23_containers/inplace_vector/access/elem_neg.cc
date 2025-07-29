// { dg-do compile { target c++26 } }

#include <inplace_vector>

template<bool Const, typename T, size_t N>
using InplaceVector 
  = std::conditional_t<Const, 
		       const std::inplace_vector<T, N>,
		       std::inplace_vector<T, N>>;

template<bool Const, size_t N, typename T>
constexpr bool
test_front_on_empty()
{
  InplaceVector<Const, T, N> v;
  (void)v.front(); // { dg-error "in 'constexpr' expansion of" }
  return true;
}

template<bool Const, size_t N, typename T>
constexpr bool
test_back_on_empty()
{
  InplaceVector<Const, T, N> v;
  (void)v.back(); // { dg-error "in 'constexpr' expansion of" }
  return true;
}

template<bool Const, size_t N, typename T>
constexpr bool
test_out_of_capacity()
{
  InplaceVector<Const, T, N> v;
  (void)v[N+2]; // { dg-error "in 'constexpr' expansion of" }
  return true;
}

template<bool Const, typename T>
constexpr bool
test_out_of_size()
{
  InplaceVector<Const, T, 10> v{1, 2, 3, 4, 5};
  (void)v[7]; // { dg-error "in 'constexpr' expansion of" }
  return true;
}

static_assert(test_front_on_empty<false, 0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_front_on_empty<false, 4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_back_on_empty<false, 0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_back_on_empty<false, 4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_out_of_capacity<false, 0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_out_of_capacity<false, 4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_out_of_size<false, int>());        // { dg-error "in 'constexpr' expansion of" }

static_assert(test_front_on_empty<true, 0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_front_on_empty<true, 4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_back_on_empty<true, 0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_back_on_empty<true, 4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_out_of_capacity<true, 0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_out_of_capacity<true, 4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_out_of_size<true, int>());        // { dg-error "in 'constexpr' expansion of" }

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "is not a constant expression" }
// { dg-prune-output "call to non-'constexpr' function" }
