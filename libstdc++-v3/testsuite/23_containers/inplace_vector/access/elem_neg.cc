// { dg-do compile { target c++26 } }

#include <inplace_vector>

template<size_t N, typename T>
constexpr bool
test_out_of_capacity()
{
  std::inplace_vector<T, N> v;
  (void)v[N+2]; // { dg-error "in 'constexpr' expansion of" }
  return true;
}

template<typename T>
constexpr bool
test_out_of_size()
{
  std::inplace_vector<T, 10> v{1, 2, 3, 4, 5};
  (void)v[7]; // { dg-error "in 'constexpr' expansion of" }
  return true;
}

static_assert(test_out_of_capacity<0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_out_of_capacity<4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_out_of_size<int>());        // { dg-error "in 'constexpr' expansion of" }

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "is not a constant expression" }
// { dg-prune-output "call to non-'constexpr' function" }
