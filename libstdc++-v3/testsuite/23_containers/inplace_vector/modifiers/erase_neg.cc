// { dg-do compile { target c++26 } }

#include <inplace_vector>

template<size_t N, typename T>
constexpr bool
test_pop_back_on_empty()
{
  std::inplace_vector<T, N> v;
  v.pop_back(); // { dg-error "in 'constexpr' expansion of" }
  return true;
}

template<size_t N, typename T>
constexpr bool
test_erase_begin_on_empty()
{
  std::inplace_vector<T, N> v;
  v.erase(v.begin()); // { dg-error "in 'constexpr' expansion of" }
  return true;
}

template<size_t N, typename T>
constexpr bool
test_erase_end(size_t size = 0)
{
  std::inplace_vector<T, N> v(size, T());
  v.erase(v.end()); // { dg-error "in 'constexpr' expansion of" }
  return true;
}

static_assert(test_pop_back_on_empty<0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_pop_back_on_empty<4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_erase_begin_on_empty<0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_erase_begin_on_empty<4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_erase_end<0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_erase_end<4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_erase_end<4, int>(2)); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_erase_end<4, int>(4)); // { dg-error "in 'constexpr' expansion of" }

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "is not a constant expression" }
// { dg-prune-output "call to non-'constexpr' function" }
