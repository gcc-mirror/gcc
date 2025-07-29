// { dg-do compile { target c++26 } }

#include <inplace_vector>

template<size_t N, typename T>
constexpr bool
test_unchecked_emplace_back()
{
  std::inplace_vector<T, N> v(N, T(1));
  v.unchecked_emplace_back(); // { dg-error "in 'constexpr' expansion of" }
  return true;
}

template<size_t N, typename T>
constexpr bool
test_push_back_lvalue()
{
  auto val = T();;
  std::inplace_vector<T, N> v(N, T(1));
  v.unchecked_push_back(val); // { dg-error "in 'constexpr' expansion of" }
  return true;
}

template<size_t N, typename T>
constexpr bool
test_push_back_rvalue()
{
  std::inplace_vector<T, N> v(N, T(1));
  v.unchecked_push_back(T()); // { dg-error "in 'constexpr' expansion of" }
  return true;
}

static_assert(test_unchecked_emplace_back<0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_unchecked_emplace_back<4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_push_back_lvalue<0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_push_back_lvalue<4, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_push_back_rvalue<0, int>()); // { dg-error "in 'constexpr' expansion of" }
static_assert(test_push_back_rvalue<4, int>()); // { dg-error "in 'constexpr' expansion of" }

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "is not a constant expression" }
// { dg-prune-output "call to non-'constexpr' function" }
