// { dg-do compile { target c++17 } }

#include <type_traits>

template<int> struct T : std::true_type { };
template<int> struct F : std::false_type { };

// [meta.logical]/5: The specialization conjunction<B_1, ..., B_n> has a
// public and unambiguous base that is either:
//   - the first type B_i in the list true_type, B_1, ..., B_n for which
//       bool(B_i::value) is false, or
//   - if there is no such B_i, the last type in the list.

static_assert(std::is_base_of_v<std::true_type, std::conjunction<>>);
static_assert(std::is_base_of_v<T<0>, std::conjunction<T<0>>>);
static_assert(std::is_base_of_v<F<0>, std::conjunction<F<0>>>);
static_assert(std::is_base_of_v<T<1>, std::conjunction<T<0>, T<1>>>);
static_assert(std::is_base_of_v<F<0>, std::conjunction<F<0>, F<1>>>);
static_assert(std::is_base_of_v<F<0>, std::conjunction<T<0>, F<0>, F<1>>>);
static_assert(std::is_base_of_v<F<0>, std::conjunction<T<0>, F<0>, T<1>, F<1>>>);

// [meta.logical]/10: The specialization disjunction<B_1, ..., B_n> has a
// public and unambiguous base that is either:
//   - the first type B_i in the list false_type, B_1, ..., B_n for which
//       bool(B_i::value) is true, or
//   - if there is no such B_i, the last type in the list.

static_assert(std::is_base_of_v<std::false_type, std::disjunction<>>);
static_assert(std::is_base_of_v<T<0>, std::disjunction<T<0>>>);
static_assert(std::is_base_of_v<F<0>, std::disjunction<F<0>>>);
static_assert(std::is_base_of_v<T<0>, std::disjunction<T<0>, T<1>>>);
static_assert(std::is_base_of_v<F<1>, std::disjunction<F<0>, F<1>>>);
static_assert(std::is_base_of_v<T<0>, std::disjunction<T<0>, F<0>, F<1>>>);
static_assert(std::is_base_of_v<T<0>, std::disjunction<T<0>, F<0>, T<1>, F<1>>>);
