// { dg-do compile { target c++17 } }

#include <type_traits>

template<class T> struct A { using type = typename T::type; };
using invalid = A<void>;

// [meta.logical]/3: For a specialization conjunction<B_1, ..., B_n>, if
// there is a template type argument B_i for which bool(B_i::value) is false,
// then instantiating conjunction<B_1, ..., B_n>::value does not require the
// instantiation of B_j::value for j > i.

static_assert(!std::conjunction_v<std::false_type, invalid>);
static_assert(!std::conjunction_v<std::false_type, invalid, invalid>);
static_assert(!std::conjunction_v<std::true_type, std::false_type, invalid>);
static_assert(!std::conjunction_v<std::true_type, std::false_type, invalid, invalid>);
static_assert(!std::conjunction_v<std::false_type,
				  std::conjunction<invalid>,
				  std::disjunction<invalid>,
				  std::negation<invalid>>);

// [meta.logical]/8: For a specialization disjunction<B_1, ..., B_n>, if
// there is a template type argument B_i for which bool(B_i::value) is true,
// then instantiating disjunction<B_1, ..., B_n>::value does not require the
// instantiation of B_j::value for j > i.

static_assert(std::disjunction_v<std::true_type, invalid>);
static_assert(std::disjunction_v<std::true_type, invalid, invalid>);
static_assert(std::disjunction_v<std::false_type, std::true_type, invalid>);
static_assert(std::disjunction_v<std::false_type, std::true_type, invalid, invalid>);
static_assert(std::disjunction_v<std::true_type,
				 std::conjunction<invalid>,
				 std::disjunction<invalid>,
				 std::negation<invalid>>);

#if __GLIBCXX__
// Also test the corresponding internal traits __and_, __or_ and __not_.
static_assert(!std::__and_v<std::false_type, invalid>);
static_assert(!std::__and_v<std::false_type, invalid, invalid>);
static_assert(!std::__and_v<std::true_type, std::false_type, invalid>);
static_assert(!std::__and_v<std::true_type, std::false_type, invalid, invalid>);
static_assert(!std::__and_v<std::false_type,
			    std::__and_<invalid>,
			    std::__or_<invalid>,
			    std::__not_<invalid>>);

static_assert(std::__or_v<std::true_type, invalid>);
static_assert(std::__or_v<std::true_type, invalid, invalid>);
static_assert(std::__or_v<std::false_type, std::true_type, invalid>);
static_assert(std::__or_v<std::false_type, std::true_type, invalid, invalid>);
static_assert(std::__or_v<std::true_type,
			  std::__and_<invalid>,
			  std::__or_<invalid>,
			  std::__not_<invalid>>);
#endif
