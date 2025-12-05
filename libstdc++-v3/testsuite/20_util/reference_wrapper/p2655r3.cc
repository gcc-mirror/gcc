// P2655R3 - common_reference_t of reference_wrapper Should Be a Reference Type
// Implemented as a DR against C++20
// { dg-do compile { target c++20 } }

#include <functional>

#if __cpp_lib_common_reference_wrapper != 202302L
# error "Feature-test macro __cpp_lib_common_reference_wrapper has wrong value in <functional>"
#endif

using std::is_same_v;
using std::common_reference_t;
using std::reference_wrapper;

static_assert( is_same_v<common_reference_t<const reference_wrapper<int>&, int&>, int&> );
static_assert( is_same_v<common_reference_t<int&, const reference_wrapper<int>&>, int&> );

static_assert( is_same_v<common_reference_t<reference_wrapper<int>, int&>,
			 common_reference_t<int&, int&>> );
static_assert( is_same_v<common_reference_t<reference_wrapper<int>, const int&>,
			 common_reference_t<int&, const int&>> );
static_assert( is_same_v<common_reference_t<reference_wrapper<const int>, int&>,
			 common_reference_t<const int&, int&>> );

static_assert( is_same_v<common_reference_t<int&, reference_wrapper<int>>,
			 common_reference_t<int&, int&>> );
static_assert( is_same_v<common_reference_t<const int&, reference_wrapper<int>>,
			 common_reference_t<int&, const int&>> );
static_assert( is_same_v<common_reference_t<int&, reference_wrapper<const int>>,
			 common_reference_t<const int&, int&>> );

static_assert( is_same_v<common_reference_t<reference_wrapper<int>&, reference_wrapper<int>&>,
			 reference_wrapper<int>&> );

static_assert( is_same_v<common_reference_t<reference_wrapper<char>,
					    reference_wrapper<int>>,
			 int> );
static_assert( is_same_v<common_reference_t<reference_wrapper<reference_wrapper<int>>,
					    reference_wrapper<int>>,
			 reference_wrapper<int>> );
static_assert( is_same_v<common_reference_t<reference_wrapper<int>,
					    reference_wrapper<reference_wrapper<int>>>,
			 reference_wrapper<int>> );

struct A { };
struct B { operator A&() const; };

template<typename T, typename U>
concept has_common_reference = requires {
  typename std::common_reference_t<T, U>;
};

static_assert( is_same_v<common_reference_t<reference_wrapper<A>, const B&>, A&> );
// reference_wrapper<const B> is not convertible to A&, as it would require two user
// defined conversions.
static_assert( !has_common_reference<A, reference_wrapper<const B>> );
static_assert( !has_common_reference<reference_wrapper<A>, reference_wrapper<const B>> );

struct D1 : A {};
struct D2 : A {};

template<template<class> typename Qual1, template<class> typename Qual2>
struct std::basic_common_reference<D1, D2, Qual1, Qual2>
 : std::common_reference<Qual1<A>, Qual2<A>>
{ };

template<template<class> typename Qual1, template<class> typename Qual2>
struct std::basic_common_reference<D2, D1, Qual1, Qual2>
 : std::common_reference<Qual1<A>, Qual2<A>>
{ };

static_assert( is_same_v<common_reference_t<D1&, D2&>, A&> );
static_assert( is_same_v<common_reference_t<reference_wrapper<D1>, D2&>, A&> );
static_assert( is_same_v<common_reference_t<D1&, reference_wrapper<D2>>, A&> );
static_assert( !has_common_reference<reference_wrapper<D1>, reference_wrapper<D2>> );
