// { dg-do compile { target c++11 } }

// LWG 3545. std::pointer_traits should be SFINAE-friendly

#include <memory>

using std::is_same;

template<typename> using void_t = void;

template<template<typename> class Probe, typename T, typename = void>
  struct has_member
  : std::false_type { };

template<template<typename> class Probe, typename T>
  struct has_member<Probe, T, void_t<Probe<T>>>
  : std::true_type { };

template<typename T>
  using element_type = typename T::element_type;
template<typename T>
  using pointer = typename T::pointer;
template<typename T>
  using difference_type = typename T::difference_type;
template<typename T>
  using rebind = typename T::template rebind<short>;
template<typename T>
  using pointer_to = decltype(T::pointer_to(std::declval<element_type<T>&>()));

using invalid = std::pointer_traits<int>;
invalid i; // invalid instantiation is not ill-formed

static_assert( !has_member<element_type, invalid>::value, "" );
static_assert( !has_member<pointer, invalid>::value, "" );
static_assert( !has_member<difference_type, invalid>::value, "" );
static_assert( !has_member<rebind, invalid>::value, "" );
static_assert( !has_member<pointer_to, invalid>::value, "" );

struct I
{
  // These members should not be used by pointer_traits<P>::pointer.
  using pointer = int;
  using difference_type = int;
  template<typename> using rebind = int;
};

using invalid2 = std::pointer_traits<I>;

static_assert( !has_member<element_type, invalid2>::value, "" );
static_assert( !has_member<pointer, invalid2>::value, "" );
static_assert( !has_member<difference_type, invalid2>::value, "" );
static_assert( !has_member<rebind, invalid2>::value, "" );
static_assert( !has_member<pointer_to, invalid2>::value, "" );

struct P
{
  using element_type = long;
  struct pointer { }; // should not be used by pointer_traits<P>::pointer

  P pointer_to(long&) const; // not static, should not be used.
};
using Ptraits = std::pointer_traits<P>;
Ptraits p;

static_assert( is_same<element_type<Ptraits>, long>::value, "" );
static_assert( is_same<pointer<Ptraits>, P>::value, "" );
static_assert( is_same<difference_type<Ptraits>, std::ptrdiff_t>::value, "" );
static_assert( !has_member<rebind, Ptraits>::value, "" );
#if __cplusplus >= 202002L
// pointer_traits<P>::pointer_to(long&) is constrained in C++20 and later.
static_assert( !has_member<pointer_to, Ptraits>::value, "" );
#else
static_assert( is_same<pointer_to<Ptraits>, P>::value, "" );
#endif

struct V { using element_type = const void; };
using Vtraits = std::pointer_traits<V>;
Vtraits v;

static_assert( is_same<element_type<Vtraits>, const void>::value, "" );
static_assert( is_same<pointer<Vtraits>, V>::value, "" );
static_assert( is_same<difference_type<Vtraits>, std::ptrdiff_t>::value, "" );
static_assert( !has_member<rebind, Vtraits>::value, "" );
static_assert( !has_member<pointer_to, Vtraits>::value, "" );

template<typename T>
struct clever_ptr
{
  static T obj;

  static clever_ptr pointer_to(T&) { return {}; }
  constexpr T* operator->() const { return &obj; }
};

using Ctraits = std::pointer_traits<clever_ptr<char>>;

static_assert( is_same<element_type<Ctraits>, char>::value, "" );
static_assert( is_same<pointer<Ctraits>, clever_ptr<char>>::value, "" );
static_assert( is_same<difference_type<Ctraits>, std::ptrdiff_t>::value, "" );
static_assert( is_same<rebind<Ctraits>, clever_ptr<short>>::value, "" );
static_assert( is_same<pointer_to<Ctraits>, clever_ptr<char>>::value, "" );

#ifdef __cpp_concepts
struct ptr_base { };

// Program-defined specialization must not be ambiguous with primary template.
template<typename P> requires std::derived_from<P, ptr_base>
struct std::pointer_traits<P>
{
  using element_type = int;
  using difference_type = long;
  using pointer = P;
};

struct Ptr : ptr_base { using element_type = int; };

using E = std::pointer_traits<Ptr>::element_type;
#endif
