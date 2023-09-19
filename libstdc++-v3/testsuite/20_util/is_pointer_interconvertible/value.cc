// { dg-do compile { target c++20 } }
// { dg-add-options no_pch }

#include <type_traits>

#ifndef __cpp_lib_is_pointer_interconvertible
# error "Feature test macro for is_pointer_interconvertible is missing in <type_traits>"
#elif __cpp_lib_is_pointer_interconvertible < 201907L
# error "Feature test macro for is_pointer_interconvertible has wrong value in <type_traits>"
#endif

static_assert( std::is_pointer_interconvertible_base_of<void, void>::value
		== std::is_pointer_interconvertible_base_of_v<void, void> );

struct B { };

static_assert( std::is_pointer_interconvertible_base_of<B, B>::value
		== std::is_pointer_interconvertible_base_of_v<B, B> );

static_assert( std::is_pointer_interconvertible_base_of_v<B, B> );
static_assert( std::is_pointer_interconvertible_base_of_v<B, const B> );
static_assert( std::is_pointer_interconvertible_base_of_v<const B, B> );
static_assert( std::is_pointer_interconvertible_base_of_v<const B, const B> );

struct D : B { int i; };

static_assert( std::is_pointer_interconvertible_base_of_v<D, D> );

static_assert( std::is_pointer_interconvertible_base_of_v<B, D> );
static_assert( std::is_pointer_interconvertible_base_of_v<const B, D> );
static_assert( std::is_pointer_interconvertible_base_of_v<B, const D> );
static_assert( std::is_pointer_interconvertible_base_of_v<const B, const D> );

static_assert( ! std::is_pointer_interconvertible_base_of_v<D, B> );

struct E : D { };
// E is not standard-layout
static_assert( ! std::is_pointer_interconvertible_base_of_v<E, B> );

struct D1 : B { };
struct D2 : B { };
struct D3 : D1, D2 { };
// B is ambiguously derived
static_assert( ! std::is_pointer_interconvertible_base_of_v<B, D3> );

union U;
static_assert( ! std::is_pointer_interconvertible_base_of_v<U, U> );
static_assert( ! std::is_pointer_interconvertible_base_of_v<U, D> );

struct I; // incomplete
static_assert( std::is_pointer_interconvertible_base_of_v<I, I> );
static_assert( std::is_pointer_interconvertible_base_of_v<I, const I> );
