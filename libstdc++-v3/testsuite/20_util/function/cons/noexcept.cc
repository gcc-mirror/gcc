// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

#include <functional>

struct X
{
  void operator()(X*);

  char bigness[100];
};

using F = std::function<void(X*)>;

static_assert( std::is_nothrow_constructible<F>::value, "" );
static_assert( std::is_nothrow_constructible<F, F>::value, "" );
static_assert( ! std::is_nothrow_constructible<F, F&>::value, "" );
static_assert( ! std::is_nothrow_constructible<F, const F&>::value, "" );
static_assert( std::is_nothrow_constructible<F, std::nullptr_t>::value, "" );

static_assert( ! std::is_nothrow_constructible<F, X>::value, "" );
using R = std::reference_wrapper<X>;
static_assert( std::is_nothrow_constructible<F, R>::value, "" );


// The standard requires that construction from a function pointer type
// does not throw, but doesn't require that the construction is noexcept.
// Strengthening that noexcept for these types is a GCC extension.
static_assert( std::is_nothrow_constructible<F, void(*)(X*)>::value, "" );
// This is a GCC extension, not required by the standard:
static_assert( std::is_nothrow_constructible<F, void(&)(X*)>::value, "" );
// This is a GCC extension, not required by the standard:
static_assert( std::is_nothrow_constructible<F, void(X::*)()>::value, "" );

auto c = [](X*){};
static_assert( std::is_nothrow_constructible<F, decltype(+c)>::value, "" );
// The standard allows this to throw, but as a GCC extenension we store
// closures with no captures in the std::function, so this is noexcept too:
static_assert( std::is_nothrow_constructible<F, decltype(c)>::value, "" );
