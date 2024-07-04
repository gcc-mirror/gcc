// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

#include <scoped_allocator>

template<typename T>
struct Alloc
{
  using value_type = T;

  Alloc() noexcept(false) { }
  Alloc(const Alloc&) noexcept(false) { }
  template<typename U> Alloc(const Alloc<U>&) noexcept { }

  T* allocate(std::size_t);
  void deallocate(T*, std::size_t);

  bool operator==(const Alloc&) const noexcept(false) { return true; }
  bool operator!=(const Alloc&) const noexcept(false) { return false; }
};

using A1 = std::allocator<int>;
using A2 = std::allocator<long>;
using Ax = Alloc<int>;
using SA1 = std::scoped_allocator_adaptor<A1, Ax>;
using SA2 = std::scoped_allocator_adaptor<A2, Ax>;
static_assert( std::is_default_constructible<SA1>::value
		 && ! std::is_nothrow_default_constructible<SA1>::value,
	       "default constructor is potentially-throwing" );
static_assert( std::is_nothrow_constructible<SA1, A1, Ax>::value,
	       "multi-arg constructor is non-throwing" );
static_assert( std::is_nothrow_copy_constructible<SA1>::value,
	       "copy constructor is non-throwing" );
static_assert( std::is_nothrow_move_constructible<SA1>::value,
	       "move constructor is non-throwing" );
static_assert( std::is_nothrow_constructible<SA1, const SA2&>::value,
	       "converting copy constructor is non-throwing" );
static_assert( std::is_nothrow_constructible<SA1, SA2>::value,
	       "converting move constructor is non-throwing" );

static_assert( noexcept(std::declval<SA1&>().inner_allocator()),
	       "inner_allocator() is non-throwing" );
static_assert( noexcept(std::declval<const SA1&>().inner_allocator()),
	       "inner_allocator() const is non-throwing" );
static_assert( noexcept(std::declval<SA1&>().outer_allocator()),
	       "outer_allocator() is non-throwing" );
static_assert( noexcept(std::declval<const SA1&>().outer_allocator()),
	       "outer_allocator() const is non-throwing" );
