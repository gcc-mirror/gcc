// { dg-do compile { target c++11 } }

// LWG 3778. vector<bool> missing exception specifications

#include <vector>
#include <testsuite_allocator.h>

using V = std::vector<bool>;

static_assert(std::is_nothrow_default_constructible<V>::value,
	      "nothrow default constructible with std::allocator");
static_assert(std::is_nothrow_constructible<V, V::allocator_type>::value,
	      "nothrow constructible with allocator argument");
static_assert(std::is_nothrow_move_constructible<V>::value,
	      "unconditionally nothrow move constructible");
static_assert(std::is_nothrow_move_assignable<V>::value,
	      "nothrow move assignment with std::allocator");
static_assert(std::__is_nothrow_swappable<V>::value,
	      "nothrow swap with std::allocator");

template<typename T>
struct Allocator : __gnu_test::SimpleAllocator<T>
{
  Allocator() noexcept(false) { }

  template<typename U>
    Allocator(const Allocator<U>&) { }
};

using V2 = std::vector<bool, Allocator<bool>>;
static_assert(std::is_default_constructible<V2>::value,
	      "default constructible with Allocator<bool>");
static_assert(std::is_nothrow_constructible<V2, V2::allocator_type>::value,
	      "nothrow constructible with allocator argument");
static_assert(! std::is_nothrow_default_constructible<V2>::value,
	      "but not nothrow default constructible with Allocator<bool>");
static_assert(std::is_nothrow_move_constructible<V2>::value,
	      "also checked by ./noexcept_move_construct.cc");
static_assert(std::__is_nothrow_swappable<V>::value,
	      "nothrow swap with std::allocator");

template<typename T, typename IAE, typename POCMA = IAE>
struct PropAllocator : __gnu_test::SimpleAllocator<T>
{
  PropAllocator() noexcept { }

  template<typename U>
    PropAllocator(const PropAllocator<U, IAE, POCMA>&) { }

  using is_always_equal = POCMA;
  using propagate_on_container_move_assignment = IAE;
};

using V3 = std::vector<bool, PropAllocator<bool, std::false_type>>;
static_assert(std::is_nothrow_move_constructible<V3>::value,
	      "unconditionally nothrow move constructible");
static_assert(std::is_nothrow_constructible<V3, V3::allocator_type>::value,
	      "nothrow constructible with allocator argument");
static_assert(! std::is_nothrow_move_assignable<V3>::value,
	      "throwing move assignment with !(propagating || equal) alloc");
#ifdef _GLIBCXX_RELEASE
// We strengthen std::vector<bool, A>::swap to be always noexcept.
static_assert(std::__is_nothrow_swappable<V3>::value,
	      "nothrow swap even with !(propagating || equal) alloc");
#endif

using V4
  = std::vector<bool, PropAllocator<bool, std::false_type, std::true_type>>;
static_assert(std::is_nothrow_constructible<V4, V4::allocator_type>::value,
	      "nothrow constructible with allocator argument");
static_assert(std::is_nothrow_move_constructible<V4>::value,
	      "unconditionally nothrow move constructible");
static_assert(std::is_nothrow_move_assignable<V4>::value,
	      "nothrow move assignment with propagating alloc");
static_assert(std::__is_nothrow_swappable<V4>::value,
	      "nothrow swap with always-equal alloc");

using V5
  = std::vector<bool, PropAllocator<bool, std::true_type, std::false_type>>;
static_assert(std::is_nothrow_constructible<V5, V5::allocator_type>::value,
	      "nothrow constructible with allocator argument");
static_assert(std::is_nothrow_move_constructible<V5>::value,
	      "unconditionally nothrow move constructible");
static_assert(std::is_nothrow_move_assignable<V5>::value,
	      "nothrow move assignment with always-equal alloc");
static_assert(std::__is_nothrow_swappable<V5>::value,
	      "nothrow swap with always-equal alloc");
