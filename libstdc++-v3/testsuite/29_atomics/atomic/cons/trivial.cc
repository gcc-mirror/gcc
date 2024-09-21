// { dg-do compile { target c++11 } }

#include <atomic>
#include <type_traits>

// C++20 / P0883R2 makes std::atomic value-initialize, so it's no
// longer going to be trivially default constructible; check that it
// still is in earlier language modes.
// (We are not applying that paper as a DR.)
#if __cpp_lib_atomic_value_initialization
constexpr bool atomic_default_ctor_is_trivial = false;
#else
constexpr bool atomic_default_ctor_is_trivial = true;
#endif

template<typename T>
using isTDC = std::is_trivially_default_constructible<T>;

static_assert(isTDC<std::atomic<bool>>::value == atomic_default_ctor_is_trivial);
static_assert(isTDC<std::atomic<char>>::value == atomic_default_ctor_is_trivial);
static_assert(isTDC<std::atomic<unsigned char>>::value == atomic_default_ctor_is_trivial);
static_assert(isTDC<std::atomic<int>>::value == atomic_default_ctor_is_trivial);
static_assert(isTDC<std::atomic<long>>::value == atomic_default_ctor_is_trivial);
static_assert(isTDC<std::atomic<unsigned long long>>::value == atomic_default_ctor_is_trivial);
static_assert(isTDC<std::atomic<int*>>::value == atomic_default_ctor_is_trivial);

struct DefaultConstructible
{
  int a;
  long long b;
  char* p;
};
static_assert(isTDC<std::atomic<DefaultConstructible>>::value == atomic_default_ctor_is_trivial);

struct NonDefaultConstructible
{
  NonDefaultConstructible(int i) : val(i) { }
  int val;
};
// Not default constructible, therefore not trivially default constructible
static_assert(isTDC<std::atomic<NonDefaultConstructible>>::value == false);
