// { dg-do compile { target c++26 } }

#include <functional>
#include <type_traits>

using std::is_same_v;
using std::nontype;
using std::nontype_t;
using std::function_ref;

int i = 0;

void f0();
void f0n() noexcept;

static_assert( is_same_v<decltype(function_ref(f0)),
			 function_ref<void()>> );
static_assert( is_same_v<decltype(function_ref(f0n)),
			 function_ref<void() noexcept>> );
static_assert( is_same_v<decltype(function_ref(nontype<f0>)),
			 function_ref<void()>> );
static_assert( is_same_v<decltype(function_ref(nontype<f0n>)),
			 function_ref<void() noexcept>> );

void f1(int);
void f1n(int) noexcept;

static_assert( is_same_v<decltype(function_ref(f1)),
			 function_ref<void(int)>> );
static_assert( is_same_v<decltype(function_ref(f1n)),
			 function_ref<void(int) noexcept>> );
static_assert( is_same_v<decltype(function_ref(nontype<f1>)),
			 function_ref<void(int)>> );
static_assert( is_same_v<decltype(function_ref(nontype<f1n>)),
			 function_ref<void(int) noexcept>> );
static_assert( is_same_v<decltype(function_ref(nontype<f1>, i)),
			 function_ref<void()>> );
static_assert( is_same_v<decltype(function_ref(nontype<f1n>, i)),
			 function_ref<void() noexcept>> );

void f2(int*, int);
void f2n(int*, int) noexcept;

static_assert( is_same_v<decltype(function_ref(f2)),
			 function_ref<void(int*, int)>> );
static_assert( is_same_v<decltype(function_ref(f2n)),
			 function_ref<void(int*, int) noexcept>> );
static_assert( is_same_v<decltype(function_ref(nontype<f2>)),
			 function_ref<void(int*, int)>> );
static_assert( is_same_v<decltype(function_ref(nontype<f2n>)),
			 function_ref<void(int*, int) noexcept>> );
static_assert( is_same_v<decltype(function_ref(nontype<f2>, &i)),
			 function_ref<void(int)>> );
static_assert( is_same_v<decltype(function_ref(nontype<f2n>, &i)),
			 function_ref<void(int) noexcept>> );

struct S
{
  int mem;
  int f();
  int fn() noexcept;

  int fc(int) const;
  int fcn(int) const noexcept;

  int fl(int) &;
  int fln(int) & noexcept;

  int fcl(float) const&;
  int fcln(float) const& noexcept;
};
S s{};
const S cs{};

static_assert( is_same_v<decltype(function_ref(nontype<&S::mem>, s)),
			 function_ref<int&()>> );
static_assert( is_same_v<decltype(function_ref(nontype<&S::mem>, cs)),
			 function_ref<const int&()>> );
static_assert( is_same_v<decltype(function_ref(nontype<&S::mem>, &s)),
			 function_ref<int&()>> );
static_assert( is_same_v<decltype(function_ref(nontype<&S::mem>, &cs)),
			 function_ref<const int&()>> );

static_assert( is_same_v<decltype(function_ref(nontype<&S::f>, s)),
			 function_ref<int()>> );
static_assert( is_same_v<decltype(function_ref(nontype<&S::fn>, &s)),
			 function_ref<int() noexcept>> );

static_assert( is_same_v<decltype(function_ref(nontype<&S::fc>, &s)),
			 function_ref<int(int)>> );
static_assert( is_same_v<decltype(function_ref(nontype<&S::fcn>, s)),
			 function_ref<int(int) noexcept>> );

static_assert( is_same_v<decltype(function_ref(nontype<&S::fl>, &s)),
			 function_ref<int(int)>> );
static_assert( is_same_v<decltype(function_ref(nontype<&S::fln>, s)),
			 function_ref<int(int) noexcept>> );

static_assert( is_same_v<decltype(function_ref(nontype<&S::fcl>, s)),
			 function_ref<int(float)>> );
static_assert( is_same_v<decltype(function_ref(nontype<&S::fcln>, &s)),
			 function_ref<int(float) noexcept>> );

