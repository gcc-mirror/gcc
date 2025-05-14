// { dg-do compile { target c++26 } }

#include <functional>

#ifndef __cpp_lib_function_ref
# error "Feature-test macro for function_ref missing in <functional>"
#elif __cpp_lib_function_ref != 202306L
# error "Feature-test macro for function_ref has wrong value in <functional>"
#endif

using std::nontype;
using std::nontype_t;
using std::function_ref;

using std::is_nothrow_move_assignable_v;
using std::is_nothrow_copy_assignable_v;
using std::is_nothrow_assignable_v;
using std::is_assignable_v;
using std::is_nothrow_swappable_v;
using std::is_trivially_copyable_v;

static_assert( is_nothrow_move_assignable_v<function_ref<void()>> );
static_assert( is_nothrow_copy_assignable_v<function_ref<void()>> );
static_assert( is_nothrow_swappable_v<function_ref<void()>> );

static_assert( ! is_assignable_v<function_ref<void()>, std::nullptr_t> );

static_assert( is_nothrow_assignable_v<function_ref<void()>, void()> );
static_assert( is_nothrow_assignable_v<function_ref<void()>, void(&)()> );
static_assert( is_nothrow_assignable_v<function_ref<void()>, void(*)()> );
static_assert( is_nothrow_assignable_v<function_ref<void()>, int()> );
static_assert( is_nothrow_assignable_v<function_ref<void()>, int(&)()> );
static_assert( is_nothrow_assignable_v<function_ref<void()>, int(*)()> );
static_assert( ! is_nothrow_assignable_v<function_ref<void()>, void(int)> );
static_assert( is_nothrow_assignable_v<function_ref<void(int)>, void(int)> );

static_assert( is_nothrow_assignable_v<function_ref<void()>,
				       void() noexcept> );
static_assert( is_nothrow_assignable_v<function_ref<void() noexcept>,
					void() noexcept> );
static_assert( ! is_assignable_v<function_ref<void() noexcept>, void() > );

struct S
{
  int x;
  int f();
};
int funS(S);

static_assert( is_nothrow_assignable_v<function_ref<int(S)>,
				       decltype(funS)> );
static_assert( is_nothrow_assignable_v<function_ref<int(S)>,
				       decltype(&funS)> );
static_assert( ! is_assignable_v<function_ref<int(S)>, decltype(&S::x)> );
static_assert( ! is_assignable_v<function_ref<int(S)>, decltype(&S::f)> );

static_assert( is_nothrow_assignable_v<function_ref<int(S)>,
				       nontype_t<funS>> );
static_assert( is_nothrow_assignable_v<function_ref<int(S)>,
				       nontype_t<&funS>> );
static_assert( is_nothrow_assignable_v<function_ref<int(S)>,
				       nontype_t<&S::x>> );
static_assert( is_nothrow_assignable_v<function_ref<int(S)>,
				       nontype_t<&S::f>> );
struct Q
{
  void operator()() const;
};

static_assert( ! is_assignable_v<function_ref<void()>, Q> );
static_assert( ! is_assignable_v<function_ref<void()>, Q&> );
static_assert( ! is_assignable_v<function_ref<void()>, const Q&> );
static_assert( ! is_assignable_v<function_ref<void() const>, Q> );
static_assert( ! is_assignable_v<function_ref<void() const>, Q&> );
static_assert( ! is_assignable_v<function_ref<void() const>, const Q&> );

static_assert( is_nothrow_assignable_v<function_ref<void()>,
				       nontype_t<Q{}>> );
static_assert( is_nothrow_assignable_v<function_ref<void() const>,
				       nontype_t<Q{}>> );

constexpr bool
test_constexpr()
{
  function_ref<void(S)> fp(nontype<funS>);
  fp = nontype<funS>;
  fp = nontype<&funS>;
  fp = nontype<&S::x>;
  fp = nontype<&S::f>;

  constexpr Q cq;
  function_ref<void() const> fq(cq);
  fq = nontype<cq>;
  return true;
}
static_assert( test_constexpr() );

void func();

void
test_instantiation()
{
  function_ref<void(S)> fp(funS);
  fp = funS;
  fp = &funS;

  test_constexpr();
}
