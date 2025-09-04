// { dg-do compile { target c++26 } }
#include <type_traits>
#include <concepts>

#include <testsuite_hooks.h>

namespace adl {

struct Friend
{};

constexpr
int operator+(Friend, int x)
{ return x; };

template<typename T>
  struct TemplFriend
  { };

template<typename T>
  constexpr
  // templated, we cannot deduce T from cw<Friend<int>>
  int operator+(TemplFriend<T>, int x)
  { return x; };


struct HiddenFriend
{
  constexpr friend
  int operator+(HiddenFriend, int x)
  { return x; }
};

template<typename T>
  struct TemplHiddenFriend
  {
    constexpr friend
    // note that this not not template itself
    int operator+(TemplHiddenFriend, int x)
    { return x; }
  };
}

template<typename T>
  concept supportMixedObj = requires
  {
    { std::cw<T{}> + 1 } -> std::same_as<int>;
  };

template<typename T>
  concept supportMixedInt = requires(T t)
  {
    { t + std::cw<1> } -> std::same_as<int>;
  };

static_assert(supportMixedObj<adl::Friend>);
static_assert(supportMixedInt<adl::Friend>);
static_assert(!supportMixedObj<adl::TemplFriend<int>>);
static_assert(supportMixedInt<adl::TemplFriend<int>>);

static_assert(supportMixedObj<adl::HiddenFriend>);
static_assert(supportMixedInt<adl::HiddenFriend>);
static_assert(supportMixedObj<adl::TemplHiddenFriend<int>>);
static_assert(supportMixedInt<adl::TemplHiddenFriend<int>>);

struct Member
{
  constexpr
  // conversion for the first argument is not allowed
  int operator+(int x) const
  { return x; }
};

static_assert(!supportMixedObj<Member>);
static_assert(supportMixedInt<Member>);

struct ExplicitThisMember
{
  constexpr
  // conversion for the first argument is not allowed
  int operator+(this ExplicitThisMember, int x)
  { return x; }
};

static_assert(!supportMixedObj<ExplicitThisMember>);
static_assert(supportMixedInt<ExplicitThisMember>);
