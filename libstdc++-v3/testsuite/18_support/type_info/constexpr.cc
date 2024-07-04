// { dg-options "-frtti" }
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }
// { dg-add-options no_pch }

#include <typeinfo>

#ifndef __cpp_lib_constexpr_typeinfo
# error "Feature-test macro for constexpr typeinfo missing in <typeinfo>"
#elif __cpp_lib_constexpr_typeinfo != 202106L
# error "Feature-test macro for constexpr typeinfo has wrong value in <typeinfo>"
#endif

struct X { };

constexpr bool
test01()
{
  if (typeid(int) == typeid(long))
    return false;

  if (typeid(int) != typeid(int))
    return false;

  struct X { virtual ~X() { } };

  if (typeid(X) != typeid(X))
    return false;

  if (typeid(X) == typeid(::X))
    return false;

  if (typeid(X) == typeid(int))
    return false;

  const auto& ti_x = typeid(X);
  if (ti_x != ti_x)
    return false;

  if (ti_x != typeid(X))
    return false;

  struct Y { };
  if (ti_x == typeid(Y))
    return false;

  return true;
}

static_assert( test01() );
