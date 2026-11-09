// { dg-options "-freflection" }
// { dg-do compile { target c++26 } }
// { dg-require-effective-target cxx11_abi }

#include <exception>
#include <meta>

consteval bool test_exception_ptr_is_type_alias()
{
  auto exptr = ^^std::exception_ptr;
  return std::meta::is_type_alias(exptr)
    && std::meta::dealias(exptr) == ^^decltype(std::current_exception());
}

static_assert( test_exception_ptr_is_type_alias() );
