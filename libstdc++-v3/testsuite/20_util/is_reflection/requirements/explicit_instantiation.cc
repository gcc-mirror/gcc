// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

// NB: This file is for testing type_traits with NO OTHER INCLUDES.

#include <type_traits>

namespace std
{
  typedef short test_type;
  template struct is_reflection<test_type>;
}
