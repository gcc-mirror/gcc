// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

// NB: This file is for testing type_traits with NO OTHER INCLUDES.

#include <type_traits>

void test01()
{
  // Check for required typedefs
  typedef std::is_consteval_only<decltype (^^int)> test_type;
  typedef test_type::value_type               value_type;
  typedef test_type::type                     type;
  typedef test_type::type::value_type         type_value_type;
  typedef test_type::type::type               type_type;
}
