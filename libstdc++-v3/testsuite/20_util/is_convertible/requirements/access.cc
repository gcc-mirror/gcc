// { dg-do compile { target  c++11 } }
// PR c++/107049

#include <type_traits>

class Private
{
  operator int() const
  {
    static_assert( not std::is_convertible<Private, int>::value, "" );
#if __cpp_lib_type_trait_variable_templates
    static_assert( not std::is_convertible_v<Private, int>, "" );
#endif
    return 0;
  }
};

static_assert( not std::is_convertible<Private, int>::value, "" );
