// { dg-options "-static-libstdc++" }
// { dg-do link }
// { dg-require-static-libstdcxx "" }
// { dg-require-cpp-feature-test __cpp_rtti }

#include <typeinfo>

int main()
{
  return typeid(0) == typeid(0u);
}
