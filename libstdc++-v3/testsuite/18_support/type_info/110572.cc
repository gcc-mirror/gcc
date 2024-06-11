// { dg-options "-static-libstdc++" }
// { dg-require-static-libstdcxx }
// { dg-require-cpp-feature-test __cpp_rtti }
// { dg-do link }

#include <typeinfo>

int main()
{
  return typeid(0) == typeid(0u);
}
