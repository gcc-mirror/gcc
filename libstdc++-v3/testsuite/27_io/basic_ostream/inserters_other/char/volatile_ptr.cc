// { dg-options "-std=gnu++23 -fno-inline" }
// { dg-do link { target c++23 } }

#include <iostream>

int main()
{
  int i = 0;
  volatile void* p = &i;
  std::cout << p << std::endl;
}
