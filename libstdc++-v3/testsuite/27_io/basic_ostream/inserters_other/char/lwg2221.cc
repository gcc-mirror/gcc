// { dg-options "-fno-inline" }
// { dg-do link { target c++17 } }

#include <iostream>

int main()
{
  std::cout << nullptr << std::endl;
}
