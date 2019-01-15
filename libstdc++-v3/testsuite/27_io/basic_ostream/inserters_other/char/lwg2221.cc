// { dg-options "-std=gnu++17 -fno-inline" }
// { dg-do link }

#include <iostream>

int main()
{
  std::cout << nullptr << std::endl;
}
