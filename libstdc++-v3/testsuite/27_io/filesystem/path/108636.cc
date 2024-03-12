// { dg-do link { target c++17 } }
// { dg-options "-fkeep-inline-functions" }
// { dg-require-filesystem-ts "" }

#include <filesystem>
int main()
{
  // PR libstdc++/108636 - link failure with -fkeep-inline-functions
}
