// { dg-do link }

// Actually not needed: -fipa-cp is default with -O2:
// { dg-additional-options "-O2 -fipa-cp" }

// The code failed because 'std::endl' becoḿes implicitly 'declare target'
// but not the 'widen' function it calls.  While the linker had no issues
// (endl is never called, either because it is inlined or optimized away),
// the IPA-CP (enabled by -O2 and higher) failed as the definition for
// 'widen' did not exist on the offload side.

#include <iostream>

void func (int m)
{
  if (m < 0)
    std::cout << "should not happen" << std::endl;
}


int main()
{
  #pragma omp target
    func (1);
}
