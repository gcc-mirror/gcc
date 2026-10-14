// { dg-do run { target c++23 } }
// { dg-require-effective-target cxx11_abi }
// { dg-options "-fno-access-control -fno-inline -fno-ipa-sra -fno-ipa-modref -fno-ipa-cp -fno-ipa-icf" }

// libstdc++/127228
// Object files built against older libstdc++ still call _M_create
// and rely on it to allocate and report allocating no more storage
// than was asked for when the old capacity is 0.

#include <string>
#include <testsuite_hooks.h>

int main()
{
  std::string s;
  {
    std::string::size_type new_cap = 25, old_cap = 0;
    (void) s._M_create(new_cap, old_cap);
    VERIFY(new_cap == 25);
  }
  {
    std::string::size_type new_cap = 2, old_cap = 0;
    (void) s._M_create(new_cap, old_cap);
    VERIFY(new_cap == 2);
  }
}
