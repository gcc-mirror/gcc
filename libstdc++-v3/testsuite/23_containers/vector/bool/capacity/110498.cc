// { dg-options "-O3 -Werror=array-bounds -fno-assume-sane-operators-new-delete" }
// { dg-do compile }

// Bug libstdc++/110498
// Spurious warnings stringop-overflow and array-bounds copying data as bytes
// into vector::reserve

#include <vector>

void f(std::vector<bool>& v)
{
  // Warning emitted when set to any number in the range [1,64].
  const std::size_t reserve_size = 30;

  v.reserve(reserve_size);
  v.push_back(0);
}

