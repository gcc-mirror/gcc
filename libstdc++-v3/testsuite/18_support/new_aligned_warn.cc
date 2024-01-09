// { dg-options "-Wattributes" }
// { dg-do compile { target c++17 } }

#include <new>

int main()
{
  // PR c++/86878 has a patch to make these warn.
  (void) operator new(1, std::align_val_t(3)); // { dg-warning "power of two" "" { xfail *-*-* } }
  (void) operator new[](1, std::align_val_t(10)); // { dg-warning "power of two" "" { xfail *-*-* } }
  (void) operator new(1, std::align_val_t(0), std::nothrow_t()); // { dg-warning "power of two" "" { xfail *-*-* } }
  (void) operator new[](1, std::align_val_t(-1), std::nothrow_t()); // { dg-warning "power of two" "" { xfail *-*-* } }
}
