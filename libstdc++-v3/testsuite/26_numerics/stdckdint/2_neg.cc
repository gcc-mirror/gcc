// { dg-do compile { target c++26 } }

#include <stdckdint.h>

void
test_add(int i, char c, bool b)
{
  ::ckd_add(&i, c, 1); // { dg-error "here" }
  ::ckd_add(&i, 1, c); // { dg-error "here" }
  ::ckd_add(&i, b, 2); // { dg-error "here" }
  ::ckd_add(&i, 2, b); // { dg-error "here" }
  ::ckd_add(&c, 3, 3); // { dg-error "here" }
  ::ckd_add((const int*)&i, 4, 4); // { dg-error "here" }
}

void
test_sub(int i, char c, bool b)
{
  ::ckd_sub(&i, c, 1); // { dg-error "here" }
  ::ckd_sub(&i, 1, c); // { dg-error "here" }
  ::ckd_sub(&i, b, 2); // { dg-error "here" }
  ::ckd_sub(&i, 2, b); // { dg-error "here" }
  ::ckd_sub(&c, 3, 3); // { dg-error "here" }
  ::ckd_sub((const int*)&i, 4, 4); // { dg-error "here" }
}

void
test_mul(int i, char c, bool b)
{
  ::ckd_mul(&i, c, 1); // { dg-error "here" }
  ::ckd_mul(&i, 1, c); // { dg-error "here" }
  ::ckd_mul(&i, b, 2); // { dg-error "here" }
  ::ckd_mul(&i, 2, b); // { dg-error "here" }
  ::ckd_mul(&c, 3, 3); // { dg-error "here" }
  ::ckd_mul((const int*)&i, 4, 4); // { dg-error "here" }
}

// { dg-prune-output "static assertion failed" }
// { dg-prune-output "pointer to 'const'" }
