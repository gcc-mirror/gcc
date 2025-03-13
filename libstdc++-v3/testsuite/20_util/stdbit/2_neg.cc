// { dg-do compile { target c++26 } }

#include <stdbit.h>

void
test_mandates()
{
  // Mandates: T is an unsigned integer type.

  ::stdc_leading_zeros(1); // { dg-error "here" }
  ::stdc_leading_ones(1); // { dg-error "here" }
  ::stdc_trailing_zeros(1); // { dg-error "here" }
  ::stdc_trailing_ones(1); // { dg-error "here" }
  ::stdc_first_leading_zero(1); // { dg-error "here" }
  ::stdc_first_leading_one(1); // { dg-error "here" }
  ::stdc_first_trailing_zero(1); // { dg-error "here" }
  ::stdc_first_trailing_one(1); // { dg-error "here" }
  ::stdc_count_zeros(1); // { dg-error "here" }
  ::stdc_count_ones(1); // { dg-error "here" }
  ::stdc_has_single_bit(1); // { dg-error "here" }
  ::stdc_bit_width(1); // { dg-error "here" }
  ::stdc_bit_floor(1); // { dg-error "here" }
  ::stdc_bit_ceil(1); // { dg-error "here" }

  ::stdc_leading_zeros(1.0); // { dg-error "here" }
  ::stdc_leading_ones(1.0); // { dg-error "here" }
  ::stdc_trailing_zeros(1.0); // { dg-error "here" }
  ::stdc_trailing_ones(1.0); // { dg-error "here" }
  ::stdc_first_leading_zero(1.0); // { dg-error "here" }
  ::stdc_first_leading_one(1.0); // { dg-error "here" }
  ::stdc_first_trailing_zero(1.0); // { dg-error "here" }
  ::stdc_first_trailing_one(1.0); // { dg-error "here" }
  ::stdc_count_zeros(1.0); // { dg-error "here" }
  ::stdc_count_ones(1.0); // { dg-error "here" }
  ::stdc_has_single_bit(1.0); // { dg-error "here" }
  ::stdc_bit_width(1.0); // { dg-error "here" }
  ::stdc_bit_floor(1.0); // { dg-error "here" }
  ::stdc_bit_ceil(1.0); // { dg-error "here" }
}

// { dg-prune-output "static assertion failed" }
// { dg-prune-output "no matching function" }
// { dg-prune-output "wrong type" }
// { dg-prune-output "invalid operands" }
// { dg-prune-output "non-integral type" }
