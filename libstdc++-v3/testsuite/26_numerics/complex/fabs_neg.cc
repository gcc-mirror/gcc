// { dg-do compile }
// Bug 120235 std::fabs(const std::complex<T>&) should not be defined

#include <complex>

void test_pr120235(std::complex<double> c)
{
  (void) std::fabs(c);
  // { dg-error "no matching function" "" { target c++98_only } 8 }
  // { dg-warning "deprecated: use 'std::abs'" "" { target c++11 } 8 }
}

// { dg-prune-output "no type named '__type'" }
