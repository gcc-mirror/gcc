// { dg-do run { xfail broken_cplxf_arg } }
// PR libstdc++/10689
// Origin: Daniel.Levine@jhuaph.edu
// { dg-options "-mieee" { target alpha*-*-* } }

#include <complex>
#include <testsuite_hooks.h>

int main()
{
   std::complex<double> z;

   VERIFY( pow(z, 1.0/3.0) == 0.0 );

   return 0;
}
