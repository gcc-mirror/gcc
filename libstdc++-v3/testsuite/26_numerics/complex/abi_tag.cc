// Test that the C++11 variants of real/imag have an ABI tag
// { dg-do compile }
// { dg-options -std=c++11 }

#include <complex>

// { dg-final { scan-assembler "_ZNKSt7complexIfE4realB5cxx11Ev" } }
float (std::complex<float>::*p1)() const = &std::complex<float>::real;
// { dg-final { scan-assembler "_ZNKSt7complexIdE4realB5cxx11Ev" } }
double (std::complex<double>::*p2)() const = &std::complex<double>::real;
// { dg-final { scan-assembler "_ZNKSt7complexI\[eg\]E4realB5cxx11Ev" } }
long double (std::complex<long double>::*p3)() const
  = &std::complex<long double>::real;
// { dg-final { scan-assembler "_ZNKSt7complexIiE4realB5cxx11Ev" } }
int (std::complex<int>::*p4)() const = &std::complex<int>::real;

// { dg-final { scan-assembler "_ZNKSt7complexIfE4imagB5cxx11Ev" } }
float (std::complex<float>::*p5)() const = &std::complex<float>::imag;
// { dg-final { scan-assembler "_ZNKSt7complexIdE4imagB5cxx11Ev" } }
double (std::complex<double>::*p6)() const = &std::complex<double>::imag;
// { dg-final { scan-assembler "_ZNKSt7complexI\[eg\]E4imagB5cxx11Ev" } }
long double (std::complex<long double>::*p7)() const
  = &std::complex<long double>::imag;
// { dg-final { scan-assembler "_ZNKSt7complexIiE4imagB5cxx11Ev" } }
int (std::complex<int>::*p8)() const = &std::complex<int>::imag;
