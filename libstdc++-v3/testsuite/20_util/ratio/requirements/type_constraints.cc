// { dg-do compile { target c++11 } }

// C++11 20.10.1 [ratio.general]
// if the template argument types R1 and R2 are not specializations of the
// ratio template, the program is ill-formed.

#include <ratio>

using namespace std;

// A type that looks like std::ratio but isn't.
template<int> struct Ratty { static constexpr int num = 1, den = 1; };

using T1 = ratio_add<Ratty<1>, Ratty<1>>::type; // { dg-error "here" }
using T2 = ratio_subtract<Ratty<3>, Ratty<3>>::type; // { dg-error "here" }
using T3 = ratio_multiply<Ratty<3>, Ratty<3>>::type; // { dg-error "here" }
using T4 = ratio_divide<Ratty<4>, Ratty<4>>::type; // { dg-error "here" }
using T5 = ratio_equal<Ratty<5>, Ratty<5>>::type; // { dg-error "here" }
using T6 = ratio_not_equal<Ratty<6>, Ratty<6>>::type; // { dg-error "here" }
using T7 = ratio_less<Ratty<7>, Ratty<7>>::type; // { dg-error "here" }
using T8 = ratio_less_equal<Ratty<8>, Ratty<8>>::type; // { dg-error "here" }
using T9 = ratio_greater<Ratty<9>, Ratty<9>>::type; // { dg-error "here" }
using T10 = ratio_greater_equal<Ratty<10>, Ratty<10>>::type; // { dg-error "here" }

#if __cplusplus >= 201703L
bool B11 = ratio_equal_v<Ratty<11>, Ratty<11>>; // { dg-error "here" "" { target c++17 } }
bool B12 = ratio_not_equal_v<Ratty<12>, Ratty<12>>; // { dg-error "here" "" { target c++17 } }
bool B13 = ratio_less_v<Ratty<13>, Ratty<13>>; // { dg-error "here" "" { target c++17 } }
bool B14 = ratio_less_equal_v<Ratty<14>, Ratty<14>>; // { dg-error "here" "" { target c++17 } }
bool B15 = ratio_greater_v<Ratty<15>, Ratty<15>>; // { dg-error "here" "" { target c++17 } }
bool B16 = ratio_greater_equal_v<Ratty<16>, Ratty<16>>; // { dg-error "here" "" { target c++17 } }
#endif

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
