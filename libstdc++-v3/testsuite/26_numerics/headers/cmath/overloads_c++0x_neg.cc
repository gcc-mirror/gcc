// { dg-do compile { target c++11 } }
// { dg-require-cmath "" }

// Copyright (C) 2011-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <cmath>

// libstdc++/48933

struct Foo { };

template Foo std::atan2<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::acosh<Foo>(Foo); // { dg-error "not match" }
template Foo std::asinh<Foo>(Foo); // { dg-error "not match" }
template Foo std::atanh<Foo>(Foo); // { dg-error "not match" }
template Foo std::cbrt<Foo>(Foo); // { dg-error "not match" }
template Foo std::copysign<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::erf<Foo>(Foo); // { dg-error "not match" }
template Foo std::erfc<Foo>(Foo); // { dg-error "not match" }
template Foo std::exp2<Foo>(Foo); // { dg-error "not match" }
template Foo std::expm1<Foo>(Foo); // { dg-error "not match" }
template Foo std::fdim<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::fma<Foo, Foo, Foo>(Foo(), Foo(), Foo()); // { dg-error "not match" }
template Foo std::fmax<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::fmin<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::hypot<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template int std::ilogb<Foo>(Foo); // { dg-error "not match" }
template Foo std::lgamma<Foo>(Foo); // { dg-error "not match" }
template long long std::llrint<Foo>(Foo); // { dg-error "not match" }
template long long std::llround<Foo>(Foo); // { dg-error "not match" }
template Foo std::log1p<Foo>(Foo); // { dg-error "not match" }
template Foo std::log2<Foo>(Foo); // { dg-error "not match" }
template Foo std::logb<Foo>(Foo); // { dg-error "not match" }
template long std::lrint<Foo>(Foo); // { dg-error "not match" }
template long std::lround<Foo>(Foo); // { dg-error "not match" }
template Foo std::nearbyint<Foo>(Foo); // { dg-error "not match" }
template Foo std::nextafter<Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::nexttoward<Foo>(Foo, long double); // { dg-error "not match" }
template Foo std::remainder<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::remquo<Foo>(Foo, Foo, int*); // { dg-error "not match" }
template Foo std::rint<Foo>(Foo); // { dg-error "not match" }
template Foo std::round<Foo>(Foo); // { dg-error "not match" }
template Foo std::scalbln<Foo>(Foo, long); // { dg-error "not match" }
template Foo std::scalbn<Foo>(Foo, int); // { dg-error "not match" }
template Foo std::tgamma<Foo>(Foo); // { dg-error "not match" }
template Foo std::trunc<Foo>(Foo); // { dg-error "not match" }
