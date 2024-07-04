// { dg-do compile }
// { dg-require-cmath "" }

// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

// 8.16.4 Additional overloads

#include <tr1/cmath>

// libstdc++/48933

struct Foo { };

template Foo std::tr1::atan2<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::tr1::acosh<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::asinh<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::atanh<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::cbrt<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::copysign<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::tr1::erf<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::erfc<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::exp2<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::expm1<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::fdim<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::tr1::fma<Foo, Foo, Foo>(Foo(), Foo(), Foo()); // { dg-error "not match" }
template Foo std::tr1::fmax<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::tr1::fmin<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::tr1::hypot<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template int std::tr1::ilogb<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::lgamma<Foo>(Foo); // { dg-error "not match" }
template long long std::tr1::llrint<Foo>(Foo); // { dg-error "not match" }
template long long std::tr1::llround<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::log1p<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::log2<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::logb<Foo>(Foo); // { dg-error "not match" }
template long std::tr1::lrint<Foo>(Foo); // { dg-error "not match" }
template long std::tr1::lround<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::nearbyint<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::nextafter<Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::tr1::nexttoward<Foo>(Foo, long double); // { dg-error "not match" }
template Foo std::tr1::remainder<Foo, Foo>(Foo, Foo); // { dg-error "not match" }
template Foo std::tr1::remquo<Foo>(Foo, Foo, int*); // { dg-error "not match" }
template Foo std::tr1::rint<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::round<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::scalbln<Foo>(Foo, long); // { dg-error "not match" }
template Foo std::tr1::scalbn<Foo>(Foo, int); // { dg-error "not match" }
template Foo std::tr1::tgamma<Foo>(Foo); // { dg-error "not match" }
template Foo std::tr1::trunc<Foo>(Foo); // { dg-error "not match" }
