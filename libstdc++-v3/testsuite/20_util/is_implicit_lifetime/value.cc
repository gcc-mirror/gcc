// Copyright (C) 2025-2026 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <type_traits>

#ifndef __cpp_lib_is_implicit_lifetime
# error "Feature test macro for is_implicit_lifetime is missing in <type_traits>"
#elif __cpp_lib_is_implicit_lifetime < 202302L
# error "Feature test macro for is_implicit_lifetime has wrong value in <type_traits>"
#endif

#include <testsuite_tr1.h>

template<typename T>
  concept Is_implicit_lifetime
    = __gnu_test::test_category<std::is_implicit_lifetime, T>(true);

static_assert( ! Is_implicit_lifetime<void> );
static_assert( ! Is_implicit_lifetime<const void> );
static_assert( ! Is_implicit_lifetime<volatile void> );
static_assert( Is_implicit_lifetime<char> );
static_assert( Is_implicit_lifetime<signed char> );
static_assert( Is_implicit_lifetime<const unsigned char> );
static_assert( Is_implicit_lifetime<short> );
static_assert( Is_implicit_lifetime<volatile unsigned short> );
static_assert( Is_implicit_lifetime<int> );
static_assert( Is_implicit_lifetime<unsigned int> );
static_assert( Is_implicit_lifetime<const volatile long> );
static_assert( Is_implicit_lifetime<unsigned long> );
static_assert( Is_implicit_lifetime<long long> );
static_assert( Is_implicit_lifetime<unsigned long long> );
static_assert( Is_implicit_lifetime<float> );
static_assert( Is_implicit_lifetime<double> );
static_assert( Is_implicit_lifetime<long double volatile> );
enum W { W1 };
static_assert( Is_implicit_lifetime<W> );
enum class X : int { X1 };
static_assert( Is_implicit_lifetime<const volatile X> );
static_assert( Is_implicit_lifetime<int *> );
static_assert( Is_implicit_lifetime<int (*) (int)> );
struct Y { int g; int foo (int); };
static_assert( Is_implicit_lifetime<int (Y::*)> );
static_assert( Is_implicit_lifetime<int (Y::*) (int)> );
static_assert( ! Is_implicit_lifetime<int &> );
static_assert( ! Is_implicit_lifetime<char &&> );
static_assert( Is_implicit_lifetime<int []> );
static_assert( Is_implicit_lifetime<int [1]> );
static_assert( Is_implicit_lifetime<const Y [42]> );
static_assert( ! Is_implicit_lifetime<int ()> );
static_assert( ! Is_implicit_lifetime<int () &> );
static_assert( ! Is_implicit_lifetime<int () const> );
static_assert( ! Is_implicit_lifetime<int (&) ()> );
struct Z;
static_assert( Is_implicit_lifetime<Z []> );
static_assert( Is_implicit_lifetime<Z [5]> );
struct A { int a, b, c; };
static_assert( Is_implicit_lifetime<A> );
class B { static int a; private: static int b; public: int c; };
static_assert( Is_implicit_lifetime<B> );
struct C { C () {} int a, b, c; };
static_assert( Is_implicit_lifetime<C> );
struct D { explicit D (int) {} int a, b, c; };
static_assert( Is_implicit_lifetime<D> );
struct E : public A { int d, e, f; };
static_assert( Is_implicit_lifetime<E> );
struct F : public C { using C::C; int d, e, f; };
static_assert( Is_implicit_lifetime<F> );
class G { int a, b; };
static_assert( Is_implicit_lifetime<G> );
struct H { private: int a, b; };
static_assert( Is_implicit_lifetime<H> );
struct I { protected: int a, b; };
static_assert( Is_implicit_lifetime<I> );
struct J { int a, b; void foo (); };
static_assert( Is_implicit_lifetime<J> );
struct K { int a, b; virtual void foo (); };
static_assert( ! Is_implicit_lifetime<K> );
struct L : virtual public A { int d, e; };
static_assert( ! Is_implicit_lifetime<L> );
struct M : protected A { int d, e; };
static_assert( Is_implicit_lifetime<M> );
struct N : private A { int d, e; };
static_assert( Is_implicit_lifetime<N> );
struct O { O () = delete; int a, b, c; };
static_assert( Is_implicit_lifetime<O> );
struct P { P () = default; int a, b, c; };
static_assert( Is_implicit_lifetime<P> );
struct Q { Q (); Q (const Q &); int a, b, c; };
static_assert( ! Is_implicit_lifetime<Q> );
struct R { R (); R (const R &); R (R &&) = default; int a, b, c; };
static_assert( Is_implicit_lifetime<R> );
struct S { S (); ~S (); int a, b, c; };
static_assert( ! Is_implicit_lifetime<S> );
static_assert( Is_implicit_lifetime<S [3]> );
struct T { T (); ~T () = default; int a, b, c; };
static_assert( Is_implicit_lifetime<T> );
struct U { U (); U (const U &) = default; int a, b, c; };
static_assert( Is_implicit_lifetime<U> );
struct V { V () = default; V (const V &); int a, b, c; };
static_assert( Is_implicit_lifetime<V> );
struct AA { Q a; Q b; };
static_assert( Is_implicit_lifetime<AA> );
struct AB { Q a; Q b; ~AB () = default; };
static_assert( Is_implicit_lifetime<AB> );
struct AC { Q a; Q b; ~AC () {} };
static_assert( ! Is_implicit_lifetime<AC> );
struct AD : public Q {};
static_assert( Is_implicit_lifetime<AD> );
struct AE : public Q { ~AE () = default; };
static_assert( Is_implicit_lifetime<AE> );
struct AF : public Q { ~AF () {} };
static_assert( ! Is_implicit_lifetime<AF> );
