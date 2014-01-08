// { dg-do compile }

// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

#include <functional>

namespace std {
  //  lib.base, base:
  template <class Arg, class Result> struct unary_function;
  template <class Arg1, class Arg2, class Result> struct binary_function;

  //  lib.arithmetic.operations, arithmetic operations:
  template <class T> struct plus;
  template <class T> struct minus;
  template <class T> struct multiplies;
  template <class T> struct divides;
  template <class T> struct modulus;
  template <class T> struct negate;

  //  lib.comparisons, comparisons:
  template <class T> struct equal_to;
  template <class T> struct not_equal_to;
  template <class T> struct greater;
  template <class T> struct less;
  template <class T> struct greater_equal;
  template <class T> struct less_equal;

  //  lib.logical.operations, logical operations:
  template <class T> struct logical_and;
  template <class T> struct logical_or;
  template <class T> struct logical_not;

  //  lib.negators, negators:
  template <class Predicate> struct unary_negate;
  template <class Predicate>
  unary_negate<Predicate>  not1(const Predicate&);
  template <class Predicate> struct binary_negate;
  template <class Predicate>
  binary_negate<Predicate> not2(const Predicate&);

  //  lib.binders, binders:
  template <class Operation>  class binder1st;
  template <class Operation, class T>
  binder1st<Operation> bind1st(const Operation&, const T&);
  template <class Operation> class binder2nd;
  template <class Operation, class T>
  binder2nd<Operation> bind2nd(const Operation&, const T&);

  //  lib.function.pointer.adaptors, adaptors:
  template <class Arg, class Result> class pointer_to_unary_function;
  template <class Arg, class Result>
  pointer_to_unary_function<Arg,Result> ptr_fun(Result (*)(Arg));
  template <class Arg1, class Arg2, class Result>
  class pointer_to_binary_function;
  template <class Arg1, class Arg2, class Result>
  pointer_to_binary_function<Arg1,Arg2,Result>
  ptr_fun(Result (*)(Arg1,Arg2));

  //  lib.member.pointer.adaptors, adaptors:
  template<class S, class T> class mem_fun_t;
  template<class S, class T, class A> class mem_fun1_t;
  template<class S, class T>
  mem_fun_t<S,T> mem_fun(S (T::*f)());
  template<class S, class T, class A>
  mem_fun1_t<S,T,A> mem_fun(S (T::*f)(A));
  template<class S, class T> class mem_fun_ref_t;
  template<class S, class T, class A> class mem_fun1_ref_t;
  template<class S, class T>
  mem_fun_ref_t<S,T> mem_fun_ref(S (T::*f)());
  template<class S, class T, class A>
  mem_fun1_ref_t<S,T,A> mem_fun_ref(S (T::*f)(A));

  template <class S, class T> class const_mem_fun_t;
  template <class S, class T, class A> class const_mem_fun1_t;
  template <class S, class T>
  const_mem_fun_t<S,T> mem_fun(S (T::*f)() const);
  template <class S, class T, class A>
  const_mem_fun1_t<S,T,A> mem_fun(S (T::*f)(A) const);
  template <class S, class T> class const_mem_fun_ref_t;
  template <class S, class T, class A> class const_mem_fun1_ref_t;
  template <class S, class T>
  const_mem_fun_ref_t<S,T> mem_fun_ref(S (T::*f)() const);
  template <class S, class T, class A>
  const_mem_fun1_ref_t<S,T,A> mem_fun_ref(S (T::*f)(A) const);
}
