// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-do compile }

// Define macros for some common variables names that we must not use for
// naming variables, parameters etc. in the library.
#define tmp (
#define A (
#define B (
#define C (
#define D (
#define E (
#define F (
#define G (
#define H (
#define I (
#define J (
#define K (
#define L (
#define M (
#define N (
#define O (
#define P (
#define Q (
#define R (
#define S (
#define T (
#define U (
#define V (
#define W (
#define X (
#define Y (
#define Z (
#if __cplusplus >= 201103L
// <random> defines member functions called a() and b()
#else
#define a (
#define b (
#endif
// <queue> and <stack> defined data members called c
#define d (
#define e (
#define f (
#define g (
#if __cplusplus >= 201402L
// <chrono> defines operator ""h in C++14
// <complex> defines operator ""i in C++14
#else
#define h (
#define i (
#endif
#define j (
#if __cplusplus >= 201103L
// <random> defines member functions called k()
#else
#define k (
#endif
#define l (
#if __cplusplus >= 201103L
// <random> defines member functions called m() and n()
#else
#define m (
#define n (
#endif
#define o (
#if __cplusplus >= 201103L
// <random> defines member functions called p()
#else
#define p (
#endif
#define q (
#define r (
#if __cplusplus >= 201103L
// <random> defines member functions called s() and t()
// <chrono> and <string> define operator ""s in C++14
#else
#define s (
#define t (
#endif
#define u (
#define v (
#define w (
#define x (
#define y (
#define z (

#ifdef _AIX
// See https://gcc.gnu.org/ml/libstdc++/2017-03/msg00015.html
#undef f
#undef r
#undef x
#undef y
#endif

#ifdef __hpux__
#undef d
#undef r
#endif

#include <bits/stdc++.h>
