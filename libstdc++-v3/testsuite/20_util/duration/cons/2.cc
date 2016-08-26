// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// 20.8.3 Class template duration [time.duration]

#include <chrono>
#include <type_traits>
#include <testsuite_hooks.h>

template<typename T>
struct type_emulator
{
  type_emulator() : i(T(0)) { }
  type_emulator(T j) : i(j) { }
  type_emulator(const type_emulator& e) : i(e.i) { }

  type_emulator& operator*=(type_emulator a)
  { i *= a.i; return *this; }
  
  type_emulator& operator+=(type_emulator a)
  { i += a.i; return *this; }
    
  operator T () { return i; }
  T i;
};

template<typename T>
bool operator==(type_emulator<T> a, type_emulator<T> b)
{ return a.i == b.i; }

template<typename T>
bool operator<(type_emulator<T> a, type_emulator<T> b)
{ return a.i < b.i; }

template<typename T>
type_emulator<T> operator+(type_emulator<T> a, type_emulator<T> b)
{ return a += b; }

template<typename T>
type_emulator<T> operator*(type_emulator<T> a, type_emulator<T> b)
{ return a *= b; }

namespace std
{
  template<typename T, typename U>
  struct common_type<type_emulator<T>, U>
  { typedef typename common_type<T,U>::type type; };
  
  template<typename T, typename U>
  struct common_type<U, type_emulator<T>>
  { typedef typename common_type<U,T>::type type; };
  
  template<typename T, typename U>
  struct common_type<type_emulator<T>, type_emulator<U>>
  { typedef typename common_type<T,U>::type type; };
  
  namespace chrono
  {    
    template<typename T>
    struct treat_as_floating_point<type_emulator<T>>
    : is_floating_point<T>
    { };
  }
}

typedef type_emulator<int> int_emulator;
typedef type_emulator<double> dbl_emulator;

// 20.8.3.1 duration constructors [time.duration.cons]
void
test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std::chrono;
  
  duration<int> d0(3);
  duration<int> d0_copy(d0);
  VERIFY(d0_copy.count() == d0.count());
  
  duration<int, std::milli> d1(5);
  duration<int, std::micro> d1_copy(d1);
  VERIFY(d1.count() * 1000 == d1_copy.count());
  
  duration<double, std::micro> d2(8.0);
  duration<double, std::milli> d2_copy(d2);
  VERIFY(d2.count() == d2_copy.count() * 1000.0);
  
  duration<int_emulator, std::milli> d3(5);
  duration<int_emulator, std::micro> d3_copy(d3);
  VERIFY(d3.count() * 1000 == d3_copy.count());
  
  duration<dbl_emulator, std::micro> d4(5.0);
  duration<dbl_emulator, std::milli> d4_copy(d4);
  VERIFY(d4.count() == d4_copy.count() * dbl_emulator(1000.0));
}

int
main()
{
  test01();
  return 0;
}
