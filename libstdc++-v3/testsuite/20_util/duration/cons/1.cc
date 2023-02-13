// { dg-do run { target c++11 } }

// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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
  type_emulator()
  : i(T(0)) { }
  
  type_emulator(T j)
  : i(j) { }
  
  type_emulator(const type_emulator& e)
  : i(e.i) { }

  type_emulator&
  operator*=(type_emulator a)
  {
    i *= a.i;
    return *this;
  }
  
  type_emulator&
  operator+=(type_emulator a)
  {
    i += a.i;
    return *this;
  }
    
  operator T ()
  { return i; }
  
  T i;
};

template<typename T>
bool
operator==(type_emulator<T> a, type_emulator<T> b)
{ return a.i == b.i; }

template<typename T>
bool
operator<(type_emulator<T> a, type_emulator<T> b)
{ return a.i < b.i; }

template<typename T>
type_emulator<T>
operator+(type_emulator<T> a, type_emulator<T> b)
{ return a += b; }

template<typename T>
type_emulator<T>
operator*(type_emulator<T> a, type_emulator<T> b)
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
  using std::chrono::duration;
  
  int r = 3;
  duration<int> d1(r);
  VERIFY(d1.count() == static_cast<duration<int>::rep>(r));
  
  double s = 8.0;
  duration<double> d2(s);
  VERIFY(d2.count() == static_cast<duration<double>::rep>(s));
    
  int_emulator ie(3);
  duration<int_emulator> d3(ie);
  VERIFY(d3.count() == static_cast<duration<int_emulator>::rep>(ie));
  
  dbl_emulator de(4.0);
  duration<dbl_emulator> d4(de);
  VERIFY(d4.count() == static_cast<duration<dbl_emulator>::rep>(de));
}

int
main()
{
  test01();
  return 0;
}
