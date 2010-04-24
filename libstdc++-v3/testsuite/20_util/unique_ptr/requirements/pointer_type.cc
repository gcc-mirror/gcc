// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation
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

// 20.6.11 Template class unique_ptr [unique.ptr.single]

#include <memory>
#include <testsuite_hooks.h>

struct A
{
  void operator()(void* p) const { }
};

struct B
{
  typedef char* pointer;
  void operator()(pointer p) const { }
};

int main()
{
  typedef std::unique_ptr<int> 	   up;
  typedef std::unique_ptr<int, A>  upA;
  typedef std::unique_ptr<int, B>  upB;
  typedef std::unique_ptr<int, A&> upAr;
  typedef std::unique_ptr<int, B&> upBr;

  static_assert( std::is_same< up::pointer, int*>::value, "" );
  static_assert( std::is_same< upA::pointer, int*>::value, "" );
  static_assert( std::is_same< upB::pointer, char*>::value, "" );
  static_assert( std::is_same< upAr::pointer, int*>::value, "" );
  static_assert( std::is_same< upBr::pointer, char*>::value, "" );
}
