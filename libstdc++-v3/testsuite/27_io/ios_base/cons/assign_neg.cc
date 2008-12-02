// { dg-do compile }

// Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 Free Software
// Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <ios>

// Library defect report
//50.  Copy constructor and assignment operator of ios_base
class test_base : public std::ios_base { };

void test01()
{
  // assign
  test_base io1;
  test_base io2;
  io1 = io2;
}
// { dg-error "synthesized" "" { target *-*-* } 42 } 
// { dg-error "within this context" "" { target *-*-* } 35 } 
// { dg-error "is private" "" { target *-*-* } 785 } 
// { dg-error "operator=" "" { target *-*-* } 0 } 
