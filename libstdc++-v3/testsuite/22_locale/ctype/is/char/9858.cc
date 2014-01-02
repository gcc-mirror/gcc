// Copyright (C) 2003-2014 Free Software Foundation, Inc.
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


// 22.2.1.3 - ctype specializations [lib.facet.ctype.special]

#include <locale>
#include <testsuite_hooks.h>

int called;

class Derived : public std::ctype<char>
{
public:
  bool 
  do_is(mask, char_type) const { return true; }

  const char_type* 
  do_is(const char_type*, const char_type* hi, mask*) const 
  { return hi; }

  const char_type* 
  do_scan_is(mask, const char_type*, const char_type* hi) const 
  { return hi; }

  const char_type* 
  do_scan_not(mask, const char_type*, const char_type* hi) const 
  { return hi; }
};

class Derived2 : public Derived
{
public:
  bool 
  do_is(mask, char_type) const { called = 1; return true; }

  const char_type* 
  do_is(const char_type*, const char_type* hi, mask*) const 
  { called = 5; return hi; }

  const char_type* 
  do_scan_is(mask, const char_type*, const char_type* hi) const 
  { called = 10; return hi; }

  const char_type* 
  do_scan_not(mask, const char_type*, const char_type* hi) const 
  { called = 15; return hi; }
};

int main()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  Derived2 d2;
  const Derived& dr = d2;

  const char* lit = "jaylib champion sound";
  ctype_base::mask m00 = static_cast<std::ctype_base::mask>(0);
  ctype_base::mask vec[5];
  for (std::size_t i = 0; i < 5; ++i)
    vec[i] = m00;
 
  called = 0;
  dr.do_is(ctype_base::space, 'a');
  VERIFY( called !=  1);

  called = 0;
  dr.do_is(lit, lit + 5, vec);
  VERIFY( called !=  5);

  called = 0;
  dr.do_scan_is(ctype_base::space, lit, lit + 5);
  VERIFY( called !=  10);

  called = 0;
  dr.do_scan_not(ctype_base::space, lit, lit + 5);
  VERIFY( called !=  15);
  
  return 0;
}
