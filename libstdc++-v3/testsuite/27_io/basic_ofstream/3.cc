// 1999-01-17 bkoz test functionality of basic_filebuf for char_type == char

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
// Free Software Foundation, Inc.
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

// 27.8.1.1 - Template class basic_filebuf 
// NB: This file is for testing basic_filebuf with NO OTHER INCLUDES.

#include <fstream>
#include <testsuite_hooks.h>

// { dg-do compile }

// libstdc++/2020
// should be able to use custom char_type, custom traits type
class gnu_char_type
{
  unsigned long character;
public:
  // operator ==
  bool
  operator==(const gnu_char_type& __lhs) 
  { return character == __lhs.character; }

  // operator <
  bool
  operator<(const gnu_char_type& __lhs) 
  { return character < __lhs.character; }

  // default ctor
  gnu_char_type() { }

  // to_char_type
  gnu_char_type(const unsigned long& __l) : character(__l) { } 

  // to_int_type
  operator unsigned long() const { return character; }
};

// char_traits specialization
struct gnu_char_traits
{
  typedef gnu_char_type	char_type;
  typedef long  		int_type;
  typedef std::streamoff 	off_type;
  typedef long   		state_type;
  typedef std::fpos<state_type>	pos_type;
  
  static void 
  assign(char_type& __c1, const char_type& __c2) { }
  
  static bool 
  eq(const char_type& __c1, const char_type& __c2) { return true; }
  
  static bool 
  lt(const char_type& __c1, const char_type& __c2) { return true; }
  
  static int 
  compare(const char_type* __s1, const char_type* __s2, size_t __n)
  { return 0; }
  
  static size_t
  length(const char_type* __s) { return 0; }
  
  static const char_type* 
  find(const char_type* __s, size_t __n, const char_type& __a)
  { return __s; }
  
  static char_type* 
  move(char_type* __s1, const char_type* __s2, size_t __n)
  { return __s1; }
  
  static char_type* 
  copy(char_type* __s1, const char_type* __s2, size_t __n)
  { return __s1; }
  
  static char_type* 
  assign(char_type* __s, size_t __n, char_type __a)
  { return __s; }
  
  static char_type 
  to_char_type(const int_type& __c)
  { return char_type(); }
  
  static int_type 
  to_int_type(const char_type& __c)
  { return int_type(); }
  
  static bool 
  eq_int_type(const int_type& __c1, const int_type& __c2)
  { return true; }
  
  static int_type 
  eof()
  { return int_type(); }
  
  static int_type 
  not_eof(const int_type& __c)
  { return int_type(); }
};

void test07()
{
  bool test __attribute__((unused)) = true;
  typedef std::basic_ofstream<gnu_char_type, gnu_char_traits> gnu_ofstr;

  try
    { 
      gnu_ofstr obj;
    }
  catch(std::exception& obj)
    { 
      test = false; 
      VERIFY( test );
    }
}

int main() 
{
  test07();
  return 0;
}



// more surf!!!
