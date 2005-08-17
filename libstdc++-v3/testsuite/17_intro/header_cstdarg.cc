// 2001-02-06  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2003 Free Software Foundation, Inc.
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

// 17.4.1.2 Headers, cstdarg

#include <cstdarg>

namespace gnu
{
  void test01()
  { 
    std::va_list __attribute__((unused)) list;
#ifndef va_arg
    #error "va_arg_must_be_a_macro"
#endif
#ifndef va_end
    #error "va_end_must_be_a_macro"
#endif
#ifndef va_start
    #error "va_start_must_be_a_macro"
#endif
  }
}
  
int main()
{
  gnu::test01();
  return 0;
}
