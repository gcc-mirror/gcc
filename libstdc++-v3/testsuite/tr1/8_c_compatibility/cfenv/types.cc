// { dg-do compile }

// 2006-01-26  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 8.6 Header <cfenv>

#include <tr1/cfenv>

void test01()
{
#if _GLIBCXX_USE_C99_FENV_TR1

  // Check for required typedefs
  typedef std::tr1::fenv_t    my_fenv_t;
  typedef std::tr1::fexcept_t my_fexcept_t;
  
#endif
}
