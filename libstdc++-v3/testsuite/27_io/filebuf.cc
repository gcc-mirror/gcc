// 1999-01-17 bkoz test functionality of basic_filebuf for char_type == char

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002 
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// NB: this test assumes that _M_buf_size == 40, and not the usual
// buffer_size length of BUFSIZ (8192), so that overflow/underflow can be
// simulated a bit more readily.

#include <fstream>
#include <testsuite_hooks.h>

// { dg-do compile }

// test05
// libstdc++/1886
// should be able to instantiate basic_filebuf for non-standard types.
template class std::basic_filebuf<short, std::char_traits<short> >;

int main() 
{
  return 0;
}



// more surf!!!









