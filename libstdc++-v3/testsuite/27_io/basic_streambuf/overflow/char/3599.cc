// 1999-10-11 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

// 27.5.2 template class basic_streambuf

#include <streambuf>
#include <ostream>
#include <testsuite_hooks.h>

// libstdc++/3599
class testbuf : public std::streambuf
{
public:
  typedef std::streambuf::traits_type traits_type;

  testbuf() : std::streambuf() { }
 
protected:
  int_type 
  overflow(int_type c __attribute__((unused)) = traits_type::eof()) 
  { return traits_type::not_eof(0); }
};

void
test07()
{
  bool test __attribute__((unused)) = true;
  testbuf ob;
  std::ostream out(&ob); 

  out << "gasp";
  VERIFY(out.good());

  out << std::endl;
  VERIFY(out.good());
}

int main() 
{
  test07();
  return 0;
}
