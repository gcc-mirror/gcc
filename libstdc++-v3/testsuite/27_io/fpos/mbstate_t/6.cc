// { dg-do compile }

// 2006-06-21  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2014 Free Software Foundation, Inc.
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


// 27.4.3 template class fpos

#include <ios>

class
octave_int
{
  template<class U>
    octave_int(U i);
};

bool
operator==(const octave_int& lhs, const octave_int& rhs);

bool
Fsave(void)
{
  return std::streampos(0) == std::streampos(0);
}
