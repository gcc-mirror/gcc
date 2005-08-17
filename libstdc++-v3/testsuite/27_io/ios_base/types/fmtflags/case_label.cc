// { dg-do compile }
// { dg-options "-Wall" { target *-*-* } }
// -*- C++ -*-
 
// Copyright (C) 2004 Free Software Foundation, Inc.
 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2, or (at
// your option) any later version.
 
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
 
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
// MA 02110-1301, USA.
 
// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.
 
// Benjamin Kosnik  <bkoz@redhat.com>

#include <ios>

// PR libstdc++/17922
// -Wall
typedef std::ios_base::fmtflags bitmask_type;

void
case_labels(bitmask_type b)
{
  switch (b) 
    {
    case std::ios_base::boolalpha:
      break;
    case std::ios_base::dec:
      break;
    case std::ios_base::fixed:
      break;
    case std::ios_base::hex:
      break;
    case std::ios_base::internal:
      break;
    case std::ios_base::left:
      break;
    case std::ios_base::oct:
      break;
    case std::ios_base::right:
      break;
    case std::ios_base::scientific:
      break;
    case std::ios_base::showbase:
      break;
    case std::ios_base::showpoint:
      break;
    case std::ios_base::showpos:
      break;
    case std::ios_base::skipws:
      break;
    case std::ios_base::unitbuf:
      break;
    case std::ios_base::uppercase:
      break;
    case std::ios_base::adjustfield:
      break;
    case std::ios_base::basefield:
      break;
    case std::ios_base::floatfield:
      break;
    case std::_S_ios_fmtflags_end:
      break;
    }
}
