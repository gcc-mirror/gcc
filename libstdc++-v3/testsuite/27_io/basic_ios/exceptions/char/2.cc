// Copyright (C) 2003-2020 Free Software Foundation, Inc.
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


// 27.4.4.2 basic_ios member functions

#include <sstream>
#include <testsuite_hooks.h>

int main()
{
  using namespace std;
  ostringstream stream;
  
  try 
    {
      stream.setstate(ios_base::failbit);
      stream.exceptions(ios_base::failbit);
      VERIFY( false );
    } 
  catch (...) 
    {
      // Don't clear.
    }

  try
    {
      // Calls clear(rdstate()), which throws in this case.
      stream.setstate(ios_base::goodbit);
      VERIFY( false );
    }
  catch (...) 
    { }

  return 0;
}
