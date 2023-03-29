// 1999-01-17 bkoz test functionality of basic_filebuf for char_type == char

// Copyright (C) 1997-2023 Free Software Foundation, Inc.
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

// 27.8.1.1 - Template class basic_filebuf 
// NB: This file is for testing basic_filebuf with NO OTHER INCLUDES.

#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_character.h>

// libstdc++/2020
// should be able to use custom char_type, custom traits type
void test07()
{
  typedef std::basic_stringstream<__gnu_test::pod_ushort> gnu_sstr;

  try
    { 
      gnu_sstr obj;
    }
  catch(std::exception& obj)
    { 
      VERIFY( false );
    }
}

int main() 
{
  test07();
  return 0;
}



// more surf!!!
