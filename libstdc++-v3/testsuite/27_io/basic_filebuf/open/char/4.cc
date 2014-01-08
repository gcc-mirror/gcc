// 2006-10-01  Paolo Carlini  <pcarlini@suse.de>

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

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

// DR 596.
void test01()
{
  bool test __attribute__((unused)) = true;
  const char* name = "tmp_file4";

  std::fstream scratch_file;

  scratch_file.open(name, std::ios_base::app);
  VERIFY( scratch_file );
  VERIFY( scratch_file.is_open() );
  scratch_file.close();

  scratch_file.open(name, std::ios_base::in | std::ios_base::out
		    | std::ios_base::app);
  VERIFY( scratch_file );
  VERIFY( scratch_file.is_open() );
  scratch_file.close();

  scratch_file.open(name, std::ios_base::in | std::ios_base::app);
  VERIFY( scratch_file );
  VERIFY( scratch_file.is_open() );
  scratch_file.close();

  scratch_file.open(name, std::ios_base::app | std::ios_base::binary);
  VERIFY( scratch_file );
  VERIFY( scratch_file.is_open() );
  scratch_file.close();

  scratch_file.open(name, std::ios_base::in | std::ios_base::out
		    | std::ios_base::app | std::ios_base::binary);
  VERIFY( scratch_file );
  VERIFY( scratch_file.is_open() );  
  scratch_file.close();

  scratch_file.open(name, std::ios_base::in | std::ios_base::app
		    | std::ios_base::binary);
  VERIFY( scratch_file );
  VERIFY( scratch_file.is_open() );  
  scratch_file.close();
}

int
main()
{
  test01();
  return 0;
}
