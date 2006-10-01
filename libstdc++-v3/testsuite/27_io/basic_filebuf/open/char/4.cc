// 2006-10-01  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006 Free Software Foundation, Inc.
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

#include <fstream>
#include <testsuite_hooks.h>

// As an extension to Table 92, consistently with the C standards, we also
// allow in|out|app and in|out|app|binary.
void test01()
{
  bool test __attribute__((unused)) = true;
  const char* name = "tmp_file4";

  std::fstream scratch_file;

  scratch_file.open(name, std::ios_base::in | std::ios_base::out
		    | std::ios_base::app);
  VERIFY( scratch_file );
  VERIFY( scratch_file.is_open() );
  scratch_file.close();

  scratch_file.open(name, std::ios_base::in | std::ios_base::out
		    | std::ios_base::app | std::ios_base::binary);
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
