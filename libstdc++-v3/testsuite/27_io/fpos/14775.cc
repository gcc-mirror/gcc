// 2004-03-31  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2023 Free Software Foundation, Inc.
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

// 27.4.3 fpos

#include <fstream>
#include <testsuite_hooks.h>

#define TWO_GB 2147483648UL

// Basic test for LFS support.
void test01()
{
#if defined (_GLIBCXX_USE_LFS) && defined (_GLIBCXX_HAVE_LIMIT_FSIZE)
  using namespace std;

  typedef filebuf::pos_type 	pos_type;
  typedef filebuf::off_type 	off_type;

  __gnu_test::set_file_limit(TWO_GB + 200);

  basic_filebuf<char> fb;
  fb.open("tmp_14775", ios_base::out | ios_base::in | ios_base::trunc);

  pos_type ret = fb.pubseekoff(TWO_GB + 100, ios_base::beg);
  VERIFY( ret != pos_type(off_type(-1)) );

  fb.close();
#endif
}

int main()
{
  test01();
  return 0;
}

