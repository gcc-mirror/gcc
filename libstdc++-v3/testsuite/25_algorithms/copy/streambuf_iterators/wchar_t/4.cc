// 2006-03-20  Paolo Carlini  <pcarlini@suse.de>

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

#include <iterator>
#include <fstream>
#include <algorithm>
#include <testsuite_hooks.h>

// In the occasion of libstdc++/25482
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;
  
  typedef istreambuf_iterator<wchar_t> in_iterator_type;

  wifstream fbuf_ref("istream_unformatted-1.txt"),
            fbuf("istream_unformatted-1.txt");

  wchar_t buffer_ref[16500],
          buffer[16500];

  fbuf_ref.read(buffer_ref, 16500);

  in_iterator_type beg(fbuf);
  in_iterator_type end;
  copy(beg, end, buffer);

  VERIFY( fbuf_ref.good() );
  VERIFY( fbuf.good() );

  VERIFY( !wmemcmp(buffer, buffer_ref, 16500) );
}

int main()
{
  test01();
  return 0;
}
