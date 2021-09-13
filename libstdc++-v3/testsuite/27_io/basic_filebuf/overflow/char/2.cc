// 2003-06-10  Paolo Carlini  <pcarlini@unitus.it>

// Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

const char name_08[] = "filebuf_virtuals-8.txt"; // empty file, need to create

class OverBuf : public std::filebuf
{
public:
  int_type pub_overflow(int_type c = traits_type::eof())
  { return std::filebuf::overflow(c); }
};

// According to 27.5.2.4.5 filebuf::overflow() returns not_eof(eof()).
void test01()
{
  using namespace std;
  typedef OverBuf::traits_type  traits_type;

  OverBuf fb;
  fb.open(name_08, ios_base::out | ios_base::trunc);
  
  VERIFY( fb.pub_overflow() == traits_type::not_eof(traits_type::eof()) );
  fb.close();
}

int main() 
{
  test01();
  return 0;
}
