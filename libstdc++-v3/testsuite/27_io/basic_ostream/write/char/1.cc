// 2003-09-22  Petur Runolfsson  <peturr02@ru.is>

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

// 27.6.2.6 Unformatted output functions
//
// _GLIBCXX_RESOLVE_LIB_DEFECTS
// DR 60. What is a formatted input function?
// basic_ostream::write(const char_type*, streamsize) is an unformatted
// output function.
// DR 63. Exception-handling policy for unformatted output.
// Unformatted output functions should catch exceptions thrown
// from streambuf members.

#include <ostream>
#include <streambuf>
#include <testsuite_hooks.h>

class Buf : public std::streambuf
{
protected:
  virtual int_type
  overflow(int_type = traits_type::eof())
  { throw 0; }
};

void test01()
{
  Buf buf;
  std::ostream os(&buf);

  VERIFY( os.good() );

  os.write("a", 1);

  VERIFY( os.rdstate() == std::ios_base::badbit );

  os.clear();
  os.exceptions(std::ios_base::badbit);

  try
    {
      os.write("b", 1);
      VERIFY( false );
    }
  catch (int)
    {
      VERIFY( os.rdstate() == std::ios_base::badbit );
    }
}

int main()
{
  test01();
  return 0;
}
