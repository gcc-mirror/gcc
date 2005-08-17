// Copyright (C) 2003 Free Software Foundation, Inc.
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <istream>
#include <ostream>
#include <streambuf>
#include <sstream>
#include <testsuite_hooks.h>

using namespace std;

void test2()
{
  bool test __attribute__((unused)) = true;
  istringstream stream;
  stream >> static_cast<streambuf*>(NULL);
  VERIFY(stream.rdstate() & ios_base::failbit);
}

void test4()
{
  bool test __attribute__((unused)) = true;
  istringstream stream;
  stream.exceptions(ios_base::failbit);

  try
    {
      stream >> static_cast<streambuf*>(NULL);
      VERIFY(false);
    }
  catch (ios_base::failure&)
    {
    }

  VERIFY(stream.rdstate() & ios_base::failbit);
}

// libstdc++/9371
int main()
{
  test2();
  test4();
  return 0;
}
