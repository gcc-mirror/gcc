// Copyright (C) 2003, 2004
// Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// libstdc++/10093
template<typename T>
void test_failbit()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  locale loc(locale::classic(), new __gnu_test::fail_num_put);
  ostringstream stream("jaylib - champion sound");
  stream.imbue(loc);

  stream.exceptions(ios_base::failbit);
  
  try
    {
      T i = T();
      stream << i;
    }
  catch (const ios_base::failure&)
    { VERIFY( false ); }
  catch(...)
    { VERIFY( false ); }

    // stream should set badbit.
    VERIFY( stream.bad() );
    VERIFY( (stream.rdstate() & ios_base::failbit) == 0 );
    VERIFY( !stream.eof() );
}

int main()
{
  test_failbit<bool>();
  test_failbit<short>();
  test_failbit<unsigned short>();
  test_failbit<int>();
  test_failbit<unsigned int>();
  test_failbit<long>();
  test_failbit<unsigned long>();

  test_failbit<float>();
  test_failbit<double>();

  return 0;
}
