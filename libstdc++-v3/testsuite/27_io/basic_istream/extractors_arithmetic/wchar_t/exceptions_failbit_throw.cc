// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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


#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// libstdc++/10093
template<typename T>
void test_failbit()
{
  using namespace std;

  locale loc(locale::classic(), new __gnu_test::fail_num_get_wchar_t);
  wistringstream stream(L"jaylib - champion sound");
  stream.imbue(loc);

  stream.exceptions(ios_base::failbit);
  
  try
    {
      T i;
      stream >> i;
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

  test_failbit<void*>();

  return 0;
}
