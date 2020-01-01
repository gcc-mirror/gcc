// Copyright (C) 2004-2020 Free Software Foundation, Inc.
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

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_io.h>

// libstdc++/9561
template<typename T>
void test_badbit()
{
  using namespace std;

  locale loc(locale::classic(), new __gnu_test::fail_num_get_wchar_t);
  wistringstream stream(L"jaylib - champion sound");
  stream.imbue(loc);

  stream.exceptions(ios_base::badbit);
  VERIFY( stream.rdstate() == ios_base::goodbit );

  try 
    {
      T i;
      stream >> i;
      VERIFY( false );
    }
  catch (const __gnu_test::facet_error&) 
    {
      // stream should set badbit and rethrow facet_error.
      VERIFY( stream.bad() );
      VERIFY( (stream.rdstate() & ios_base::failbit) == 0 );
      VERIFY( !stream.eof() );
    }
  catch (...)
    {
      VERIFY(false);
    }
}


int main()
{
  test_badbit<bool>();
  test_badbit<short>();
  test_badbit<unsigned short>();
  test_badbit<int>();
  test_badbit<unsigned int>();
  test_badbit<long>();
  test_badbit<unsigned long>();

  test_badbit<float>();
  test_badbit<double>();

  test_badbit<void*>();

  return 0;
}
