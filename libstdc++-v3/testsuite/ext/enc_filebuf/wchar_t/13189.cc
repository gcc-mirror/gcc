// { dg-require-iconv "ISO-8859-1" }

// Copyright (C) 2003-2020 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include <ext/enc_filebuf.h>

void test01()
{
  using namespace std;
  typedef wchar_t char_type;
  typedef __gnu_cxx::enc_filebuf<char_type> filebuf_type;
  typedef filebuf_type::state_type state_type;
  typedef codecvt<char_type, char, state_type> enc_codecvt;

  ios_base::openmode mode = ios_base::in | ios_base::out | ios_base::trunc;
  try
    {
      state_type st;
      filebuf_type fbuf(st);
      locale loc(locale::classic(), new enc_codecvt);
      fbuf.pubimbue(loc);
      fbuf.open("tmp_13189w", mode);
      fbuf.sputc(L'a');
      fbuf.pubseekoff(0, ios_base::beg);
      fbuf.sgetc();
      fbuf.close();
    }
  catch(...)
    {
    }
}

int main() 
{ 
  test01();
  return 0; 
}
