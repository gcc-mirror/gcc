// Copyright (C) 2003, 2004, 2005 Free Software Foundation
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

#include <testsuite_hooks.h>
#ifdef _GLIBCXX_USE___ENC_TRAITS
#include <ext/enc_filebuf.h>
#endif

void test01()
{
#ifdef _GLIBCXX_USE___ENC_TRAITS
  using namespace std;
  typedef __enc_traits state_type;
  typedef char char_type;
  typedef __gnu_cxx::enc_filebuf<char_type> filebuf_type;
  typedef codecvt<char_type, char, state_type> enc_codecvt;

  bool test __attribute__((unused)) = true;
  ios_base::openmode mode = ios_base::in | ios_base::out | ios_base::trunc;
  try
    {
      state_type st;
      filebuf_type fbuf(st);
      locale loc(locale::classic(), new enc_codecvt);
      fbuf.pubimbue(loc);
      fbuf.open("tmp_13189c", mode);
      fbuf.sputc('a');
      fbuf.pubseekoff(0, ios_base::beg);
      fbuf.sgetc();
      fbuf.close();
    }
  catch(...)
    {
    }
#endif
}

int main() 
{ 
  test01();
  return 0; 
}
