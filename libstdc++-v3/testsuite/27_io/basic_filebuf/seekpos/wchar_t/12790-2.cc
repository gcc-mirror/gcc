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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.8.1.4 Overridden virtual functions

#include <locale>
#include <fstream>
#include <testsuite_hooks.h>

class Cvt : public std::codecvt<wchar_t, char, std::mbstate_t>
{
public:
  mutable bool unshift_called;

  Cvt()
  : unshift_called(false)
  { }

protected:
  bool
  do_always_noconv() const throw()
  { return false; }

  int
  do_encoding() const throw()
  { return -1; }

  std::codecvt_base::result
  do_unshift(std::mbstate_t&, char* to, char*, char*& to_next) const
  {
    unshift_called = true;
    to_next = to;
    return std::codecvt_base::ok;
  }
};

// libstdc++/12790
// basic_filebuf::seekpos() should call codecvt::unshift()
void test01()
{
  using namespace std;

  bool test __attribute__((unused)) = true;
  const char* name = "tmp_seekpos_12790";

  Cvt* cvt = new Cvt;
  locale loc(locale::classic(), cvt);

  wfilebuf fb;
  fb.pubsetbuf(0, 0);
  fb.pubimbue(loc);

  fb.open(name, ios_base::out);
  streampos pos = fb.pubseekoff(0, ios_base::beg);
  fb.sputc(L'a');

  VERIFY( !cvt->unshift_called );
  fb.pubseekpos(pos);
  VERIFY( cvt->unshift_called );
}

int main()
{
  test01();
  return 0;
}
