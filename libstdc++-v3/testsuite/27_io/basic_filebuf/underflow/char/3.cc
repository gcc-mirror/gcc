// Copyright (C) 2007-2025 Free Software Foundation, Inc.
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
#include <cctype>
#include <locale>
#include <testsuite_hooks.h>

class Mycvtcc
: public std::codecvt<char, char, std::mbstate_t>
{
protected:
  virtual result
  do_in(state_type&,
	const extern_type* from, const extern_type* from_end,
	const extern_type*& from_next,
	intern_type* to, intern_type* to_limit,
	intern_type*& to_next) const
  {
    from_next = from, to_next = to;
    
    if (from_next == from_end || to_next == to_limit)
      return partial;
    
    if (std::islower(*from_next))
      *to_next = std::toupper(*from_next);
    else
      *to_next = *from_next;
    ++from_next, ++to_next;
    return ok;
  }

  virtual bool
  do_always_noconv() const throw()
  { return false; }
};

// See Novell Bug 255122
void test01()
{
  using namespace std;

  const char* name = "tmp_underflow_3.tst";
  filebuf fbuf, fbufx;

  fbuf.open(name, ios_base::out | ios_base::trunc);
  VERIFY( fbuf.sputc('a') == 'a' );
  VERIFY( fbuf.sputc('b') == 'b' );
  VERIFY( fbuf.sputc('\n') == '\n' );
  fbuf.close();

  fbufx.pubimbue(locale(locale::classic(), new Mycvtcc));
  fbufx.open(name, ios_base::in);
  VERIFY( fbufx.sbumpc() == 'A' );
  VERIFY( fbufx.sbumpc() == 'B' );
  VERIFY( fbufx.sbumpc() == '\n' );
  VERIFY( fbufx.sbumpc() == filebuf::traits_type::eof() );
  fbufx.close();
}

int main()
{
  test01();
  return 0;
}
