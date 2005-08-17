// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003 Free Software Foundation
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

// 22.2.1.5 - Template class codecvt [lib.locale.codecvt]

#include <locale>
#include <testsuite_hooks.h>

bool length_called = false;

class length_codecvt : public std::codecvt<char, char, std::mbstate_t>
{
  typedef std::codecvt<char, char, std::mbstate_t> c_codecvt;

public:
  // DR75: type of first argument of do_length is state_type&
  virtual int do_length(state_type& state, const extern_type* from,
                        const extern_type* end, std::size_t max) const
  {
    length_called = true;
    return c_codecvt::do_length(state, from, end, max);
  }
};

// Required instantiation, degenerate conversion.
// codecvt<char, char, mbstate_t>
//
// libstdc++/9224
void test02()
{
  using namespace std;
  typedef codecvt_base::result			result;
  typedef codecvt<char, char, mbstate_t> 	c_codecvt;

  bool test __attribute__((unused)) = true;
  const char* 		c_lit = "black pearl jasmine tea";
  int 			size = 25;

  locale 		loc (locale::classic(), new length_codecvt);
  c_codecvt::state_type state;
  const c_codecvt* 	cvt = &use_facet<c_codecvt>(loc); 

  length_called = false;
  cvt->length(state, c_lit, c_lit + size, 5);
  VERIFY( length_called );
}

int main ()
{
  test02();
  return 0;
}
