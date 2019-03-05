// 2003-02-06  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

// 22.2.1.5 - Template class codecvt [lib.locale.codecvt]

#include <locale>
#include <cstring>
#include <testsuite_hooks.h>

// Need to explicitly set the state(mbstate_t) to zero.
// How to do this is not specified by the ISO C99 standard, so we
// might need to add some operators to make the intuiative case
// work:
//   w_codecvt::state_type state00;
//   state00 = 0;  
// or, can use this explicit "C" initialization:
//   w_codecvt::state_type state01 = {0, 0};
// .. except Ulrich says: Use memset. Always use memset. Feel the force...
void
zero_state(std::mbstate_t& state)
{ std::memset(&state, 0, sizeof(std::mbstate_t)); }

bool length_called = false;

class length_codecvt : public std::codecvt<wchar_t, char, std::mbstate_t>
{
  typedef std::codecvt<wchar_t, char, std::mbstate_t> w_codecvt;

public:
  // DR75: type of first argument of do_length is state_type&
  virtual int do_length(state_type& state, const extern_type* from,
                        const extern_type* end, std::size_t max) const
  {
    length_called = true;
    return w_codecvt::do_length(state, from, end, max);
  }
};

// Required instantiation
// codecvt<wchar_t, char, mbstate_t>
//
// libstdc++/9224
void test06()
{
  using namespace std;
  typedef codecvt<wchar_t, char, mbstate_t> 	w_codecvt;
  typedef char					ext_type;

  const ext_type* 	e_lit = "black pearl jasmine tea";
  int 			size = strlen(e_lit);

  locale 		loc;
  loc = locale(loc, new length_codecvt);
  const w_codecvt* 	cvt = &use_facet<w_codecvt>(loc); 

  w_codecvt::state_type state04;
  zero_state(state04);
  length_called = false;
  cvt->length(state04, e_lit, e_lit + size, 5);
  VERIFY( length_called );
}

int main ()
{
  test06();
  return 0;
}
