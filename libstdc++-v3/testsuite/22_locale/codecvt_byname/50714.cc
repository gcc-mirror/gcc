// { dg-require-namedlocale "de_DE.ISO8859-15" }

// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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
#include <ext/pod_char_traits.h>
#include <testsuite_hooks.h>

#define mychar __gnu_cxx::character<unsigned short, int>

namespace std
{
  template<> codecvt<mychar, char, mbstate_t>::~codecvt()
  { }

  template<>
  codecvt<mychar, char, mbstate_t>::result
  codecvt<mychar, char, mbstate_t>::
  do_out(state_type&, const intern_type*, const intern_type*,
	 const intern_type*&, extern_type*, extern_type*,
	 extern_type*&) const
  { return codecvt_base::ok; }

  template<>
  codecvt<mychar, char, mbstate_t>::result
  codecvt<mychar, char, mbstate_t>::
  do_in(state_type&, const extern_type*, const extern_type*,
	const extern_type*&, intern_type*, intern_type*,
	intern_type*&) const
  { return codecvt_base::ok; }

  template<>
  codecvt<mychar, char, mbstate_t>::result
  codecvt<mychar, char, mbstate_t>::
  do_unshift(state_type&, extern_type*, extern_type*,
	     extern_type*&) const
  { return noconv; }

  template<>
  int
  codecvt<mychar, char, mbstate_t>::do_encoding() const throw()
  { return 0; }

  template<>
  bool
  codecvt<mychar, char, mbstate_t>::do_always_noconv() const throw()
  { return false; }

  template<>
  int
  codecvt<mychar, char, mbstate_t>::
  do_length(state_type&, const extern_type*, const extern_type*,
	    size_t) const
  { return 0; }

  template<>
  int
  codecvt<mychar, char, mbstate_t>::do_max_length() const throw()
  { return 4; }
}

// libstdc++/50714
void test01()
{
  using namespace std;

  {
    locale loc(locale::classic(),
	       new codecvt<mychar, char, mbstate_t>());
  }
  {
    locale loc2(locale::classic(),
		new codecvt_byname<mychar, char, mbstate_t>(ISO_8859(15,de_DE)));
  }
}

int main()
{
  test01();
  return 0;
}
