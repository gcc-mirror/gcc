// 1999-09-20 bkoz

// Copyright (C) 1999 Free Software Foundation, Inc.
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

// 27.6.2.1 basic_ostream
// NB: This file is for testing ostream with NO OTHER INCLUDES.

// XXX only if using namespaces, as without leads to duplicate symbols
#if _GLIBCPP_USE_NAMESPACES 
#include <ostream>
#include <bits/basic_ios.tcc> // XXX Hack, in a perfect world this not needed.
#include <locale> // XXX As above, needed for ctype/num_get use_facets(...)
#include <bits/locale_facets.tcc>

namespace test {
 
  using namespace std;
  typedef short type_t;
  template class basic_ostream<type_t, char_traits<type_t> >;

} // test
#endif

int main() {
  return 0;
}

