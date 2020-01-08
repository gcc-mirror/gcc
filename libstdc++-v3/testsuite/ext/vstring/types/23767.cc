// 2005-09-12  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//

// { dg-do compile }

#include <ext/vstring.h>

struct T
{
  typedef __gnu_cxx::__vstring Vstring;
  typedef Vstring::iterator iterator;
  typedef Vstring::const_iterator const_iterator;

  char t(iterator f)             { return *f; }
  char t(const_iterator f) const { return *f; }
};

// libstdc++/23767
void f()
{
  __gnu_cxx::__vstring vs;
  T t;
  T::const_iterator i = vs.begin();
  
  t.t(i);
}
