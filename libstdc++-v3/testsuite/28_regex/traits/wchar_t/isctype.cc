// { dg-do run { target c++11 } }
// { dg-additional-options "-DNEWLINE_IN_CLASS_BLANK" { target newlib } }

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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

// 28.3 Requirements [re.req]
// 28.2(4) Table 127 - Regular expression traits class requirements
// 28.7(11) Class template regex_traits [re.traits]

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  typedef wchar_t CharT;
  typedef std::regex_traits<CharT> traits;

  const CharT lower[]   = L"lOWer";
  const CharT upper[]   = L"UPPER";
  const CharT nothing[] = L"nothing";
  const CharT word[]    = L"w";
  const CharT blank[]   = L"blank";
  const CharT digit[]   = L"digit";
  traits t;

#define range(s) s, s+sizeof(s)/sizeof(s[0])-1
  VERIFY( t.isctype(L'_', t.lookup_classname(range(word))));
  VERIFY( t.isctype(L'A', t.lookup_classname(range(word))));
  VERIFY(!t.isctype(L'~', t.lookup_classname(range(word))));
  VERIFY(!t.isctype(L'e', t.lookup_classname(range(upper))));
  VERIFY( t.isctype(L'e', t.lookup_classname(range(lower))));
  VERIFY(!t.isctype(L'e', t.lookup_classname(range(nothing))));
  VERIFY(!t.isctype(L'_', t.lookup_classname(range(digit))));
  VERIFY( t.isctype(L' ', t.lookup_classname(range(blank))));
  VERIFY( t.isctype(L'\t', t.lookup_classname(range(blank))));
#if defined (NEWLINE_IN_CLASS_BLANK)
  /* On some targets, '\n' is in class 'blank'.
     See https://gcc.gnu.org/ml/gcc-patches/2015-02/msg00059.html.  */
  VERIFY( t.isctype(L'\n', t.lookup_classname(range(blank))));
#else
  VERIFY(!t.isctype(L'\n', t.lookup_classname(range(blank))));
#endif
  VERIFY( t.isctype(L't', t.lookup_classname(range(upper), true)));
  VERIFY( t.isctype(L'T', t.lookup_classname(range(lower), true)));
#undef range
}

int main()
{
  test01();
  return 0;
}
