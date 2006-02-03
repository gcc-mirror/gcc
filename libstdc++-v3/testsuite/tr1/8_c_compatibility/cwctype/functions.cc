// { dg-do compile }

// 2006-02-03  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2006 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 8.32 Additions to header <cwctype>

#include <tr1/cwctype>

#if _GLIBCXX_USE_WCHAR_T

void test01()
{
#if _GLIBCXX_USE_C99_WCTYPE_TR1

  std::wint_t ch = 0;
  int ret;
  ret = std::tr1::iswblank(ch);

#endif
}

#endif
