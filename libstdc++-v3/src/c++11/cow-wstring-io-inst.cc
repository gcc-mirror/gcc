// Reference-counted COW wide string instantiations for I/O -*- C++ -*-

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

//
// ISO C++ 14882: 21  Strings library
//

#define _GLIBCXX_USE_CXX11_ABI 0
#include <bits/c++config.h>

#ifdef _GLIBCXX_USE_WCHAR_T
#include <ostream>
#include <istream>

#if ! _GLIBCXX_USE_DUAL_ABI
# error This file should not be compiled for this configuration.
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // The equivalent SSO wstring instantiations are in c++98/misc-inst.cc,
  // repeat them for COW wstring

  // string related to iostreams
  template
    basic_istream<wchar_t>&
    operator>>(basic_istream<wchar_t>&, wstring&);
  template
    basic_ostream<wchar_t>&
    operator<<(basic_ostream<wchar_t>&, const wstring&);
  template
    basic_istream<wchar_t>&
    getline(basic_istream<wchar_t>&, wstring&, wchar_t);
  template
    basic_istream<wchar_t>&
    getline(basic_istream<wchar_t>&, wstring&);

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
#endif
