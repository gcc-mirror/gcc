// <format> Formatting -*- C++ -*-

// Copyright The GNU Toolchain Authors.
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

/** @file bits/formatfwd.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{format}
 */

#ifndef _GLIBCXX_FORMAT_FWD_H
#define _GLIBCXX_FORMAT_FWD_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

// <bits/version.h> must have been included before this header:
#ifdef __glibcxx_format // C++ >= 20 && HOSTED

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // [format.context], class template basic_format_context
  template<typename _Out, typename _CharT> class basic_format_context;

  // [format.parse.ctx], class template basic_format_parse_context
  template<typename _CharT> class basic_format_parse_context;

  // [format.formatter], formatter
  template<typename _Tp, typename _CharT = char> struct formatter;

namespace __format
{
#ifdef _GLIBCXX_USE_WCHAR_T
  template<typename _CharT>
    concept __char = same_as<_CharT, char> || same_as<_CharT, wchar_t>;
#else
  template<typename _CharT>
    concept __char = same_as<_CharT, char>;
#endif

  template<__char _CharT>
    struct __formatter_int;
}

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __glibcxx_format
#endif // _GLIBCXX_FORMAT_FWD_H
