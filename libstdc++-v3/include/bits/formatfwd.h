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

#include <concepts>
#include <type_traits>
#if __glibcxx_format_ranges // C++ >= 23 && HOSTED
#  include <bits/ranges_base.h>  // input_range, range_reference_t
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // [format.context], class template basic_format_context
  template<typename _Out, typename _CharT> class basic_format_context;

  // [format.parse.ctx], class template basic_format_parse_context
  template<typename _CharT> class basic_format_parse_context;

  // [format.formatter], formatter
  template<typename _Tp, typename _CharT = char> struct formatter;

/// @cond undocumented
namespace __format
{
#ifdef _GLIBCXX_USE_WCHAR_T
  template<typename _CharT>
    concept __char = same_as<_CharT, char> || same_as<_CharT, wchar_t>;
#else
  template<typename _CharT>
    concept __char = same_as<_CharT, char>;
#endif

  template<typename _Tp, typename _Context,
	   typename _Formatter
	     = typename _Context::template formatter_type<remove_const_t<_Tp>>,
	   typename _ParseContext
	     = basic_format_parse_context<typename _Context::char_type>>
    concept __parsable_with
      = semiregular<_Formatter>
	  && requires (_Formatter __f, _ParseContext __pc)
    {
      { __f.parse(__pc) } -> same_as<typename _ParseContext::iterator>;
    };

  template<typename _Tp, typename _Context,
	   typename _Formatter
	     = typename _Context::template formatter_type<remove_const_t<_Tp>>,
	   typename _ParseContext
	     = basic_format_parse_context<typename _Context::char_type>>
    concept __formattable_with
      = semiregular<_Formatter>
	  && requires (const _Formatter __cf, _Tp&& __t, _Context __fc)
    {
      { __cf.format(__t, __fc) } -> same_as<typename _Context::iterator>;
    };

  // An unspecified output iterator type used in the `formattable` concept.
  template<typename _CharT>
    struct _Iter_for;
  template<typename _CharT>
    using _Iter_for_t = typename _Iter_for<_CharT>::type;

  template<typename _Tp, typename _CharT,
	   typename _Context = basic_format_context<_Iter_for_t<_CharT>, _CharT>>
    concept __formattable_impl
      = __parsable_with<_Tp, _Context> && __formattable_with<_Tp, _Context>;

  template<typename _Formatter>
    concept __has_debug_format = requires(_Formatter __f)
    {
      __f.set_debug_format();
    };

  template<__char _CharT>
    struct __formatter_int;
} // namespace __format
/// @endcond

#if __glibcxx_format_ranges // C++ >= 23 && HOSTED
  // [format.formattable], concept formattable
  template<typename _Tp, typename _CharT>
    concept formattable
      = __format::__formattable_impl<remove_reference_t<_Tp>, _CharT>;

   template<typename _Tp, __format::__char _CharT = char>
     requires same_as<remove_cvref_t<_Tp>, _Tp> && formattable<_Tp, _CharT>
     class range_formatter;

/// @cond undocumented
namespace __format
{
  template<typename _Rg, typename _CharT>
    concept __const_formattable_range
      = ranges::input_range<const _Rg>
	  && formattable<ranges::range_reference_t<const _Rg>, _CharT>;

  template<typename _Rg, typename _CharT>
    using __maybe_const_range
      = __conditional_t<__const_formattable_range<_Rg, _CharT>, const _Rg, _Rg>;

  template<typename _Tp, typename _CharT>
    using __maybe_const
      = __conditional_t<formattable<const _Tp, _CharT>, const _Tp, _Tp>;
}
#endif // format_ranges


_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __glibcxx_format
#endif // _GLIBCXX_FORMAT_FWD_H
