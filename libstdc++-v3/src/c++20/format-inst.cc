// Definitions for <format> -*- C++ -*-

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

#include <format>

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<typename _Context>
    typename basic_format_arg<_Context>::handle
    basic_format_arg<_Context>::_M_handle_unrecognized() const
    {
      // If _M_type corresponds to a new value of _Arg_t introduced after GCC 16,
      // this function should return a handle that refers to the union member of
      // _M_val corresponding to that _Arg_t value.	    
      __throw_format_error("format error: unrecognized argument type");
    }

 template basic_format_arg<format_context>::handle
   basic_format_arg<format_context>::_M_handle_unrecognized() const;

# ifdef _GLIBCXX_USE_WCHAR_T
 template basic_format_arg<wformat_context>::handle
   basic_format_arg<wformat_context>::_M_handle_unrecognized() const;
# endif

namespace __format
{

 template _Sink_iter<char>
   __do_vformat_to<char, 1>(_Sink_iter<char>, string_view,
			    format_context&);

# ifdef _GLIBCXX_USE_WCHAR_T
 template _Sink_iter<wchar_t>
   __do_vformat_to<wchar_t, 1>(_Sink_iter<wchar_t>, wstring_view,
			       wformat_context&);
# endif

} // namespace __format
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
