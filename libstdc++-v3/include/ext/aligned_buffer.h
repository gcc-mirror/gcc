// Aligned memory buffer -*- C++ -*-

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

/** @file ext/aligned_buffer.h
 *  This file is a GNU extension to the Standard C++ Library.
 */

#ifndef _ALIGNED_BUFFER_H
#define _ALIGNED_BUFFER_H 1

#pragma GCC system_header

#if __cplusplus >= 201103L
# include <type_traits>
#else
# include <bits/c++0x_warning.h>
#endif

namespace __gnu_cxx
{
  template<typename _Tp>
    struct __aligned_buffer
    : std::aligned_storage<sizeof(_Tp), std::alignment_of<_Tp>::value>
    {
      typename
	std::aligned_storage<sizeof(_Tp), std::alignment_of<_Tp>::value>::type
	_M_storage;

      __aligned_buffer() = default;

      // Can be used to avoid value-initialization
      __aligned_buffer(std::nullptr_t) { }

      void*
      _M_addr() noexcept
      {
        return static_cast<void*>(&_M_storage);
      }

      const void*
      _M_addr() const noexcept
      {
        return static_cast<const void*>(&_M_storage);
      }

      _Tp*
      _M_ptr() noexcept
      { return static_cast<_Tp*>(_M_addr()); }

      const _Tp*
      _M_ptr() const noexcept
      { return static_cast<const _Tp*>(_M_addr()); }
    };

} // namespace

#endif /* _ALIGNED_BUFFER_H */
