// -*- C++ -*- header.

// Copyright (C) 2008
// Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/** @file bits/atomicfwd_cxx.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

// "C++" only bits.

#define _ATOMIC_MEMBER_ _M_i

_GLIBCXX_END_EXTERN_C

  namespace __atomic0
  {
    template<typename _IntTp>
      struct __atomic_base;

    struct atomic_flag;
    struct atomic_address;
    struct atomic_bool;
  } 

  namespace __atomic2
  {
    template<typename _IntTp>
      struct __atomic_base;

    struct atomic_flag;
    struct atomic_address;
    struct atomic_bool;
  } 

  namespace __atomic1
  {
    using __atomic2::atomic_flag;
    using __atomic2::atomic_bool;
    using __atomic0::atomic_address;
    using __atomic0::__atomic_base;
  } 

  /// atomic_char
  typedef __atomic_base<char>  	       		atomic_char;

  /// atomic_schar
  typedef __atomic_base<signed char>         	atomic_schar;

  /// atomic_uchar
  typedef __atomic_base<unsigned char>  	atomic_uchar;

  /// atomic_short
  typedef __atomic_base<short>  		atomic_short;

  /// atomic_ushort
  typedef __atomic_base<unsigned short>  	atomic_ushort;

  /// atomic_int
  typedef __atomic_base<int>  	       		atomic_int;

  /// atomic_uint
  typedef __atomic_base<unsigned int>        	atomic_uint;

  /// atomic_long
  typedef __atomic_base<long>  	       		atomic_long;

  /// atomic_ulong
  typedef __atomic_base<unsigned long>  	atomic_ulong;

  /// atomic_llong
  typedef __atomic_base<long long>  		atomic_llong;

  /// atomic_ullong
  typedef __atomic_base<unsigned long long> 	atomic_ullong;

  /// atomic_wchar_t
  typedef __atomic_base<wchar_t>  		atomic_wchar_t;

  /// atomic_char16_t
  typedef __atomic_base<char16_t>  		atomic_char16_t;

  /// atomic_char32_t
  typedef __atomic_base<char32_t>  		atomic_char32_t;

  template<typename _Tp>
    struct atomic;
_GLIBCXX_BEGIN_EXTERN_C
