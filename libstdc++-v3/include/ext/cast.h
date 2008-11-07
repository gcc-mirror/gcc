// <cast.h> -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
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

#ifndef _EXT_CAST_
#define _EXT_CAST_ 1

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx);

  /**
   * These functions are here to allow containers to support non standard
   * pointer types.  For normal pointers, these resolve to the use of the
   * standard cast operation.  For other types the functions will perform
   * the apprpriate cast to/from the custom pointer class so long as that
   * class meets the following conditions:
   * 1) has a typedef element_type which names tehe type it points to.
   * 2) has a get() const method which returns element_type*.
   * 3) has a constructor which can take one element_type* argument.
   */

  /**
   * This type supports the semantics of the pointer cast operators (below.)
   */
  template<typename _ToType>
    struct _Caster
    {
      typedef typename _ToType::element_type*  type;
    };
  template<typename _ToType>
    struct _Caster<_ToType*>
    {
      typedef _ToType*  type;
    };

  /**
   * Casting operations for cases where _FromType is not a standard pointer.
   * _ToType can be a standard or non-standard pointer.  Given that _FromType
   * is not a pointer, it must have a get() method that returns the standard
   * pointer equivalent of the address it points to, and must have an
   * element_type typedef which names the type it points to.
   */
  template<typename _ToType, typename _FromType>
    inline _ToType
    __static_pointer_cast(const _FromType& __arg)
    { return _ToType(static_cast<typename _Caster<_ToType>::
		     type>(__arg.get())); }

  template<typename _ToType, typename _FromType>
    inline _ToType
    __dynamic_pointer_cast(const _FromType& __arg)
    { return _ToType(dynamic_cast<typename _Caster<_ToType>::
		     type>(__arg.get())); }

  template<typename _ToType, typename _FromType>
    inline _ToType
    __const_pointer_cast(const _FromType& __arg)
    { return _ToType(const_cast<typename _Caster<_ToType>::
		     type>(__arg.get())); }

  template<typename _ToType, typename _FromType>
    inline _ToType
    __reinterpret_pointer_cast(const _FromType& __arg)
    { return _ToType(reinterpret_cast<typename _Caster<_ToType>::
		     type>(__arg.get())); }

  /**
   * Casting operations for cases where _FromType is a standard pointer.
   * _ToType can be a standard or non-standard pointer.
   */
  template<typename _ToType, typename _FromType>
    inline _ToType
    __static_pointer_cast(_FromType* __arg)
    { return _ToType(static_cast<typename _Caster<_ToType>::
		     type>(__arg)); }

  template<typename _ToType, typename _FromType>
    inline _ToType
    __dynamic_pointer_cast(_FromType* __arg)
    { return _ToType(dynamic_cast<typename _Caster<_ToType>::
		     type>(__arg)); }

  template<typename _ToType, typename _FromType>
    inline _ToType
    __const_pointer_cast(_FromType* __arg)
    { return _ToType(const_cast<typename _Caster<_ToType>::
		     type>(__arg)); }

  template<typename _ToType, typename _FromType>
    inline _ToType
    __reinterpret_pointer_cast(_FromType* __arg)
    { return _ToType(reinterpret_cast<typename _Caster<_ToType>::
		     type>(__arg)); }

_GLIBCXX_END_NAMESPACE

#endif
