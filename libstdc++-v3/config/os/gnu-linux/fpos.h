// File position object and stream types, GNU version -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2003 
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

//
// ISO C++ 14882: 27 Input/output library
//

/** @file fpos.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _CPP_BITS_FPOS_H
#define _CPP_BITS_FPOS_H 1

#pragma GCC system_header

#include <bits/c++io.h>
#include <cwchar> 	// For mbstate_t.

namespace std
{
  // 27.4.1  Types

  // [27.4.3] template class fpos
  /**
   *  @doctodo
  */
  template<typename _StateT>
    class fpos
    {
    private:
      fpos_t		_M_pos;

    public:
      _StateT
      state() const;

      void 
      state(_StateT __st);

      fpos() : _M_pos(fpos_t()) { }

      // NB: The standard defines only the implicit copy ctor and the
      // previous two members.  The rest is a "conforming extension".
      fpos(streamoff __off, _StateT __st = _StateT());

      fpos(const fpos_t& __pos) : _M_pos(__pos) { }

      operator streamoff() const { return _M_pos.__pos; }

      operator fpos_t() const { return _M_pos; }

      fpos& 
      operator+=(streamoff __off) 
      { 
	_M_pos.__pos += __off; 
	return *this; 
      }

      fpos& 
      operator-=(streamoff __off) 
      { 
	_M_pos.__pos -= __off; 
	return *this; 
      }

      fpos 
      operator+(streamoff __off) 
      { 
	fpos __t(*this); 
	__t += __off;
	return __t;
      }

      fpos      
      operator-(streamoff __off) 
      { 
	fpos __t(*this); 
	__t -= __off; 
	return __t;
      }

      bool  
      operator==(const fpos& __pos) const
      { return _M_pos.__pos == __pos._M_pos.__pos; }

      bool  
      operator!=(const fpos& __pos) const
      { return !(*this == __pos); }
    };

  template<>
    inline mbstate_t
    fpos<mbstate_t>::state() const { return _M_pos.__state; }

  template<>
    inline void 
    fpos<mbstate_t>::state(mbstate_t __st) { _M_pos.__state = __st; }

  template<>
    inline 
    fpos<mbstate_t>::fpos(streamoff __off, mbstate_t __st) : _M_pos(fpos_t())
    { 
      _M_pos.__pos = __off;
      _M_pos.__state = __st;
    }

  /// 27.2, paragraph 10 about fpos/char_traits circularity
  typedef fpos<mbstate_t> 		streampos;
#  ifdef _GLIBCXX_USE_WCHAR_T
  /// 27.2, paragraph 10 about fpos/char_traits circularity
  typedef fpos<mbstate_t> 		wstreampos;
#  endif
}  // namespace std

#endif 
