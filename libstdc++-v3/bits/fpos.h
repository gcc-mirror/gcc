// File position object and stream types

// Copyright (C) 1997-1999 Free Software Foundation, Inc.
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

#ifndef _CPP_BITS_FPOS_H
#define _CPP_BITS_FPOS_H 1

// Need this here as well as in std_ios because fpos is used in
// char_traits, and char_traits is used by string, which may or may
// not have included the std_ios file.
#include <bits/c++io.h>

namespace std {

  // 27.4.1  Types

  // 27.4.3  Template class fpos
  template<typename _StateT>
    class fpos
    {
    public:

      // Types:
      typedef _StateT __state_type;

      __state_type
      state() const  { return _M_st; }

      void 
      state(__state_type __st)  { _M_st = __st; }

      // NB: The standard defines only the implicit copy ctor and the
      // previous two members.  The rest is a "conforming extension".
      fpos(): _M_st(__state_type()), _M_pos(streamoff()) { }

      fpos(streamoff __pos, __state_type __st)
      : _M_st(__st), _M_pos(__pos) { }

      fpos(streamoff __pos)
      : _M_st(), _M_pos(__pos) { }

      operator streamoff() const { return _M_pos; }

      fpos& 
      operator+=(streamoff __off) { _M_pos += __off; return *this; }

      fpos& 
      operator-=(streamoff __off) { _M_pos -= __off; return *this; }

      bool  
      operator==(const fpos& __pos2) const { return _M_pos == __pos2._M_pos; }

      bool  
      operator!=(const fpos& __pos2) const { return _M_pos != __pos2._M_pos; }
      
      streamoff 
      _M_position() const { return _M_pos; }

      void
      _M_position(streamoff __pos)  { _M_pos = __pos; }

    private:
      __state_type _M_st;
      streamoff _M_pos;
    };

  template<typename _State>
    inline fpos<_State> 
    operator+(const fpos<_State>& __pos, streamoff __off)
    { 
      fpos<_State> t(__pos); 
      return t += __off; 
    }

  template<typename _State>
    inline fpos<_State>
    operator-(const fpos<_State>& __pos, streamoff __off)
    { 
      fpos<_State> t(__pos); 
      return t -= __off; 
    }

  template<typename _State>
    inline streamoff 
    operator-(const fpos<_State>& __pos1, const fpos<_State>& __pos2)
    { return __pos1._M_position() - __pos2._M_position(); }

}  // namespace std

#endif /* _CPP_BITS_FPOS_H */


