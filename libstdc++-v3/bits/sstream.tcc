// String based streams -*- C++ -*-

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
// ISO C++ 14882: 27.7  String-based streams
//

#ifndef _CPP_BITS_SSTREAM_TCC
#define _CPP_BITS_SSTREAM_TCC	1

#include <bits/std_sstream.h>

namespace std {

  template <class _CharT, class _Traits, class _Alloc>
    basic_stringbuf<_CharT, _Traits, _Alloc>::int_type 
    basic_stringbuf<_CharT, _Traits, _Alloc>::
    pbackfail(int_type __c)
    {
      int_type __retval = traits_type::eof();
      bool __testeof = traits_type::eq_int_type(__c, traits_type::eof());
      bool __testpos = _M_in_cur && _M_in_beg < _M_in_cur; 
      
      // Try to put back __c into input sequence in one of three ways.
      // Order these tests done in is unspecified by the standard.
      if (!__testeof && __testpos 
	  && traits_type::eq(traits_type::to_char_type(__c), this->gptr()[-1]))
	{
	  --_M_in_cur;
	  __retval = __c;
	}
      else if (!__testeof && __testpos)
	{
	  --_M_in_cur;
	  *_M_in_cur = traits_type::to_char_type(__c);
	  __retval = __c;
	}
      else if (__testeof && __testpos)
	{
	  --_M_in_cur;
	  __retval = traits_type::not_eof(__c);
	}
      return __retval;
    }
  
  template <class _CharT, class _Traits, class _Alloc>
    basic_stringbuf<_CharT, _Traits, _Alloc>::int_type 
    basic_stringbuf<_CharT, _Traits, _Alloc>::
    overflow(int_type __c)
    {
      int_type __retval = traits_type::eof();
      bool __testeof = traits_type::eq_int_type(__c, __retval);
      bool __testwrite = _M_out_cur < _M_buf + _M_buf_size;
      bool __testout = _M_mode & ios_base::out;

      // Try to append __c into output sequence in one of two ways.
      // Order these tests done in is unspecified by the standard.
      if (__testout)
	{
	  if (!__testeof)
	    {
	      // NB: Start ostringstream buffers at 1024 bytes. This
	      // is an experimental value (pronounced "arbitrary" in
	      // some of the hipper english-speaking countries), and
	      // can be changed to suite particular needs.
	      __size_type __len = max(_M_buf_size, static_cast<int_type>(512));
	      __len *= 2;

	      if (__testwrite)
		__retval = this->sputc(__c);
	      else if (__len <= _M_string.max_size())
		{
		  // Force-allocate, re-sync.
		  _M_string = this->str();
		  _M_string.reserve(__len);
		  _M_buf_size = static_cast<int_type>(__len);
		  _M_really_sync(_M_in_cur - _M_in_beg, 
				 _M_out_cur - _M_out_beg);
		  *_M_out_cur = traits_type::to_char_type(__c);
		  _M_buf_bump(1);
		  __retval = __c;
		}
	    }
	  else
	    __retval = traits_type::not_eof(__c);
	}
      return __retval;
    }

  template <class _CharT, class _Traits, class _Alloc>
    basic_stringbuf<_CharT, _Traits, _Alloc>::pos_type
    basic_stringbuf<_CharT, _Traits, _Alloc>::
    seekoff(off_type __off, ios_base::seekdir __way, ios_base::openmode __mode)
    {
      pos_type __retval =  pos_type(off_type(-1)); 
      bool __testin = __mode & ios_base::in && _M_mode & ios_base::in;
      bool __testout = __mode & ios_base::out && _M_mode & ios_base::out;
      bool __testboth = __testin && __testout && __way != ios_base::cur;
      
      if (_M_buf_size && ((__testin != __testout) || __testboth))
	{
	  char_type* __beg = _M_buf;
	  char_type* __curi = NULL;
	  char_type* __curo = NULL;
	  char_type* __endi = NULL;
	  char_type* __endo = NULL;

	  if (__testin)
	    {
	      __curi = this->gptr();
	      __endi = this->egptr();
	    }
	  if (__testout)
	    {
	      __curo = this->pptr();
	      __endo = this->epptr();
	    }

	  off_type __newoffi = 0;
	  off_type __newoffo = 0;
	  if (__way == ios_base::cur)
	    {
	      __newoffi = __curi - __beg;
	      __newoffo = __curo - __beg;
	    }
	  else if (__way == ios_base::end)
	    {
	      __newoffi = __endi - __beg;
	      __newoffo = __endo - __beg;
	    }

	  if (__testin
	      && __newoffi + __off >= 0 && __endi - __beg >= __newoffi + __off)
	    {
	      _M_in_cur = __beg + __newoffi + __off;
	      __retval = pos_type(__newoffi);
	    }
	  if (__testout
	      && __newoffo + __off >= 0 && __endo - __beg >= __newoffo + __off)
	    {
	      _M_buf_bump(__newoffo + __off - (_M_out_cur - __beg));
	      __retval = pos_type(__newoffo);
	    }
	}
      return __retval;
    }

  template <class _CharT, class _Traits, class _Alloc>
    basic_stringbuf<_CharT, _Traits, _Alloc>::pos_type
    basic_stringbuf<_CharT, _Traits, _Alloc>::
    seekpos(pos_type __sp, ios_base::openmode __mode)
    {
      pos_type __retval =  pos_type(off_type(-1)); 
      off_type __pos = __sp._M_position();
      char_type* __beg = NULL;
      char_type* __end = NULL;
      bool __testin = __mode & ios_base::in && _M_mode & ios_base::in;
      bool __testout = __mode & ios_base::out && _M_mode & ios_base::out;
      
      if (__testin)
	{
	  __beg = this->eback();
	  __end = this->egptr();
	}
      if (__testout)
	{
	  __beg = this->pbase();
	  __end = _M_buf + _M_buf_size;
	}
 
      if (0 <= __pos && __pos <= __end - __beg)
	{
	  // Need to set both of these if applicable
	  if (__testin)
	    _M_in_cur = _M_in_beg + __pos;
	  if (__testout)
	    _M_buf_bump((__pos) - (_M_out_cur - __beg));
	  __retval = pos_type(off_type(__pos));
	}
      
      return __retval;
    }

} // namespace std

#endif	/* _CPP_BITS_SSTREAM_TCC */









