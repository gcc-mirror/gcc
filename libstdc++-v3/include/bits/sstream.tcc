// String based streams -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2001, 2002, 2003
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
// ISO C++ 14882: 27.7  String-based streams
//

#ifndef _CPP_BITS_SSTREAM_TCC
#define _CPP_BITS_SSTREAM_TCC	1

#pragma GCC system_header

#include <sstream>

namespace std
{
  template <class _CharT, class _Traits, class _Alloc>
    typename basic_stringbuf<_CharT, _Traits, _Alloc>::int_type 
    basic_stringbuf<_CharT, _Traits, _Alloc>::
    pbackfail(int_type __c)
    {
      int_type __ret = traits_type::eof();
      bool __testeof = traits_type::eq_int_type(__c, traits_type::eof());
      bool __testpos = this->_M_in_cur && this->_M_in_beg < this->_M_in_cur; 
      
      // Try to put back __c into input sequence in one of three ways.
      // Order these tests done in is unspecified by the standard.
      if (__testpos)
	{
	  if (traits_type::eq(traits_type::to_char_type(__c), this->gptr()[-1])
	      && !__testeof)
	    {
	      --this->_M_in_cur;
	      __ret = __c;
	    }
	  else if (!__testeof)
	    {
	      --this->_M_in_cur;
	      *this->_M_in_cur = traits_type::to_char_type(__c);
	      __ret = __c;
	    }
	  else if (__testeof)
	    {
	      --this->_M_in_cur;
	      __ret = traits_type::not_eof(__c);
	    }
	}
      return __ret;
    }
  
  template <class _CharT, class _Traits, class _Alloc>
    typename basic_stringbuf<_CharT, _Traits, _Alloc>::int_type 
    basic_stringbuf<_CharT, _Traits, _Alloc>::
    overflow(int_type __c)
    {
      bool __testout = this->_M_mode & ios_base::out;
      if (__builtin_expect(!__testout, false))
	return traits_type::eof();

      bool __testeof = traits_type::eq_int_type(__c, traits_type::eof());
      if (__builtin_expect(__testeof, false))
	return traits_type::not_eof(__c);

      // NB: Start ostringstream buffers at 512 chars. This is an
      // experimental value (pronounced "arbitrary" in some of the
      // hipper english-speaking countries), and can be changed to
      // suit particular needs.
      __size_type __len = std::max(__size_type(_M_string.capacity() + 1),
				   __size_type(512));
      bool __testput = this->_M_out_cur < this->_M_out_end;
      if (__builtin_expect(!__testput && __len > _M_string.max_size(), false))
	return traits_type::eof();

      // Try to append __c into output sequence in one of two ways.
      // Order these tests done in is unspecified by the standard.
      if (!__testput)
	{
	  // Force-allocate, re-sync.
	  _M_string = this->str();
	  // In virtue of DR 169 (TC) we are allowed to grow more than
	  // one char. That's easy to implement thanks to the exponential
	  // growth policy builtin into basic_string.
	  _M_string.reserve(__len);
	  _M_really_sync(const_cast<char_type*>(_M_string.data()),
			 this->_M_in_cur - this->_M_in_beg, 
			 this->_M_out_cur - this->_M_out_beg);
	}
      return this->sputc(traits_type::to_char_type(__c));
    }

  template <class _CharT, class _Traits, class _Alloc>
    typename basic_stringbuf<_CharT, _Traits, _Alloc>::pos_type
    basic_stringbuf<_CharT, _Traits, _Alloc>::
    seekoff(off_type __off, ios_base::seekdir __way, ios_base::openmode __mode)
    {
      pos_type __ret =  pos_type(off_type(-1)); 
      bool __testin = (ios_base::in & this->_M_mode & __mode) != 0;
      bool __testout = (ios_base::out & this->_M_mode & __mode) != 0;
      bool __testboth = __testin && __testout && __way != ios_base::cur;
      __testin &= !(__mode & ios_base::out);
      __testout &= !(__mode & ios_base::in);

      if (_M_string.capacity() && (__testin || __testout || __testboth))
	{
	  char_type* __beg = this->_M_buf;
	  char_type* __curi = NULL;
	  char_type* __curo = NULL;
	  char_type* __endi = NULL;
	  char_type* __endo = NULL;

	  if (__testin || __testboth)
	    {
	      __curi = this->gptr();
	      __endi = this->egptr();
	    }
	  if (__testout || __testboth)
	    {
	      __curo = this->pptr();
	      // Due to the resolution of DR169, ios_base::end
	      // is this->_M_out_lim, not epptr().
	      __endo = this->_M_out_lim;
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

	  if ((__testin || __testboth)
	      && __newoffi + __off >= 0 && __endi - __beg >= __newoffi + __off)
	    {
	      this->_M_in_cur = __beg + __newoffi + __off;
	      __ret = pos_type(__newoffi);
	    }
	  if ((__testout || __testboth)
	      && __newoffo + __off >= 0 && __endo - __beg >= __newoffo + __off)
	    {
	      _M_out_cur_move(__newoffo + __off - (this->_M_out_cur - __beg));
	      __ret = pos_type(__newoffo);
	    }
	}
      return __ret;
    }

  template <class _CharT, class _Traits, class _Alloc>
    typename basic_stringbuf<_CharT, _Traits, _Alloc>::pos_type
    basic_stringbuf<_CharT, _Traits, _Alloc>::
    seekpos(pos_type __sp, ios_base::openmode __mode)
    {
      pos_type __ret =  pos_type(off_type(-1)); 
      
      if (_M_string.capacity())
	{
	  off_type __pos = __sp; // Use streamoff operator to do conversion.
	  char_type* __beg = NULL;
	  char_type* __end = NULL;
	  bool __testin = (ios_base::in & this->_M_mode & __mode) != 0;
	  bool __testout = (ios_base::out & this->_M_mode & __mode) != 0;
	  
	  // NB: Ordered.
	  bool __testposi = false;
	  bool __testposo = false;
	  if (__testin)
	    {
	      __beg = this->eback();
	      __end = this->egptr();
	      if (0 <= __pos && __pos <= __end - __beg)
		__testposi = true;
	    }
	  if (__testout)
	    {
	      __beg = this->pbase();
	      __end = this->epptr();
	      if (0 <= __pos && __pos <= __end - __beg)
		__testposo = true;
	    }
	  if (__testposi || __testposo)
	    {
	      if (__testposi)
		this->_M_in_cur = this->_M_in_beg + __pos;
	      if (__testposo)
		_M_out_cur_move((__pos) - (this->_M_out_cur - __beg));
	      __ret = pos_type(off_type(__pos));
	    }
	}
      return __ret;
    }

  // Inhibit implicit instantiations for required instantiations,
  // which are defined via explicit instantiations elsewhere.  
  // NB:  This syntax is a GNU extension.
  extern template class basic_stringbuf<char>;
  extern template class basic_istringstream<char>;
  extern template class basic_ostringstream<char>;
  extern template class basic_stringstream<char>;

#ifdef _GLIBCPP_USE_WCHAR_T
  extern template class basic_stringbuf<wchar_t>;
  extern template class basic_istringstream<wchar_t>;
  extern template class basic_ostringstream<wchar_t>;
  extern template class basic_stringstream<wchar_t>;
#endif
} // namespace std

#endif
