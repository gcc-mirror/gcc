// Stream buffer classes -*- C++ -*-

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
// ISO C++ 14882: 27.5  Stream buffers
//

#ifndef _CPP_BITS_STREAMBUF_TCC
#define _CPP_BITS_STREAMBUF_TCC 1

namespace std {

  template<typename _CharT, typename _Traits>
    basic_streambuf<_CharT, _Traits>::int_type
    basic_streambuf<_CharT, _Traits>::
    sbumpc()
    {
      int_type __retval;
      if (_M_in_cur && _M_in_cur < _M_in_end)
	{
	  char_type __c = *gptr();
	  ++_M_in_cur;
	  if (_M_buf_unified &&  _M_mode & ios_base::out)
	    ++_M_out_cur;
	  __retval = traits_type::to_int_type(__c);
	}
      else 
	__retval = this->uflow();
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    basic_streambuf<_CharT, _Traits>::int_type
    basic_streambuf<_CharT, _Traits>::
    sputbackc(char_type __c) 
    {
      int_type __retval;
      bool __testpos = _M_in_cur && _M_in_beg < _M_in_cur;
      bool __testne = _M_in_cur && !traits_type::eq(__c, this->gptr()[-1]);
      if (!__testpos || __testne)
	__retval = pbackfail(traits_type::to_int_type(__c));
      else 
	{
	  --_M_in_cur;
	  if (_M_buf_unified && _M_mode & ios_base::out)
	    --_M_out_cur;
	  __retval = traits_type::to_int_type(*this->gptr());
	}
      return __retval;
    }
  
  template<typename _CharT, typename _Traits>
    basic_streambuf<_CharT, _Traits>::int_type
    basic_streambuf<_CharT, _Traits>::
    sungetc()
    {
      int_type __retval;
      if (_M_in_cur && _M_in_beg < _M_in_cur)
	{
	  --_M_in_cur;
	  if (_M_buf_unified && _M_mode & ios_base::out)
	    --_M_out_cur;
	  __retval = traits_type::to_int_type(*_M_in_cur);
	}
      else 
	__retval = this->pbackfail();
      return __retval;
    }

  // Don't test against _M_buf + _M_buf_size, because _M_buf reflects
  // allocated space, and on certain (rare but entirely legal)
  // situations, there will be no allocated space yet the internal
  // buffers will still be valid. (This happens if setp is used to set
  // the internal buffer to say some externally-allocated sequence.)
  template<typename _CharT, typename _Traits>
    basic_streambuf<_CharT, _Traits>::int_type
    basic_streambuf<_CharT, _Traits>::
    sputc(char_type __c)
    {
      int_type __retval;

      if (_M_out_cur && _M_out_cur < _M_out_beg + _M_buf_size)
	{
	  *_M_out_cur = __c;
	  _M_buf_bump(1);
	  __retval = traits_type::to_int_type(__c);
	}
      else
	__retval = this->overflow(traits_type::to_int_type(__c));
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    streamsize
    basic_streambuf<_CharT, _Traits>::
    xsgetn(char_type* __s, streamsize __n)
    {
      bool __testout = _M_mode & ios_base::out;
      streamsize __retval = 0;

      if (__n)
	{
	  while (__retval < __n)
	    {
	      if (_M_in_cur < _M_in_end)
		{
		  size_t __len;
		  if (_M_in_cur + __n - __retval <= _M_in_end)
		    __len = __n - __retval;
		  else
		    __len = _M_in_end - _M_in_cur;
		  traits_type::copy(__s, _M_in_cur, __len);
		  __retval += __len;
		  __s += __len;
		  _M_in_cur += __len;
		  if (_M_buf_unified && __testout)
		    _M_out_cur += __len;
		}
	      
	      if (__retval != __n)
		{
		  if (this->uflow() != traits_type::eof())
		    ++__retval;
		  else
		    break;
		}
	    }
	}
      return __retval;
    }

  // Don't test against _M_buf + _M_buf_size, because _M_buf reflects
  // allocated space, and on certain (rare but entirely legal)
  // situations, there will be no allocated space yet the internal
  // buffers will still be valid. (This happens if setp is used to set
  // the internal buffer to say some externally-allocated sequence.)
  template<typename _CharT, typename _Traits>
    streamsize
    basic_streambuf<_CharT, _Traits>::
    xsputn(const char_type* __s, streamsize __n)
    {
      streamsize __retval = 0;

      if (__n)
	{
	  while (__retval < __n)
	    {
	      bool __testput = _M_out_cur < _M_out_beg + _M_buf_size;
	      bool __testout = _M_mode & ios_base::out;
	      if (!(__testput && __testout))
		{
		  char_type __c = *__s;
		  char_type __overfc = this->overflow(__c);
		  if (__c == __overfc)
		    {
		      ++__retval;
		      ++__s;
		    }
		  else
		    break;
		}
	      
	      if (__retval != __n)
		{
		  size_t __len;
		  if (_M_out_cur + __n - __retval <= _M_out_beg + _M_buf_size)
		    __len = __n - __retval;
		  else
		    __len = _M_out_beg + _M_buf_size - _M_out_cur;
		  traits_type::copy(_M_out_cur, __s, __len);
		  __retval += __len;
		  __s += __len;
		  _M_buf_bump(__len);
		}
	    }
	}
      return __retval;
    }


  // Conceivably, this could be used to implement buffer-to-buffer
  // copies, if this was ever desired in an un-ambiguous way by the
  // standard. If so, then checks for __ios being zero would be
  // necessary.
  template<typename _CharT, typename _Traits>
    static streamsize
    _S_copy_streambufs(basic_ios<_CharT, _Traits>& __ios,
		       basic_streambuf<_CharT, _Traits>* __sbin,
		       basic_streambuf<_CharT, _Traits>* __sbout) 
    {
      typedef typename _Traits::int_type	int_type;

      streamsize __retval = 0;
      streamsize __bufsize = __sbin->in_avail();
      streamsize __xtrct;
      bool __testout = __sbin->_M_mode & ios_base::out;
      bool __testput = __sbout->_M_mode & ios_base::out;
      try {
	while (__testput && __bufsize != -1)
	  {
	    __xtrct = __sbout->sputn(__sbin->gptr(), __bufsize);
	    __retval += __xtrct;
	    __sbin->_M_in_cur += __xtrct;
	    if (__testout && __sbin->_M_buf_unified)
	      __sbin->_M_out_cur += __xtrct;
	    if (__xtrct == __bufsize)
	      {
		int_type __c = __sbin->sgetc();
		if (__c == _Traits::eof())
		  {
		    __ios.setstate(ios_base::eofbit);
		    break;
		  }
	      }
	    else
	      break;
	  }
      }
      catch(exception& __fail) {
	if ((__ios.exceptions() & ios_base::failbit) != 0)
	  throw;
      }
      return __retval;
    }

} // namespace std

#endif // _CPP_BITS_STREAMBUF_TCC




