// File based streams -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003
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
// ISO C++ 14882: 27.8  File-based streams
//

#ifndef _CPP_BITS_FSTREAM_TCC
#define _CPP_BITS_FSTREAM_TCC 1

#pragma GCC system_header

namespace std
{
  template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    _M_allocate_internal_buffer()
    {
      if (!_M_buf_allocated && this->_M_buf_size)
	{
	  // Allocate internal buffer.
	  this->_M_buf = new char_type[this->_M_buf_size];
	  _M_buf_allocated = true;
	}
    }

  // Both close and setbuf need to deallocate internal buffers, if it exists.
  template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    _M_destroy_internal_buffer() throw()
    {
      if (_M_buf_allocated)
	{
	  delete [] this->_M_buf;
	  this->_M_buf = NULL;
	  _M_buf_allocated = false;
	}
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::
    basic_filebuf() : __streambuf_type(), _M_file(&_M_lock), 
    _M_state_cur(__state_type()), _M_state_beg(__state_type()),
    _M_buf(NULL), _M_buf_size(BUFSIZ), _M_buf_allocated(false),
    _M_reading(false), _M_writing(false), _M_last_overflowed(false),
    _M_pback_cur_save(0), _M_pback_end_save(0), _M_pback_init(false),
    _M_codecvt(0)
    { 
      if (has_facet<__codecvt_type>(this->_M_buf_locale))
	_M_codecvt = &use_facet<__codecvt_type>(this->_M_buf_locale);
    }

  template<typename _CharT, typename _Traits>
    typename basic_filebuf<_CharT, _Traits>::__filebuf_type* 
    basic_filebuf<_CharT, _Traits>::
    open(const char* __s, ios_base::openmode __mode)
    {
      __filebuf_type *__ret = NULL;
      if (!this->is_open())
	{
	  _M_file.open(__s, __mode);
	  if (this->is_open())
	    {
	      _M_allocate_internal_buffer();
	      this->_M_mode = __mode;

	      // Setup initial buffer to 'uncommitted' mode.
	      _M_reading = false;
	      _M_writing = false;
	      _M_set_buffer(-1);

	      if ((__mode & ios_base::ate) 
		  && this->seekoff(0, ios_base::end, __mode) < 0)
		// 27.8.1.3,4
		this->close();
	      else
		__ret = this;
	    }
	}
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    typename basic_filebuf<_CharT, _Traits>::__filebuf_type* 
    basic_filebuf<_CharT, _Traits>::
    close() throw()
    {
      __filebuf_type* __ret = NULL;
      if (this->is_open())
	{
	  bool __testfail = false;
	  try
	    {
	      if (this->pbase() < this->pptr()
		  && traits_type::eq_int_type(this->overflow(),
					      traits_type::eof()))
		__testfail = true;
#if 0
	      // XXX not done
	      if (_M_last_overflowed)
		{
		  _M_output_unshift();
		  this->overflow();
		}
#endif
	    }
	  catch(...)
	    {
	      __testfail = true;
	    }
	      
	  // NB: Do this here so that re-opened filebufs will be cool...
	  this->_M_mode = ios_base::openmode(0);
	  this->_M_pback_init = false;
	  _M_destroy_internal_buffer();
	  _M_reading = false;
	  _M_writing = false;
	  _M_set_buffer(-1);
	  
	  if (!_M_file.close())
	    __testfail = true;

	  if (!__testfail)
	    __ret = this;
	}
      _M_last_overflowed = false;
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    streamsize 
    basic_filebuf<_CharT, _Traits>::
    showmanyc()
    {
      streamsize __ret = -1;
      const bool __testin = this->_M_mode & ios_base::in;

      if (__testin && this->is_open())
	{
	  // For a stateful encoding (-1) the pending sequence might be just
	  // shift and unshift prefixes with no actual character.
	  __ret = this->egptr() - this->gptr();
	  if (__check_facet(_M_codecvt).encoding() >= 0)
	    __ret += _M_file.showmanyc() / _M_codecvt->max_length();
	}

      _M_last_overflowed = false;	
      return __ret;
    }
  
  template<typename _CharT, typename _Traits>
    typename basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    _M_underflow(bool __bump)
    {
      int_type __ret = traits_type::eof();
      const bool __testin = this->_M_mode & ios_base::in;
      const bool __testout = this->_M_mode & ios_base::out;

      if (__testin && !_M_writing)
	{
	  // Check for pback madness, and if so swich back to the
	  // normal buffers and jet outta here before expensive
	  // fileops happen...
	  _M_destroy_pback();

	  if (this->gptr() < this->egptr())
	    {
	      __ret = traits_type::to_int_type(*this->gptr());
	      if (__bump)
		this->gbump(1);
	      return __ret;
	    }

	  // Get and convert input sequence.
	  const size_t __buflen = this->_M_buf_size > 1
	                          ? this->_M_buf_size - 1 : 1;
	  streamsize __elen = 0;
	  streamsize __ilen = 0;
	  if (__check_facet(_M_codecvt).always_noconv())
	    {
	      __elen = _M_file.xsgetn(reinterpret_cast<char*>(this->eback()), __buflen);
	      __ilen = __elen;
	    }
	  else
	    {
	      // Worst-case number of external bytes.
	      // XXX Not done encoding() == -1.
	      const streamsize __blen = __buflen * _M_codecvt->max_length();
	      char* __buf = static_cast<char*>(__builtin_alloca(__blen));
	      __elen = _M_file.xsgetn(__buf, __blen);

 	      const char* __eend;
	      char_type* __iend;
	      codecvt_base::result __r;
	      __r = _M_codecvt->in(_M_state_cur, __buf, __buf + __elen, 
				   __eend, this->eback(), 
				   this->eback() + __buflen, __iend);
	      if (__r == codecvt_base::ok || __r == codecvt_base::partial)
		__ilen = __iend - this->eback();
	      else if (__r == codecvt_base::noconv)
		{
		  traits_type::copy(this->eback(),
				    reinterpret_cast<char_type*>(__buf), 
				    __elen);
		  __ilen = __elen;
		}
	      else 
		{
		  // Unwind.
		  __ilen = 0;
		  _M_file.seekoff(-__elen, ios_base::cur, ios_base::in);
		}
	    }

	  if (__ilen > 0)
	    {
	      _M_set_buffer(__ilen);
	      _M_reading = true;
	      __ret = traits_type::to_int_type(*this->gptr());
	      if (__bump)
		this->gbump(1);
	    }
	  else if (__elen == 0)
	    {
	      // If the actual end of file is reached, set 'uncommitted'
	      // mode, thus allowing an immediate write without an 
	      // intervening seek.
	      _M_set_buffer(-1);
	      _M_reading = false;
	    }
	}
      _M_last_overflowed = false;	
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    typename basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    pbackfail(int_type __i)
    {
      int_type __ret = traits_type::eof();
      const bool __testin = this->_M_mode & ios_base::in;

      if (__testin && !_M_writing)
	{
	  // Remember whether the pback buffer is active, otherwise below
	  // we may try to store in it a second char (libstdc++/9761).
	  const bool __testpb = this->_M_pback_init;	   
	  const bool __testeof = traits_type::eq_int_type(__i, __ret);
	  
	  int_type __tmp;
	  if (this->eback() < this->gptr())
	    {
	      this->gbump(-1);
	      __tmp = traits_type::to_int_type(*this->gptr());
	    }
	  else if (this->seekoff(-1, ios_base::cur) >= 0)
	    {
	      __tmp = this->underflow();
	      if (traits_type::eq_int_type(__tmp, __ret))
		return __ret;
	    }
	  else
	    {
	      // At the beginning of the buffer, need to make a
	      // putback position available.  But the seek may fail
	      // (f.i., at the beginning of a file, see
	      // libstdc++/9439) and in that case we return
	      // traits_type::eof().
	      return __ret;
	    }

	  // Try to put back __i into input sequence in one of three ways.
	  // Order these tests done in is unspecified by the standard.
	  if (!__testeof && traits_type::eq_int_type(__i, __tmp))
	    __ret = __i;
	  else if (__testeof)
	    __ret = traits_type::not_eof(__i);
	  else if (!__testpb)
	    {
	      _M_create_pback();
	      _M_reading = true;
	      *this->gptr() = traits_type::to_char_type(__i); 
	      __ret = __i;
	    }
	}
      _M_last_overflowed = false;	
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    typename basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    overflow(int_type __c)
    {
      int_type __ret = traits_type::eof();
      const bool __testeof = traits_type::eq_int_type(__c, __ret);
      const bool __testout = this->_M_mode & ios_base::out;
      
      if (__testout && !_M_reading)
	{
	  if (this->pbase() < this->pptr())
	    {
	      // If appropriate, append the overflow char.
	      if (!__testeof)
		{
		  *this->pptr() = traits_type::to_char_type(__c);
		  this->pbump(1);
		}
	      
	      // Convert pending sequence to external representation,
	      // output.
	      if (_M_convert_to_external(this->pbase(),
					 this->pptr() - this->pbase())
		  && (!__testeof || (__testeof && !_M_file.sync())))
		{
		  _M_set_buffer(0);
		  __ret = traits_type::not_eof(__c);
		}
	    }
	  else if (this->_M_buf_size > 1)
	    {
	      // Overflow in 'uncommitted' mode: set _M_writing, set
	      // the buffer to the initial 'write' mode, and put __c
	      // into the buffer.
	      _M_set_buffer(0);
	      _M_writing = true;
	      if (!__testeof)
		{
		  *this->pptr() = traits_type::to_char_type(__c);
		  this->pbump(1);
		}
	      __ret = traits_type::not_eof(__c);
	    }
	  else
	    {
	      // Unbuffered.
	      char_type __conv = traits_type::to_char_type(__c);
	      if (__testeof || _M_convert_to_external(&__conv, 1))
		{		  
		  _M_writing = true;
		  __ret = traits_type::not_eof(__c);
		}
	    }
	}
      _M_last_overflowed = true;	
      return __ret;
    }
  
  template<typename _CharT, typename _Traits>
    bool
    basic_filebuf<_CharT, _Traits>::
    _M_convert_to_external(_CharT* __ibuf, streamsize __ilen)
    {
      // Sizes of external and pending output.
      streamsize __elen = 0;
      streamsize __plen = 0;

      if (__check_facet(_M_codecvt).always_noconv())
	{
	  __elen += _M_file.xsputn(reinterpret_cast<char*>(__ibuf), __ilen);
	  __plen += __ilen;
	}
      else
	{
	  // Worst-case number of external bytes needed.
	  // XXX Not done encoding() == -1.
	  streamsize __blen = __ilen * _M_codecvt->max_length();
	  char* __buf = static_cast<char*>(__builtin_alloca(__blen));

	  char* __bend;
	  const char_type* __iend;
	  codecvt_base::result __r;
	  __r = _M_codecvt->out(_M_state_cur, __ibuf, __ibuf + __ilen,
				__iend, __buf, __buf + __blen, __bend);
	  
	  if (__r == codecvt_base::ok || __r == codecvt_base::partial)
	    __blen = __bend - __buf;
	  else if (__r == codecvt_base::noconv)
	    {
	      // Same as the always_noconv case above.
	      __buf = reinterpret_cast<char*>(__ibuf);
	      __blen = __ilen;
	    }
	  else
	    {
	      // Result == error.
	      __blen = 0;
	    }
	  
	  if (__blen)
	    {
	      __elen += _M_file.xsputn(__buf, __blen);
	      __plen += __blen;
	    }
	  
	  // Try once more for partial conversions.
	  if (__r == codecvt_base::partial)
	    {
	      const char_type* __iresume = __iend;
	      streamsize __rlen = this->pptr() - __iend;
	      __r = _M_codecvt->out(_M_state_cur, __iresume,
				    __iresume + __rlen, __iend, __buf, 
				    __buf + __blen, __bend);
	      if (__r != codecvt_base::error)
		{
		  __rlen = __bend - __buf;
		  __elen += _M_file.xsputn(__buf, __rlen);
		  __plen += __rlen;
		}
	    }
	}
      return __elen && __elen == __plen;
    }

  template<typename _CharT, typename _Traits>
    typename basic_filebuf<_CharT, _Traits>::__streambuf_type* 
    basic_filebuf<_CharT, _Traits>::
    setbuf(char_type* __s, streamsize __n)
    {
      if (!this->is_open() && __s == 0 && __n == 0)
	this->_M_buf_size = 1;
      else if (__s && __n > 0)
	{
	  // This is implementation-defined behavior, and assumes that
	  // an external char_type array of length __n exists and has
	  // been pre-allocated. If this is not the case, things will
	  // quickly blow up. When __n > 1, __n - 1 positions will be
	  // used for the get area, __n - 1 for the put area and 1
	  // position to host the overflow char of a full put area.
	  // When __n == 1, 1 position will be used for the get area
	  // and 0 for the put area, as in the unbuffered case above.

	  // Step 1: Destroy the current internal array.
	  _M_destroy_internal_buffer();
	  
	  // Step 2: Use the external array.
	  this->_M_buf = __s;
	  this->_M_buf_size = __n;
	  _M_reading = false;
	  _M_writing = false;
	  _M_set_buffer(-1);
	}
      _M_last_overflowed = false;	
      return this; 
    }
  
  template<typename _CharT, typename _Traits>
    typename basic_filebuf<_CharT, _Traits>::pos_type
    basic_filebuf<_CharT, _Traits>::
    seekoff(off_type __off, ios_base::seekdir __way, ios_base::openmode __mode)
    {
      pos_type __ret =  pos_type(off_type(-1)); 
      const bool __testin = (ios_base::in & this->_M_mode & __mode) != 0;
      const bool __testout = (ios_base::out & this->_M_mode & __mode) != 0;
      
      int __width = 0;
      if (_M_codecvt)
	  __width = _M_codecvt->encoding();
      if (__width < 0)
	__width = 0;

      const bool __testfail = __off != 0 && __width <= 0;      
      if (this->is_open() && !__testfail && (__testin || __testout)) 
	{
	  // Ditch any pback buffers to avoid confusion.
	  _M_destroy_pback();

	  // Sync the internal and external streams.	      
	  off_type __computed_off = __width * __off;
	  
	  if (this->pbase() < this->pptr()
	      || _M_last_overflowed)
	    {
	      // Part one: update the output sequence.
	      this->sync();
	      
	      // Part two: output unshift sequence.
	      _M_output_unshift();
	    }
	  else if (_M_reading && __way == ios_base::cur)
	    __computed_off += this->gptr() - this->egptr();
	  
	  // Return pos_type(off_type(-1)) in case of failure.
	  __ret = _M_file.seekoff(__computed_off, __way, __mode);
	  _M_reading = false;
	  _M_writing = false;
	  _M_set_buffer(-1);
	}
      _M_last_overflowed = false;	
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    typename basic_filebuf<_CharT, _Traits>::pos_type
    basic_filebuf<_CharT, _Traits>::
    seekpos(pos_type __pos, ios_base::openmode __mode)
    {
#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
// 171. Strange seekpos() semantics due to joint position
      return this->seekoff(off_type(__pos), ios_base::beg, __mode);
#endif
    }

  template<typename _CharT, typename _Traits>
    void 
    basic_filebuf<_CharT, _Traits>::
    _M_output_unshift()
    { }

  template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    imbue(const locale& __loc)
    {
      const bool __testbeg = !this->seekoff(0, ios_base::cur, this->_M_mode);
      const bool __teststate = __check_facet(_M_codecvt).encoding() == -1;

      if (this->_M_buf_locale != __loc 
	  && (!this->is_open() || (__testbeg && !__teststate)))
	{
	  this->_M_buf_locale = __loc;
	  if (__builtin_expect(has_facet<__codecvt_type>(__loc), true))
	    _M_codecvt = &use_facet<__codecvt_type>(__loc);

	  // NB This may require the reconversion of previously
	  // converted chars. This in turn may cause the
	  // reconstruction of the original file. YIKES!!  This
	  // implementation interprets this requirement as requiring
	  // the file position be at the beginning, and a stateless
	  // encoding, or that the filebuf be closed. Opinions may differ.
	}
      _M_last_overflowed = false;	
    }

  // Inhibit implicit instantiations for required instantiations,
  // which are defined via explicit instantiations elsewhere.  
  // NB:  This syntax is a GNU extension.
#if _GLIBCPP_EXTERN_TEMPLATE
  extern template class basic_filebuf<char>;
  extern template class basic_ifstream<char>;
  extern template class basic_ofstream<char>;
  extern template class basic_fstream<char>;

#ifdef _GLIBCPP_USE_WCHAR_T
  extern template class basic_filebuf<wchar_t>;
  extern template class basic_ifstream<wchar_t>;
  extern template class basic_ofstream<wchar_t>;
  extern template class basic_fstream<wchar_t>;
#endif
#endif
} // namespace std

#endif 
