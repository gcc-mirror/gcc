// File based streams -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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

namespace std
{
  template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    _M_allocate_file()
    {
      if (!_M_file)
	{
	  _M_buf_unified = true; // Tie input to output for basic_filebuf.
	  try 
	    { _M_file = new __file_type(&_M_lock); }
	  catch(...) 
	    {
	      delete _M_file;
	      __throw_exception_again;
	    }
	}
    }

  template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    _M_allocate_internal_buffer()
    {
      if (!_M_buf && _M_buf_size_opt)
	{
	  _M_buf_size = _M_buf_size_opt;

	  // Allocate internal buffer.
	  try { _M_buf = new char_type[_M_buf_size]; }
	  catch(...) 
	    {
	      delete [] _M_buf;
	      __throw_exception_again;
	    }
	  _M_buf_allocated = true;
	}
    }

  // Both close and setbuf need to deallocate internal buffers, if it exists.
  template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    _M_destroy_internal_buffer()
    {
      if (_M_buf_allocated)
	{
	  delete [] _M_buf;
	  _M_buf = NULL;
	  _M_buf_allocated = false;
	  this->setg(NULL, NULL, NULL);
	  this->setp(NULL, NULL);
	}
    }

 template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    _M_allocate_pback_buffer()
    {
      if (!_M_pback && _M_pback_size)
	{
	  // Allocate pback buffer.
	  try 
	    { _M_pback = new char_type[_M_pback_size]; }
	  catch(...) 
	    {
	      delete [] _M_pback;
	      __throw_exception_again;
	    }
	}
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::
    basic_filebuf() 
    : __streambuf_type(), _M_file(NULL), _M_state_cur(__state_type()), 
    _M_state_beg(__state_type()), _M_buf_allocated(false), 
    _M_last_overflowed(false)
    { }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::
    basic_filebuf(__c_file_type* __f, ios_base::openmode __mode, int_type __s)
    : __streambuf_type(),  _M_file(NULL), _M_state_cur(__state_type()), 
    _M_state_beg(__state_type()), _M_buf_allocated(false), 
    _M_last_overflowed(false)
    {
      _M_allocate_file();
      _M_file->sys_open(__f, __mode);
      if (this->is_open())
	{
	  _M_mode = __mode;
	  if (__s)
	    {
	      _M_buf_size_opt = __s;
	      _M_allocate_internal_buffer();
	      _M_set_indeterminate();
	    }
	  _M_allocate_pback_buffer();
	}
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::__filebuf_type* 
    basic_filebuf<_CharT, _Traits>::
    open(const char* __s, ios_base::openmode __mode)
    {
      __filebuf_type *__ret = NULL;
      if (!this->is_open())
	{
	  _M_allocate_file();
	  _M_file->open(__s, __mode);
	  if (this->is_open())
	    {
	      _M_allocate_internal_buffer();
	      _M_allocate_pback_buffer();
	      _M_mode = __mode;
	      
	      // For time being, set both (in/out) sets  of pointers.
	      _M_set_indeterminate();
	      if (__mode & ios_base::ate
		  && this->seekoff(0, ios_base::end, __mode) < 0)
		this->close();
	      __ret = this;
	    }
	}
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::__filebuf_type* 
    basic_filebuf<_CharT, _Traits>::
    close()
    {
      __filebuf_type *__ret = NULL;
      if (this->is_open())
	{
	  bool __testput = _M_out_cur && _M_out_beg < _M_out_end;
	  if (__testput)
	    _M_really_overflow(traits_type::eof());

	  // NB: Do this here so that re-opened filebufs will be cool...
	  _M_pback_destroy();

#if 0
	  // XXX not done
	  if (_M_last_overflowed)
	    {
	      _M_output_unshift();
	      _M_really_overflow(traits_type::eof());
	    }
#endif

	  _M_mode = ios_base::openmode(0);
	  _M_destroy_internal_buffer();

	  if (_M_pback)
	    {
	      delete [] _M_pback;
	      _M_pback = NULL;
	    }
	  __ret = this;
	}

      // Can actually allocate this file as part of an open and never
      // have it be opened.....
      if (_M_file)
	{
	  delete _M_file;
	  _M_file = NULL;
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
      bool __testin = _M_mode & ios_base::in;

      if (__testin)
	{
	  bool __testeof = false;
	  if (_M_in_cur >= _M_in_end)
	    __testeof = this->underflow() == traits_type::eof();
	  if (!__testeof)
	    __ret = _M_in_end - _M_in_cur;
	}
      _M_last_overflowed = false;	
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    underflow()
    {
      int_type __ret = traits_type::eof();
      bool __testin = _M_mode & ios_base::in;
      bool __testout = _M_mode & ios_base::out;

      // XXX Should re-enable codecvt bits disabled after 2.90.8.
      if (__testin)
	{
	  // Check for pback madness, and if so swich back to the
	  // normal buffers and jet outta here before expensive
	  // fileops happen...
	  if (_M_pback_init)
	    {
	      _M_pback_destroy();
	      if (_M_in_cur < _M_in_end)
		return traits_type::to_int_type(*_M_in_cur);
	    }

	  bool __testget = _M_in_cur && _M_in_beg < _M_in_cur;
	  bool __testinit = _M_is_indeterminate();
	  // Sync internal and external buffers.
	  // NB: __testget -> __testput as _M_buf_unified here.
	  if (__testget)
	    {
	      if (__testout)
		_M_really_overflow();
#if _GLIBCPP_AVOID_FSEEK
	      else if ((_M_in_cur - _M_in_beg) == 1)
		_M_file->sys_getc();
#endif
	      else 
		_M_file->seekoff(_M_in_cur - _M_in_beg, 
				 ios_base::cur, ios_base::in);
	    }

	  if (__testinit || __testget)
	    {
	      // Assume buffered case, need to refill internal buffers.
	      streamsize __size = _M_file->xsgetn(_M_in_beg, _M_buf_size);
	      if (0 < __size)
		{
		  _M_set_determinate(__size);
		  if (__testout)
		    _M_out_cur = _M_in_cur;
		  __ret = traits_type::to_int_type(*_M_in_cur);
#if _GLIBCPP_AVOID_FSEEK
		  if (__size == 1)
		    _M_file->sys_ungetc(*_M_in_cur);
		  else
		    {
#endif
		  streamoff __p = _M_file->seekoff(0 - __size, ios_base::cur, 
						   ios_base::in);
		  if (__p == -1)
		    {
		      // XXX Something is wrong, do error checking.
		    }
#if _GLIBCPP_AVOID_FSEEK
		    }
#endif
		}	   
	    }
	}
      _M_last_overflowed = false;	
      return __ret;
    }
  
  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    pbackfail(int_type __i)
    {
      int_type __ret = traits_type::eof();
      bool __testin = _M_mode & ios_base::in;

      if (__testin)
	{
	  bool __testpb = _M_in_beg < _M_in_cur;
	  char_type __c = traits_type::to_char_type(__i);
	  bool __testeof = traits_type::eq_int_type(__i, __ret);

	  if (__testpb)
	    {
	      bool __testout = _M_mode & ios_base::out;
	      bool __testeq = traits_type::eq(__c, this->gptr()[-1]);

	      // Try to put back __c into input sequence in one of three ways.
	      // Order these tests done in is unspecified by the standard.
	      if (!__testeof && __testeq)
		{
		  --_M_in_cur;
		  if (__testout)
		    --_M_out_cur;
		  __ret = __i;
		}
	      else if (__testeof)
		{
		  --_M_in_cur;
		  if (__testout)
		    --_M_out_cur;
		  __ret = traits_type::not_eof(__i);
		}
	      else if (!__testeof)
		{
		  --_M_in_cur;
		  if (__testout)
		    --_M_out_cur;
		  _M_pback_create();
		  *_M_in_cur = __c; 
		  __ret = __i;
		}
	    }
	  else
	    {	 
 	      // At the beginning of the buffer, need to make a
	      // putback position available.
	      this->seekoff(-1, ios_base::cur);
	      this->underflow();
 	      if (!__testeof)
 		{
		  if (!traits_type::eq(__c, *_M_in_cur))
		    {
		      _M_pback_create();
		      *_M_in_cur = __c;
		    }
 		  __ret = __i;
 		}
 	      else
 		__ret = traits_type::not_eof(__i);
 	    }
	}
      _M_last_overflowed = false;	
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    overflow(int_type __c)
    {
      int_type __ret = traits_type::eof();
      bool __testput = _M_out_cur && _M_out_cur < _M_buf + _M_buf_size;
      bool __testout = _M_mode & ios_base::out;
      
      if (__testout)
	{
	  if (__testput)
	    {
	      *_M_out_cur = traits_type::to_char_type(__c);
	      _M_out_cur_move(1);
	      __ret = traits_type::not_eof(__c);
	    }
	  else 
	    __ret = this->_M_really_overflow(__c);
	}

      _M_last_overflowed = false;    // Set in _M_really_overflow, below.
      return __ret;
    }
  
  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    _M_really_overflow(int_type __c)
    {
      int_type __ret = traits_type::eof();
      bool __testput = _M_out_cur && _M_out_beg < _M_out_end;
      bool __testunbuffered = _M_file && !_M_buf_size;

      if (__testput || __testunbuffered)
	{
#if 1
	  int __plen = _M_out_end - _M_out_beg;
	  streamsize __len = 0;

	  if (__plen)
	    __len = _M_file->xsputn(_M_out_beg, __plen);

	  if (__c !=traits_type::eof())
	    {
 	      char_type __pending = traits_type::to_char_type(__c);
 	      __len += _M_file->xsputn(&__pending, 1);
  	      ++__plen;
	    }

	  // NB: Need this so that external byte sequence reflects
	  // internal buffer.
	  _M_file->sync();
	  if (__len == __plen)
	    {
	      _M_set_indeterminate();
	      __ret = traits_type::not_eof(__c);
	    }
#else
	  // Part one: Allocate temporary conversion buffer on
	  // stack. Convert internal buffer plus __c (ie,
	  // "pending sequence") to temporary conversion buffer.
	  int __plen = _M_out_end - _M_out_beg;
	  char_type* __pbuf = static_cast<char_type*>(__builtin_alloca(sizeof(char_type) * __plen + 1));
	  traits_type::copy(__pbuf, this->pbase(), __plen);
	  if (!__testeof)
	    {
	      __pbuf[__plen] = traits_type::to_char_type(__c);
	      ++__plen;
	    }

	  char_type* __pend;
	  char* __conv_buf = static_cast<char*>(__builtin_alloca(__plen));
	  char* __conv_end;
	  _M_state_beg = _M_state_cur;

	  __res_type __r = _M_fcvt->out(_M_state_cur, 
					__pbuf, __pbuf + __plen,
					const_cast<const char_type*&>(__pend),
					__conv_buf, __conv_buf + __plen,
					__conv_end);
	  
	  // Part two: (Re)spill converted "pending sequence"
	  // contents (now in temporary conversion buffer) to
	  // external buffer (_M_file->_IO_*) using
	  // _M_file->sys_write(), and do error (minimal) checking.
	  if (__r != codecvt_base::error)
	    {
	      streamsize __len = _M_file->xsputn(__conv_buf, __plen);
	      // NB: Need this so that external byte sequence reflects
	      // internal buffer.
	      _M_file->sync();
	      if (__len == __plen)
		{
		  _M_set_indeterminate();
		  __ret = traits_type::not_eof(__c);
		}
	    }
#endif
	}	      
      _M_last_overflowed = true;	
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::__streambuf_type* 
    basic_filebuf<_CharT, _Traits>::
    setbuf(char_type* __s, streamsize __n)
    {
      if (!this->is_open() && __s == 0 && __n == 0)
	_M_buf_size_opt = 0;
      else if (__s && __n)
	{
	  // This is implementation-defined behavior, and assumes
	  // that an external char_type array of length (__s + __n)
	  // exists and has been pre-allocated. If this is not the
	  // case, things will quickly blow up.
	  // Step 1: Destroy the current internal array.
	  _M_destroy_internal_buffer();
	  
	  // Step 2: Use the external array.
	  _M_buf = __s;
	  _M_buf_size_opt = _M_buf_size = __n;
	  _M_set_indeterminate();
	  
	// Step 3: Make sure a pback buffer is allocated.
	  _M_allocate_pback_buffer();
	}
      _M_last_overflowed = false;	
      return this; 
    }
  
  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::pos_type
    basic_filebuf<_CharT, _Traits>::
    seekoff(off_type __off, ios_base::seekdir __way, ios_base::openmode __mode)
    {
      pos_type __ret =  pos_type(off_type(-1)); 
      bool __testopen = this->is_open();
      bool __testin = __mode & ios_base::in && _M_mode & ios_base::in;
      bool __testout = __mode & ios_base::out && _M_mode & ios_base::out;

      // Should probably do has_facet checks here.
      int __width = use_facet<__codecvt_type>(_M_buf_locale).encoding();
      if (__width < 0)
	__width = 0;
      bool __testfail = __off != 0  && __width <= 0;
      
      if (__testopen && !__testfail && (__testin || __testout))
	{
	  // Ditch any pback buffers to avoid confusion.
	  _M_pback_destroy();

	  if (__way != ios_base::cur || __off != 0)
	    { 
	      off_type __computed_off = __width * __off;
	      
	      bool __testget = _M_in_cur && _M_in_beg < _M_in_end;
	      bool __testput = _M_out_cur && _M_out_beg < _M_out_end;
	      // Sync the internal and external streams.
	      // out
	      if (__testput || _M_last_overflowed)
		{
		  // Part one: update the output sequence.
		  this->sync();
		  // Part two: output unshift sequence.
		  _M_output_unshift();
		}
	      //in
	      // NB: underflow() rewinds the external buffer.
	      else if (__testget && __way == ios_base::cur)
		__computed_off += _M_in_cur - _M_in_beg;
	  
	      __ret = _M_file->seekoff(__computed_off, __way, __mode);
	      _M_set_indeterminate();
	    }
	  // NB: Need to do this in case _M_file in indeterminate
	  // state, ie _M_file->_offset == -1
	  else
	    {
	      __ret = _M_file->seekoff(__off, ios_base::cur, __mode);
	      __ret += max(_M_out_cur, _M_in_cur) - _M_buf;
	    }
	}
      _M_last_overflowed = false;	
      return __ret;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::pos_type
    basic_filebuf<_CharT, _Traits>::
    seekpos(pos_type __pos, ios_base::openmode __mode)
    {
      pos_type __ret;
      off_type __off = __pos;

      __ret = this->seekoff(__off, ios_base::beg, __mode); 

      _M_last_overflowed = false;	
      return __ret;
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
      bool __testbeg = gptr() == eback() && pptr() == pbase();

      if (__testbeg && _M_buf_locale != __loc)
	{
	  _M_buf_locale = __loc;
	  _M_buf_locale_init = true;
	}

      // NB this may require the reconversion of previously
      // converted chars. This in turn may cause the reconstruction
      // of the original file. YIKES!!
      // XXX The part in the above comment is not done.
      _M_last_overflowed = false;	
    }
  
} // namespace std

#endif // _CPP_BITS_FSTREAM_TCC


