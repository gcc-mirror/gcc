// File based streams -*- C++ -*-

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
// ISO C++ 14882: 27.8  File-based streams
//

#ifndef _CPP_BITS_FSTREAM_TCC
#define _CPP_BITS_FSTREAM_TCC 1

namespace std
{
  template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    _M_init_filebuf(void)
    {
      _M_buf_unified = true; // Tie input to output for basic_filebuf.
      _M_buf_size = static_cast<int_type>(BUFSIZ * sizeof(char_type)); 
      try {
	_M_file = new __file_type(&_M_lock);
      }
      catch(...) {
	delete _M_file;
	throw;
      }
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::
    basic_filebuf() 
    : __streambuf_type(), _M_file(NULL), _M_last_overflowed(false), 
    _M_state_cur(), _M_state_beg() 
    { _M_fcvt = &use_facet<__codecvt_type>(this->getloc()); }


  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::
    basic_filebuf(int __fd, const char* /*__name*/, ios_base::openmode __mode)
    : __streambuf_type(), _M_last_overflowed(false),
    _M_state_cur(), _M_state_beg()
    {
      _M_fcvt = &use_facet<__codecvt_type>(this->getloc());
      _M_init_filebuf();
      _M_file->sys_open(__fd, __mode);
      if (this->is_open() && _M_buf_size)
	{
	  _M_mode = __mode;
	  // XXX So that istream::getc() will only need to get 1 char,
	  // as opposed to BUF_SIZE.
	  if (__fd == 0)
	    _M_buf_size = 1;

	  try {
	    _M_buf = new char_type[_M_buf_size];
	  }
	  catch(...) {
	    delete [] _M_buf;
	    throw;
	  }

	  this->_M_set_indeterminate();
	}
   }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::__filebuf_type* 
    basic_filebuf<_CharT, _Traits>::
    open(const char* __s, ios_base::openmode __mode)
    {
      __filebuf_type *__retval = NULL;
      if (!this->is_open())
	{
	  _M_init_filebuf();
	  _M_file->open(__s, __mode);
	  if (this->is_open() && _M_buf_size)
	    {
	      _M_mode = __mode;

	      try {
		_M_buf = new char_type[_M_buf_size];
	      }
	      catch(...) {
		delete [] _M_buf;
		throw;
	      }
	      
	      // For time being, set both (in/out) sets  of pointers.
	      _M_set_indeterminate();
	      if (__mode & ios_base::ate
		  && this->seekoff(0, ios_base::end, __mode) < 0)
		this->close();
	      __retval = this;
	    }
	}
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::__filebuf_type* 
    basic_filebuf<_CharT, _Traits>::
    close()
    {
      __filebuf_type *__retval = NULL;
      if (this->is_open())
	{
	  bool __testput = _M_out_cur && _M_out_beg < _M_out_end;
	  if (__testput)
	    _M_really_overflow(traits_type::eof());
	  
#if 0
	  // XXX not done
	  if (_M_last_overflowed)
	    {
	      _M_output_unshift();
	      _M_really_overflow(traits_type::eof());
	    }
#endif
	  
	  if (_M_file->close())
	    {
	      _M_mode = ios_base::openmode(0);
	      if (_M_buf_size)
		delete [] _M_buf;
	      _M_buf = NULL;
	      this->setg(NULL, NULL, NULL);
	      this->setp(NULL, NULL);
	      __retval = this;
	    }
	}
      _M_last_overflowed = false;	
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    streamsize 
    basic_filebuf<_CharT, _Traits>::
    showmanyc()
    {
      streamsize __retval = -1;
      bool __testin = _M_mode & ios_base::in;

      if (__testin)
	{
	  bool __testeof = false;
	  if (_M_in_cur >= _M_in_end)
	    __testeof = this->underflow() == traits_type::eof();
	  if (!__testeof)
	    __retval = (_M_in_end - _M_in_cur) / sizeof(char_type);
	}
      _M_last_overflowed = false;	
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    underflow()
    {
      int_type __retval = traits_type::eof();
      bool __testget = _M_in_cur && _M_in_beg < _M_in_cur;
      bool __testinit = _M_is_indeterminate();
      bool __testout = _M_mode & ios_base::out;
      bool __testin = _M_mode & ios_base::in;
      
      if (__testin)
	{
	  // Sync internal and external buffers.
	  // NB: __testget -> __testput as _M_buf_unified here.
	  if (__testget)
	    {
	      if (__testout)
		_M_really_overflow();
	      else 
		_M_file->seekoff(_M_in_cur - _M_in_beg, 
				 ios_base::cur, ios_base::in);
	    }

	  if (__testinit || __testget)
	    {
	      // Part one: (Re)fill external buf (_M_file->_IO_*) from
	      // external byte sequence (whatever physical byte sink or
	      // FILE actually is.)
	      char __conv_buf[_M_buf_size];
	      streamsize __size = _M_file->xsgetn(__conv_buf, _M_buf_size);
	      
	      // Part two: (Re)fill internal buf contents from external buf.
	      if (0 < __size)
		{
		  _M_set_determinate(__size);
		  
		  char* __conv_cur = __conv_buf;
		  _M_state_beg = _M_state_cur;
		  __res_type __r = _M_fcvt->in(_M_state_cur, 
					       __conv_buf,
					       __conv_buf + __size,
					 const_cast<const char*&>(__conv_cur), 
					      _M_in_beg, _M_in_end, _M_in_cur);
	      
		  if (__r == codecvt_base::partial)
		    {
		      // XXX Retry with larger _M_buf size.
		    }
		  
		  // Set pointers to internal and external buffers
		  // correctly. . .
		  if (__r != codecvt_base::error)
		    {
		      if (__testout)
			_M_out_cur = _M_in_cur;
		      __retval = traits_type::to_int_type(*_M_in_cur);
		    }

		  // Part three: Sync the current internal buffer
		  // position with the (now overshot) external buffer
		  // position.  
		  streamoff __p = _M_file->seekoff(0 - __size, ios_base::cur, 
						  ios_base::in);
		  if (__p == -1)
		    {
		      // XXX Something is wrong, do error checking.
		    }
		}
	    }	      
	}
      _M_last_overflowed = false;	
      return __retval;
    }
  
  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    pbackfail(int_type __i)
    {
      int_type __retval = traits_type::eof();
      char_type __c = traits_type::to_char_type(__i);
      bool __testeof = traits_type::eq_int_type(__i, traits_type::eof());
      bool __testout = _M_mode & ios_base::out;
      bool __testin = _M_mode & ios_base::in;

      if (__testin)
	{
	  if (!_M_is_indeterminate())	  
	    {
	      bool __testpb = _M_in_beg < _M_in_cur;
	      bool __testeq = traits_type::eq(__c, this->gptr()[-1]);

	      // Try to put back __c into input sequence in one of three ways.
	      // Order these tests done in is unspecified by the standard.
	      if (!__testeof && __testpb && __testeq)
		{
		  --_M_in_cur;
		  if (__testout)
		    --_M_out_cur;
		  __retval = __i;
		}
	      else if (!__testeof && __testpb && __testout)
		{
		  --_M_in_cur;
		  if (__testout)
		    --_M_out_cur;
		  *_M_in_cur = __c;
		  __retval = __i;
		}
	      else if (__testeof && __testpb)
		{
		  --_M_in_cur;
		  if (__testout)
		    --_M_out_cur;
		  __retval = traits_type::not_eof(__i);
		}
	    }
	  else
	    {
	      // Need to make a putback position available.
	      this->seekoff(-1, ios_base::cur);
	      this->underflow();
	      if (!__testeof)
		{
		  *_M_in_cur = __c;
		  __retval = __c;
		}
	      else
		__retval = traits_type::not_eof(__i);
	    }
	}
      _M_last_overflowed = false;	
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    overflow(int_type __c)
    {
      int_type __retval = traits_type::eof();
      bool __testpos = _M_out_cur && _M_out_cur >= _M_buf + _M_buf_size;
      bool __testout = _M_mode & ios_base::out;
      
      if (__testout)
	{
	  if (!__testpos)
	    {
	      *_M_out_cur = traits_type::to_char_type(__c);
	      _M_buf_bump(1);
	      __retval = traits_type::not_eof(__c);
	    }
	  else 
	    __retval = this->_M_really_overflow(__c);
	}

      _M_last_overflowed = false;    // Set in _M_really_overflow, below.
      return __retval;
    }
  
  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::int_type 
    basic_filebuf<_CharT, _Traits>::
    _M_really_overflow(int_type __c)
    {
      int_type __retval = traits_type::eof();
      bool __testput = _M_out_cur && _M_out_beg < _M_out_end;
      bool __testeof = traits_type::eq_int_type(__c, traits_type::eof());
      
      if (__testput)
	{
	  // Part one: Allocate temporary conversion buffer on
	  // stack. Convert internal buffer plus __c (ie,
	  // "pending sequence") to temporary conversion buffer.
	  int __plen = _M_out_end - _M_out_beg;
	  char_type __pbuf[__plen + sizeof(char_type)];	      
	  traits_type::copy(__pbuf, this->pbase(), __plen);
	  if (!__testeof)
	    {
	      __pbuf[__plen] = traits_type::to_char_type(__c);
	      ++__plen;
	    }

	  char_type* __pend;
	  char __conv_buf[__plen];
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
		  __retval = traits_type::not_eof(__c);
		}
	    }
	}	      
      _M_last_overflowed = true;	
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::pos_type
    basic_filebuf<_CharT, _Traits>::
    seekoff(off_type __off, ios_base::seekdir __way, ios_base::openmode __mode)
    {
      pos_type __retval =  pos_type(off_type(-1)); 
      bool __testopen = this->is_open();
      bool __testin = __mode & ios_base::in && _M_mode & ios_base::in;
      bool __testout = __mode & ios_base::out && _M_mode & ios_base::out;
      int __width = _M_fcvt->encoding();
      if (__width < 0)
	__width = 0;
      bool __testfail = __off != 0  && __width <= 0;
      
      if (__testopen && !__testfail && (__testin || __testout))
	{
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
	  
	      __retval = _M_file->seekoff(__computed_off, __way, __mode);
	      _M_set_indeterminate();
	    }
	  // NB: Need to do this in case _M_file in indeterminate
	  // state, ie _M_file->_offset == -1
	  else
	    {
	      __retval = _M_file->seekoff(__off, ios_base::cur, __mode);
	      __retval += max(_M_out_cur, _M_in_cur) - _M_buf;
	    }
	}
      _M_last_overflowed = false;	
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    basic_filebuf<_CharT, _Traits>::pos_type
    basic_filebuf<_CharT, _Traits>::
    seekpos(pos_type __pos, ios_base::openmode __mode)
    {
      pos_type __retval;
      off_type __off = __pos;

      __retval = this->seekoff(__off, ios_base::beg, __mode); 

      _M_last_overflowed = false;	
      return __retval;
    }

  template<typename _CharT, typename _Traits>
    void 
    basic_filebuf<_CharT, _Traits>::
    _M_output_unshift()
    {
#if 0
      // XXX Not complete, or correct.
      int __width = _M_fcvt->encoding();
      
      if (__width < 0)
	{
	  // Part one: call codecvt::unshift
	  int __unsft_len = 0;
	  char_type __unsft_buf[_M_buf_size];
	    char_type* __unsft_cur; // XXX Set to external buf.
	    _M_state_beg = _M_state_cur;
	    __res_type __r = _M_fcvt->unshift(_M_state_cur, 
					      __unsft_buf,
					      __unsft_buf + _M_buf_size,
					      __unsft_cur);
	    
	    // Note, for char_type == char, wchar_t unshift
	    // should store no charachers.
	    if (__r == codecvt_base::ok || __r == codecvt_base::noconv)
	      __unsft_len = __unsft_cur - __unsft_buf;
	    
	    // "Output the resulting sequence."
	    if (__unsft_len)
	      {
		int __plen = _M_out_cur - _M_out_beg;
		int __rlen = __plen  + __unsft_len;
		char_type __rbuf[__rlen];
		char_type* __rend;
		traits_type::copy(__rbuf, this->pbase(), __plen);
		traits_type::copy(__rbuf + __plen, __unsft_buf, 
				  __unsft_len);

		char __conv_buf[__rlen];
		char* __conv_end;
		
		_M_state_beg = _M_state_cur; // XXX Needed?
		__r = _M_fcvt->out(_M_state_cur, 
				  __rbuf, __rbuf + __rlen,
				  const_cast<const char_type*&>(__rend),
				  __conv_buf, 
				  __conv_buf + __rlen,
				  __conv_end);
		
		if (__r != codecvt_base::error)
		  {
		    streamsize __r = _M_file->xsputn(__conv_buf, __rlen);
		    if (__r == __rlen)
		      {
			_M_out_cur = _M_out_beg;
			if (_M_mode & ios_base::in)
			  _M_in_cur = _M_out_cur;
		      }
		    else
		      {
			// XXX Throw "wig out and die exception?"
		      }
		  }
	      }
	  }
#endif
    }

  template<typename _CharT, typename _Traits>
    void
    basic_filebuf<_CharT, _Traits>::
    imbue(const locale& __loc)
    {
      bool __testbeg = gptr() == eback() && pptr() == pbase();
      bool __teststate = _M_fcvt->encoding() == -1;
      
      _M_locale_set = true;
      if (__testbeg && !__teststate && _M_locale_buf != __loc)
	{
	  // XXX Will need to save these older values.
	  _M_locale_buf = __loc;
	  _M_fcvt = &use_facet<__codecvt_type>(_M_locale_buf);
	  // XXX Necessary?
	  _M_fctype_buf = &use_facet<__ctype_type>(_M_locale_buf); 
	}
      // NB this may require the reconversion of previously
      // converted chars. This in turn may cause the reconstruction
      // of the original file. YIKES!!
      // XXX The part in the above comment is not done.
      _M_last_overflowed = false;	
    }
  
} // namespace std

#endif // _CPP_BITS_FSTREAM_TCC










