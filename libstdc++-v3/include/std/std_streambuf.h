// Stream buffer classes -*- C++ -*-

// Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
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
// ISO C++ 14882: 27.5  Stream buffers
//

/** @file streambuf
 *  This is a Standard C++ Library header.  You should @c #include this header
 *  in your programs, rather than any of the "st[dl]_*.h" implementation files.
 */

#ifndef _CPP_STREAMBUF
#define _CPP_STREAMBUF	1

#pragma GCC system_header

#include <bits/c++config.h>
#include <iosfwd>
#include <cstdio> 	// For SEEK_SET, SEEK_CUR, SEEK_END
#include <bits/localefwd.h>
#include <bits/ios_base.h>

namespace std
{
  template<typename _CharT, typename _Traits>
    streamsize
    __copy_streambufs(basic_ios<_CharT, _Traits>& _ios,
		      basic_streambuf<_CharT, _Traits>* __sbin,
		      basic_streambuf<_CharT, _Traits>* __sbout);
  
  // 27.5.2 Template class basic_streambuf<_CharT, _Traits>
  template<typename _CharT, typename _Traits>
    class basic_streambuf 
    {
    public:
      // Types:
      typedef _CharT 					char_type;
      typedef _Traits 					traits_type;
      typedef typename traits_type::int_type 		int_type;
      typedef typename traits_type::pos_type 		pos_type;
      typedef typename traits_type::off_type 		off_type;

      // Non-standard Types:
      typedef ctype<char_type>           		__ctype_type;
      typedef basic_streambuf<char_type, traits_type>  	__streambuf_type;
      typedef typename traits_type::state_type 		__state_type;
      
      friend class basic_ios<char_type, traits_type>;
      friend class basic_istream<char_type, traits_type>;
      friend class basic_ostream<char_type, traits_type>;
      friend class istreambuf_iterator<char_type, traits_type>;
      friend class ostreambuf_iterator<char_type, traits_type>;

      friend streamsize
      __copy_streambufs<>(basic_ios<char_type, traits_type>& __ios,
			  __streambuf_type* __sbin,__streambuf_type* __sbout);
      
    protected:
      // Pointer to the beginning of internally-allocated
      // space. Filebuf manually allocates/deallocates this, whereas
      // stringstreams attempt to use the built-in intelligence of the
      // string class. If you are managing memory, set this. If not,
      // leave it NULL.
      char_type*		_M_buf; 	

      // Actual size of allocated internal buffer, in bytes.
      size_t			_M_buf_size;

      // Optimal or preferred size of internal buffer, in bytes.
      size_t			_M_buf_size_opt;

      // True iff _M_in_* and _M_out_* buffers should always point to
      // the same place.  True for fstreams, false for sstreams.
      bool 			_M_buf_unified;	

      // This is based on _IO_FILE, just reordered to be more
      // consistent, and is intended to be the most minimal abstraction
      // for an internal buffer.
      // get == input == read
      // put == output == write
      char_type* 		_M_in_beg;  	// Start of get area. 
      char_type* 		_M_in_cur;	// Current read area. 
      char_type* 		_M_in_end;	// End of get area. 
      char_type* 		_M_out_beg; 	// Start of put area. 
      char_type* 		_M_out_cur;  	// Current put area. 
      char_type* 		_M_out_end;  	// End of put area. 

      // Place to stash in || out || in | out settings for current streambuf.
      ios_base::openmode 	_M_mode;	

      // Current locale setting.
      locale 			_M_buf_locale;	

      // True iff locale is initialized.
      bool 			_M_buf_locale_init;

      // Necessary bits for putback buffer management. Only used in
      // the basic_filebuf class, as necessary for the standard
      // requirements. The only basic_streambuf member function that
      // needs access to these data members is in_avail...
      // NB: pbacks of over one character are not currently supported.
      static const size_t   	_S_pback_size = 1; 
      char_type			_M_pback[_S_pback_size]; 
      char_type*		_M_pback_cur_save;
      char_type*		_M_pback_end_save;
      bool			_M_pback_init; 

      // Yet unused.
      fpos<__state_type>	_M_pos;

      // Initializes pback buffers, and moves normal buffers to safety.
      // Assumptions:
      // _M_in_cur has already been moved back
      void
      _M_pback_create()
      {
	if (!_M_pback_init)
	  {
	    size_t __dist = _M_in_end - _M_in_cur;
	    size_t __len = min(_S_pback_size, __dist);
	    traits_type::copy(_M_pback, _M_in_cur, __len);
	    _M_pback_cur_save = _M_in_cur;
	    _M_pback_end_save = _M_in_end;
	    this->setg(_M_pback, _M_pback, _M_pback + __len);
	    _M_pback_init = true;
	  }
      }

      // Deactivates pback buffer contents, and restores normal buffer.
      // Assumptions:
      // The pback buffer has only moved forward.
      void
      _M_pback_destroy()
      {
	if (_M_pback_init)
	  {
	    // Length _M_in_cur moved in the pback buffer.
	    size_t __off_cur = _M_in_cur - _M_pback;
	    
	    // For in | out buffers, the end can be pushed back...
	    size_t __off_end = 0;
	    size_t __pback_len = _M_in_end - _M_pback;
	    size_t __save_len = _M_pback_end_save - _M_buf;
	    if (__pback_len > __save_len)
	      __off_end = __pback_len - __save_len;

	    this->setg(_M_buf, _M_pback_cur_save + __off_cur, 
		       _M_pback_end_save + __off_end);
	    _M_pback_cur_save = NULL;
	    _M_pback_end_save = NULL;
	    _M_pback_init = false;
	  }
      }

      // Correctly sets the _M_in_cur pointer, and bumps the
      // _M_out_cur pointer as well if necessary.
      void 
      _M_in_cur_move(off_type __n) // argument needs to be +-
      {
	bool __testout = _M_out_cur;
	_M_in_cur += __n;
	if (__testout && _M_buf_unified)
	  _M_out_cur += __n;
      }

      // Correctly sets the _M_out_cur pointer, and bumps the
      // appropriate _M_*_end pointers as well. Necessary for the
      // un-tied stringbufs, in in|out mode.
      // Invariant:
      // __n + _M_out_[cur, end] <= _M_buf + _M_buf_size
      // Assuming all _M_*_[beg, cur, end] pointers are operating on
      // the same range:
      // _M_buf <= _M_*_ <= _M_buf + _M_buf_size
      void 
      _M_out_cur_move(off_type __n) // argument needs to be +-
      {
	bool __testin = _M_in_cur;

	_M_out_cur += __n;
	if (__testin && _M_buf_unified)
	  _M_in_cur += __n;
	if (_M_out_cur > _M_out_end)
	  {
	    _M_out_end = _M_out_cur;
	    // NB: in | out buffers drag the _M_in_end pointer along...
	    if (__testin)
	      _M_in_end += __n;
	  }
      }

      // Return the size of the output buffer.  This depends on the
      // buffer in use: allocated buffers have a stored size in
      // _M_buf_size and setbuf() buffers don't.
      off_type
      _M_out_buf_size()
      {
	off_type __ret = 0;
	if (_M_out_cur)
	  {
	    // Using allocated buffer.
	    if (_M_out_beg == _M_buf)
	      __ret = _M_out_beg + _M_buf_size - _M_out_cur;
	    // Using non-allocated buffer.
	    else
	      __ret = _M_out_end - _M_out_cur;
	  }
	return __ret;
      }

  public:
      virtual 
      ~basic_streambuf() 
      {
	_M_buf_unified = false;
	_M_buf_size = 0;
	_M_buf_size_opt = 0;
	_M_mode = ios_base::openmode(0);
	_M_buf_locale_init = false;
      }

      // Locales:
      locale 
      pubimbue(const locale &__loc)
      {
	locale __tmp(this->getloc());
	this->imbue(__loc);
	return __tmp;
      }

      locale   
      getloc() const
      {
	if (_M_buf_locale_init)
	  return _M_buf_locale; 
	else 
	  return locale();
      } 

      // Buffer and positioning:
      __streambuf_type* 
      pubsetbuf(char_type* __s, streamsize __n) 
      { return this->setbuf(__s, __n); }

      pos_type 
      pubseekoff(off_type __off, ios_base::seekdir __way, 
		 ios_base::openmode __mode = ios_base::in | ios_base::out)
      { return this->seekoff(__off, __way, __mode); }

      pos_type 
      pubseekpos(pos_type __sp,
		 ios_base::openmode __mode = ios_base::in | ios_base::out)
      { return this->seekpos(__sp, __mode); }

      int 
      pubsync() { return this->sync(); }

      // Get and put areas:
      // Get area:
      streamsize 
      in_avail() 
      { 
	streamsize __ret;
	if (_M_in_cur && _M_in_cur < _M_in_end)
	  {
	    if (_M_pback_init)
	      {
		size_t __save_len =  _M_pback_end_save - _M_pback_cur_save;
		size_t __pback_len = _M_in_cur - _M_pback;
		__ret = __save_len - __pback_len;
	      }
	    else
	      __ret = this->egptr() - this->gptr();
	  }
	else
	  __ret = this->showmanyc();
	return __ret;
      }

      int_type 
      snextc()
      {
	int_type __eof = traits_type::eof();
	return (traits_type::eq_int_type(this->sbumpc(), __eof) 
		? __eof : this->sgetc());
      }

      int_type 
      sbumpc();

      int_type 
      sgetc()
      {
	int_type __ret;
	if (_M_in_cur && _M_in_cur < _M_in_end)
	  __ret = traits_type::to_int_type(*(this->gptr()));
	else 
	  __ret = this->underflow();
	return __ret;
      }

      streamsize 
      sgetn(char_type* __s, streamsize __n)
      { return this->xsgetn(__s, __n); }

      // Putback:
      int_type 
      sputbackc(char_type __c);

      int_type 
      sungetc();

      // Put area:
      int_type 
      sputc(char_type __c);

      streamsize 
      sputn(const char_type* __s, streamsize __n)
      { return this->xsputn(__s, __n); }

    protected:
      basic_streambuf()
      : _M_buf(NULL), _M_buf_size(0), _M_buf_size_opt(BUFSIZ), 
      _M_buf_unified(false), _M_in_beg(0), _M_in_cur(0), _M_in_end(0), 
      _M_out_beg(0), _M_out_cur(0), _M_out_end(0), 
      _M_mode(ios_base::openmode(0)), _M_buf_locale(locale()), 
      _M_buf_locale_init(false), _M_pback_cur_save(0), _M_pback_end_save(0), 
      _M_pback_init(false)
      { }

      // Get area:
      char_type* 
      eback() const { return _M_in_beg; }

      char_type* 
      gptr()  const { return _M_in_cur;  }

      char_type* 
      egptr() const { return _M_in_end; }

      void 
      gbump(int __n) { _M_in_cur += __n; }

      void 
      setg(char_type* __gbeg, char_type* __gnext, char_type* __gend)
      {
	_M_in_beg = __gbeg;
	_M_in_cur = __gnext;
	_M_in_end = __gend;
	if (!(_M_mode & ios_base::in) && __gbeg && __gnext && __gend)
	  _M_mode = _M_mode | ios_base::in;
      }

      // Put area:
      char_type* 
      pbase() const { return _M_out_beg; }

      char_type* 
      pptr() const { return _M_out_cur; }

      char_type* 
      epptr() const { return _M_out_end; }

      void 
      pbump(int __n) { _M_out_cur += __n; }

      void 
      setp(char_type* __pbeg, char_type* __pend)
      { 
	_M_out_beg = _M_out_cur = __pbeg; 
	_M_out_end = __pend; 
	if (!(_M_mode & ios_base::out) && __pbeg && __pend)
	  _M_mode = _M_mode | ios_base::out;
      }

      // Virtual functions:
      // Locales:
      virtual void 
      imbue(const locale& __loc) 
      { 
	_M_buf_locale_init = true;
	if (_M_buf_locale != __loc)
	  _M_buf_locale = __loc;
      }

      // Buffer management and positioning:
      virtual basic_streambuf<char_type,_Traits>* 
      setbuf(char_type*, streamsize)
      {	return this; }
      
      virtual pos_type 
      seekoff(off_type, ios_base::seekdir,
	      ios_base::openmode /*__mode*/ = ios_base::in | ios_base::out)
      { return pos_type(off_type(-1)); } 

      virtual pos_type 
      seekpos(pos_type, 
	      ios_base::openmode /*__mode*/ = ios_base::in | ios_base::out)
      { return pos_type(off_type(-1)); } 

      virtual int 
      sync() { return 0; }

      // Get area:
      virtual streamsize 
      showmanyc() { return 0; }

      virtual streamsize 
      xsgetn(char_type* __s, streamsize __n);

      virtual int_type 
      underflow()
      { return traits_type::eof(); }

      virtual int_type 
      uflow() 
      {
	int_type __ret = traits_type::eof();
	bool __testeof = traits_type::eq_int_type(this->underflow(), __ret);
	bool __testpending = _M_in_cur && _M_in_cur < _M_in_end;
	if (!__testeof && __testpending)
	  {
	    __ret = traits_type::to_int_type(*_M_in_cur);
	    ++_M_in_cur;
	    if (_M_buf_unified && _M_mode & ios_base::out)
	      ++_M_out_cur;
	  }
	return __ret;    
      }

      // Putback:
      virtual int_type 
      pbackfail(int_type /* __c */  = traits_type::eof())
      { return traits_type::eof(); }

      // Put area:
      virtual streamsize 
      xsputn(const char_type* __s, streamsize __n);

      virtual int_type 
      overflow(int_type /* __c */ = traits_type::eof())
      { return traits_type::eof(); }

#ifdef _GLIBCPP_DEPRECATED
    public:
      void 
      stossc() 
      {
	if (_M_in_cur < _M_in_end) 
	  ++_M_in_cur;
	else 
	  this->uflow();
      }
#endif

#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
    // Side effect of DR 50. 
    private:
      basic_streambuf(const __streambuf_type&) { }; 

      __streambuf_type& 
      operator=(const __streambuf_type&) { return *this; };
#endif
    };
} // namespace std

#ifdef _GLIBCPP_NO_TEMPLATE_EXPORT
# define export
#endif
#ifdef  _GLIBCPP_FULLY_COMPLIANT_HEADERS
#include <bits/streambuf.tcc>
#endif

#endif	
