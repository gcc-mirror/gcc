// Stream buffer classes -*- C++ -*-

// Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.
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

#ifndef _CPP_STREAMBUF
#define _CPP_STREAMBUF	1

#include <bits/c++config.h>
#include <bits/std_iosfwd.h>
#include <bits/std_cstdio.h> 	// For SEEK_SET, SEEK_CUR, SEEK_END
#include <bits/localefwd.h>
#include <bits/ios_base.h>

namespace std {

  template<typename _CharT, typename _Traits>
    static streamsize
    _S_copy_streambufs(basic_ios<_CharT, _Traits>& __ios,
		       basic_streambuf<_CharT, _Traits>* __sbin,
		       basic_streambuf<_CharT, _Traits>* __sbout);
  
  // 27.5.2 Template class basic_streambuf<_CharT, _Traits>
  template<typename _CharT, typename _Traits>
    class basic_streambuf 
    {
    public:
      // Types:
      typedef _CharT 					char_type;
      typedef typename _Traits::int_type 		int_type;
      typedef typename _Traits::pos_type 		pos_type;
      typedef typename _Traits::off_type 		off_type;
      typedef _Traits 					traits_type;

      // Non-standard Types:
      typedef ctype<_CharT>           			__ctype_type;
      typedef basic_streambuf<_CharT, _Traits> 		__streambuf_type;
      
      friend class basic_ios<char_type, traits_type>;
      friend class basic_istream<char_type, traits_type>;
      friend class basic_ostream<char_type, traits_type>;
      friend class istreambuf_iterator<char_type, traits_type>;
      friend class ostreambuf_iterator<char_type, traits_type>;

      friend streamsize
      _S_copy_streambufs<>(basic_ios<_CharT, _Traits>& __ios,
			 basic_streambuf<_CharT, _Traits>* __sbin,
			 basic_streambuf<_CharT, _Traits>* __sbout);
      
    protected:

      // Pointer to the beginning of internally-allocated
      // space. Filebuf manually allocates/deallocates this, whereas
      // stringstreams attempt to use the built-in intelligence of the
      // string class. If you are managing memory, set this. If not,
      // leave it NULL.
      char_type*		_M_buf; 	

      // Actual size of internal buffer, in bytes.
      int_type			_M_buf_size;

      // Optimal or preferred size of internal buffer, in bytes.
      int_type			_M_buf_size_opt;

      // True iff _M_in_* and _M_out_* buffers should always point to
      // the same place.  True for fstreams, false for sstreams.
      bool 			_M_buf_unified;	

       // This is based on _IO_FILE, just reordered to be more
      // consistent, and is intended to be the most minimal abstraction
      // for an internal buffer.
      // get == input == read
      // put == output == write
      char_type* 		_M_in_cur;	// Current read area. 
      char_type* 		_M_in_beg;  	// Start of get area. 
      char_type* 		_M_in_end;	// End of get area. 
      char_type* 		_M_out_cur;  	// Current put area. 
      char_type* 		_M_out_beg; 	// Start of put area. 
      char_type* 		_M_out_end;  	// End of put area. 

      // Place to stash in || out || in | out settings for current streambuf.
      ios_base::openmode 	_M_mode;	

      // Current locale setting.
      locale 			_M_locale_buf;	

      // True iff locale is initialized.
      bool 			_M_locale_set;

      // Cached use_facet<ctype>, which is based on the current locale info.
      const __ctype_type*	_M_fctype_buf;      

      // Correctly sets the _M_out_cur pointer, and bumps the
      // appropriate _M_*_end pointers as well. Necessary for the
      // un-tied stringbufs, in in|out mode.
      // Invariant:
      // __n + _M_out_[cur, end] <= _M_buf + _M_buf_size
      // Assuming all _M_*_[beg, cur, end] pointers are operating on
      // the same range:
      // _M_buf <= _M_*_ <= _M_buf + _M_buf_size
      void 
      _M_buf_bump(off_type __n) // argument needs to be +-
      {
	bool __testin = _M_mode & ios_base::in;
	bool __testout = _M_mode & ios_base::out;
	_M_out_cur += __n;
	if (_M_buf_unified && __testin)
	  _M_in_cur = _M_out_cur;
	if (_M_out_cur > _M_out_end)
	  {
	    _M_out_end = _M_out_cur;
	    if (__testin && __testout && _M_out_end > _M_in_end)
	      _M_in_end = _M_out_cur;
	  }
      }

      // These three functions are used to clarify internal buffer
      // maintance. After an overflow, or after a seekoff call that
      // started at beg or end, or possibly when the stream becomes
      // unbuffered, and a myrid other obscure corner cases, the
      // internal buffer does not truly reflect the contents of the
      // external buffer. At this point, for whatever reason, it is in
      // an indeterminate state.
      void
      _M_set_indeterminate(void)
      {
	if (_M_mode & ios_base::in)
	  this->setg(_M_buf, _M_buf, _M_buf);
	if (_M_mode & ios_base::out)
	  this->setp(_M_buf, _M_buf);
      }

      void
      _M_set_determinate(off_type __off)
      {
	bool __testin = _M_mode & ios_base::in;
	bool __testout = _M_mode & ios_base::out;
	if (__testin)
	  {
	    this->setg(_M_buf, _M_buf, _M_buf + __off);
	    if (!__testout)
	      _M_buf_size = static_cast<int_type>(__off);
	  }
	if (__testout)
	  this->setp(_M_buf, _M_buf + __off);

      }

      bool
      _M_is_indeterminate(void)
      { 
	bool __retval = false;
	if (_M_mode & ios_base::in)
	  __retval = _M_in_beg == _M_in_cur && _M_in_cur == _M_in_end;
	if (_M_mode & ios_base::out)
	  __retval = _M_out_beg == _M_out_cur && _M_out_cur == _M_out_end;
	return __retval;
      }

  public:
      virtual 
      ~basic_streambuf() 
      {
	_M_buf_unified = false;
	_M_buf_size = 0;
	_M_mode = ios_base::openmode(0);
	_M_fctype_buf = NULL;
	_M_locale_set = false;
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
	if (_M_locale_set)
	  return _M_locale_buf; 
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
	streamsize __retval;
	if (_M_in_cur && _M_in_cur < _M_in_end)
	  __retval = this->egptr() - this->gptr();
	else
	  __retval = this->showmanyc();
	return __retval;
      }

      int_type 
      snextc()
      {
	int_type __eof = traits_type::eof();
	return (this->sbumpc() == __eof ? __eof : this->sgetc()); 
      }

      int_type 
      sbumpc();

      int_type 
      sgetc()
      {
	int_type __retval;
	if (_M_in_cur && _M_in_cur < _M_in_end)
	  __retval = traits_type::to_int_type(*gptr());
	else 
	  __retval = this->underflow();
	return __retval;
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
      : _M_buf(NULL), _M_buf_size(0), 
	_M_buf_size_opt(static_cast<int_type>(BUFSIZ * sizeof(char_type))),
	_M_buf_unified(false), _M_in_cur(0), _M_in_beg(0), _M_in_end(0), 
	_M_out_cur(0), _M_out_beg(0), _M_out_end(0), 
	_M_mode(ios_base::openmode(0)), _M_locale_buf(locale()), 
	_M_locale_set(false) 

      { _M_fctype_buf =  &use_facet<__ctype_type>(this->getloc()); }

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
	// The output sequence is highly tied to _M_buf and
	// _M_buf_size in addition to the actual pointers into the
	// buffer. Because of this, (re)set _M_buf_size here, as
	// sputc/xsputn need _M_buf_size to be accurate. (The
	// corresponding input functions rely instead on _M_in_end.)
	_M_buf_size = max(_M_buf_size, static_cast<int_type>(__pend - __pbeg));
      }

      // Virtual functions:
      // Locales:
      virtual void 
      imbue(const locale& __loc) 
      { 
	_M_locale_set = true;
	if (_M_locale_buf != __loc)
	 {
	   _M_locale_buf = __loc;
	   _M_fctype_buf = &use_facet<__ctype_type>(_M_locale_buf); 
	 }	
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
	int_type __retval = traits_type::eof();
	bool __testeof = this->underflow() == __retval;
	bool __testpending = _M_in_cur && _M_in_cur < _M_in_end;
	
	if (!__testeof && __testpending)
	  {
	    __retval = traits_type::to_int_type(*_M_in_cur);
	    ++_M_in_cur;
	    if (_M_buf_unified && _M_mode & ios_base::out)
	      ++_M_out_cur;
	  }
	return __retval;    
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

#ifdef _GLIBCPP_DEPRICATED
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
    private:
      basic_streambuf(const __streambuf_type&);

      __streambuf_type& 
      operator=(const __streambuf_type&);
#endif
    };

  typedef basic_streambuf<char>        	streambuf;
  typedef basic_streambuf<wchar_t> 	wstreambuf;

} // namespace std

#ifdef _GLIBCPP_NO_TEMPLATE_EXPORT
# define export
#ifdef  _GLIBCPP_FULLY_COMPLIANT_HEADERS
#include <bits/streambuf.tcc>
#endif
#endif

#endif	/* _CPP_STREAMBUF */











