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

#ifndef _CPP_SSTREAM
#define _CPP_SSTREAM	1

#include <bits/std_istream.h>
#include <bits/std_ostream.h>

namespace std {

  template<typename _CharT, typename _Traits, typename _Alloc>
    class basic_stringbuf : public basic_streambuf<_CharT, _Traits>
    {
    public:
      // Types:
      typedef _CharT 					char_type;
      typedef typename _Traits::int_type 		int_type;
      typedef typename _Traits::pos_type 		pos_type;
      typedef typename _Traits::off_type 		off_type;
      typedef _Traits                    		traits_type;
      // Non-standard types:
      typedef basic_streambuf<_CharT, _Traits> 		__streambuf_type;
      typedef basic_string<_CharT, _Traits, _Alloc> 	__string_type;
      typedef typename __string_type::size_type		__size_type;

    private:
      // Data Members:
      __string_type 		_M_string;
      
    public:
      // Constructors:
      explicit 
      basic_stringbuf(ios_base::openmode __mode = ios_base::in | ios_base::out)
      : __streambuf_type(), _M_string()
      { _M_init_stringbuf(__mode); }

      explicit 
      basic_stringbuf(const __string_type& __str,
		      ios_base::openmode __mode = ios_base::in | ios_base::out)
      : __streambuf_type(), _M_string(__str)
      { _M_init_stringbuf(__mode); }

      // Get and set:
      __string_type 
      str() const 
      {
	if (_M_mode & ios_base::in && !(_M_mode & ios_base::out))
	  return _M_string; 
	else
	  {
	    // This is the deal: _M_string.size() is value that
	    // represents the size of the intial string that makes
	    // _M_string, and may not be the correct size of the
	    // current stringbuf internal buffer.
	    __size_type __len = _M_string.size();
	    if (_M_out_cur > _M_out_beg)
	      __len = max(__size_type(_M_out_end - _M_out_beg), __len);
	    return __string_type(_M_out_beg, _M_out_beg + __len);
	  }
      }

      void 
      str(const __string_type& __s)
      {
	_M_string = __s;
	_M_init_stringbuf(_M_mode);
      }

    protected:
      // Common initialization code for both ctors goes here.
      void
      _M_init_stringbuf(ios_base::openmode __mode)
      {
	// _M_buf_size is a convenient alias for "what the streambuf
	// thinks the allocated size of the string really is." This is
	// necessary as ostringstreams are implemented with the
	// streambufs having control of the allocation and
	// re-allocation of the internal string object, _M_string.
	_M_buf_size = _M_string.size();
	_M_mode = __mode;
	if (_M_mode & ios_base::ate)
	  _M_really_sync(0, _M_buf_size); 
	else  
	  _M_really_sync(0, 0); 
      }

      // Overridden virtual functions:
      virtual int_type 
      underflow()
      {
	if (_M_in_cur && _M_in_cur < _M_in_end)
	  return traits_type::to_int_type(*gptr());
	else
	  return traits_type::eof();
      }

      virtual int_type 
      pbackfail(int_type __c = traits_type::eof());

      virtual int_type 
      overflow(int_type __c = traits_type::eof());

      virtual __streambuf_type* 
      setbuf(char_type* __s, streamsize __n)
      { 
	if (__n) 
	  {
	    _M_string = __string_type(__s, __n);
	    _M_really_sync(0, 0);
	  }
	return this; 
      } 

      virtual pos_type 
      seekoff(off_type __off, ios_base::seekdir __way,
	      ios_base::openmode __mode = ios_base::in | ios_base::out);

      virtual pos_type 
      seekpos(pos_type __sp, 
	      ios_base::openmode __mode = ios_base::in | ios_base::out);

      // Internal function for correctly updating the internal buffer
      // for a particular _M_string, due to initialization or
      // re-sizing of an existing _M_string.
      // Assumes: contents of _M_string and internal buffer match exactly.
      // __i == _M_in_cur - _M_in_beg      
      // __o == _M_out_cur - _M_out_beg
      virtual int 
      _M_really_sync(__size_type __i, __size_type __o)
      {
	char_type* __base = const_cast<char_type*>(_M_string.data());
	bool __testin = _M_mode & ios_base::in;
	bool __testout = _M_mode & ios_base::out;
	__size_type __len = _M_string.size();

	_M_buf = __base;
	if (__testin)
	    this->setg(__base, __base + __i, __base + __len);
	if (__testout)
	  {
	    this->setp(__base, __base + __len);
	    _M_out_cur += __o;
	  }
	return 0;
      }
    };

  typedef basic_stringbuf<char> stringbuf;
  typedef basic_stringbuf<wchar_t> wstringbuf;
  

  // 27.7.2  Template class basic_istringstream
  template<typename _CharT, typename _Traits, typename _Alloc>
    class basic_istringstream : public basic_istream<_CharT, _Traits>
    {
    public:
      // Types:
      typedef _CharT char_type;
      typedef typename _Traits::int_type 		int_type;
      typedef typename _Traits::pos_type 		pos_type;
      typedef typename _Traits::off_type 		off_type;
      typedef _Traits                    		traits_type;

      // Non-standard types:
      typedef basic_string<_CharT, _Traits, _Alloc> 	__string_type;
      typedef basic_stringbuf<_CharT, _Traits, _Alloc> 	__stringbuf_type;
      typedef basic_istream<_CharT, _Traits>		__istream_type;

      // Constructors:
      explicit 
      basic_istringstream(ios_base::openmode __mode = ios_base::in)
      : __istream_type(new __stringbuf_type(__mode | ios_base::in))
      { }

      explicit 
      basic_istringstream(const __string_type& __str,
			  ios_base::openmode __mode = ios_base::in)
      : __istream_type(new __stringbuf_type(__str, __mode | ios_base::in))
      { }

      ~basic_istringstream()
      { 
	delete _M_streambuf; 
	_M_streambuf = NULL;
      }

      // Members:
      __stringbuf_type* 
      rdbuf() const
      { return static_cast<__stringbuf_type*>(_M_streambuf); }

      __string_type
      str() const
      { return this->rdbuf()->str(); }
  
      void 
      str(const __string_type& __s)
      { rdbuf()->str(__s); }

    };

  typedef basic_istringstream<char> istringstream;
  typedef basic_istringstream<wchar_t> wistringstream;

  // 27.7.3  Template class basic_ostringstream
  template <typename _CharT, typename _Traits, typename _Alloc>
    class basic_ostringstream : public basic_ostream<_CharT, _Traits>
    {
    public:
      // Types:
      typedef _CharT char_type;
      typedef typename _Traits::int_type int_type;
      typedef typename _Traits::pos_type pos_type;
      typedef typename _Traits::off_type off_type;
      typedef _Traits                    traits_type;
      // Non-standard types:
      typedef basic_string<_CharT, _Traits, _Alloc> 	__string_type;
      typedef basic_stringbuf<_CharT, _Traits, _Alloc> 	__stringbuf_type;
      typedef basic_ostream<_CharT, _Traits>		__ostream_type;

      // Constructors/destructor:
      explicit 
      basic_ostringstream(ios_base::openmode __mode = ios_base::out)
      : __ostream_type(new __stringbuf_type(__mode | ios_base::out))
      { }

      explicit 
      basic_ostringstream(const __string_type __str,
			  ios_base::openmode __mode = ios_base::out)
      : __ostream_type(new __stringbuf_type(__str, __mode | ios_base::out))
      { }

      ~basic_ostringstream()
      { 
	delete _M_streambuf; 
	_M_streambuf = NULL;
      }

      // Members:
      __stringbuf_type* 
      rdbuf() const
      { return static_cast<__stringbuf_type*>(_M_streambuf); }

      __string_type
      str() const
      { return this->rdbuf()->str(); }
 
      void 
      str(const __string_type& __s)
      { rdbuf()->str(__s); }

    };
  
  typedef basic_ostringstream<char> ostringstream;
  typedef basic_ostringstream<wchar_t> wostringstream;
  
  // 27.7.4  Template class basic_stringstream
  template <typename _CharT, typename _Traits, typename _Alloc>
    class basic_stringstream : public basic_iostream<_CharT, _Traits>
    {
    public:
      // Types
      typedef _CharT char_type;
      typedef typename _Traits::int_type int_type;
      typedef typename _Traits::pos_type pos_type;
      typedef typename _Traits::off_type off_type;
      typedef _Traits                    traits_type;
       // Non-standard types:
      typedef basic_string<_CharT, _Traits, _Alloc> 	__string_type;
      typedef basic_stringbuf<_CharT, _Traits, _Alloc> 	__stringbuf_type;
      typedef basic_iostream<_CharT, _Traits>		__iostream_type;
     
      // Constructors/destructors
      explicit 
      basic_stringstream(ios_base::openmode __mode = 
			 ios_base::out | ios_base::in)
      : __iostream_type(new __stringbuf_type(__mode))
      { }

      explicit 
      basic_stringstream(const __string_type& __str,
			 ios_base::openmode __mode = 
			 ios_base::out | ios_base::in)
      : __iostream_type(new __stringbuf_type(__str, __mode))
      { }

      ~basic_stringstream()
      { 
	delete _M_streambuf; 
	_M_streambuf = NULL;
      }

      // Members:
      __stringbuf_type* 
      rdbuf() const
      { return static_cast<__stringbuf_type*>(_M_streambuf); }

      __string_type
      str() const
      { return rdbuf()->str(); }

      void 
      str(const __string_type& __s)
      { rdbuf()->str(__s); }
    };

  typedef basic_stringstream<char> stringstream;
  typedef basic_stringstream<wchar_t> wstringstream;
  
} // namespace std



#ifdef _GLIBCPP_NO_TEMPLATE_EXPORT
# define export
#ifdef  _GLIBCPP_FULLY_COMPLIANT_HEADERS
# include <bits/sstream.tcc>
#endif
#endif


#endif	/* _CPP_SSTREAM */














