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

#ifndef _CPP_FSTREAM
#define _CPP_FSTREAM	1

#pragma GCC system_header

#include <bits/std_istream.h>
#include <bits/std_ostream.h>
#include <bits/basic_file.h>
#include <bits/std_locale.h>	// For codecvt
#include <bits/c++threads.h>	// For __mutext_type

namespace std 
{
  template<typename _CharT, typename _Traits>
    class basic_filebuf : public basic_streambuf<_CharT, _Traits>
    {
    public:
      // Types:
      typedef _CharT                     	        char_type;
      typedef _Traits                    	        traits_type;
      typedef typename traits_type::int_type 		int_type;
      typedef typename traits_type::pos_type 		pos_type;
      typedef typename traits_type::off_type 		off_type;
      
      // Non-standard Types:
      typedef basic_streambuf<char_type, traits_type>  	__streambuf_type;
      typedef basic_filebuf<char_type, traits_type>     __filebuf_type;
      typedef __basic_file<char_type>		        __file_type;
      typedef typename traits_type::state_type          __state_type;
      typedef codecvt<char_type, char, __state_type>    __codecvt_type;
      typedef typename __codecvt_type::result 	        __res_type;
      typedef ctype<char_type>                          __ctype_type;

      friend class ios_base; // For sync_with_stdio.

    private:
      // Data Members:
      // External buffer.
      __file_type* 		_M_file;

      // Current and beginning state type for codecvt.
      __state_type		_M_state_cur;
      __state_type 		_M_state_beg; 	

      // MT lock inherited from libio or other low-level io library.
      __c_lock          	_M_lock;

      // XXX Needed? 
      bool			_M_last_overflowed;  
  
    public:
      // Constructors/destructor:
      basic_filebuf();

      // Non-standard ctor:
      basic_filebuf(__c_file_type* __f, bool __s, ios_base::openmode __mode);
 
      virtual 
      ~basic_filebuf() 
      { 
	this->close();
	_M_last_overflowed = false;
      }

      // Members:
      bool 
      is_open(void) const { return _M_file ? _M_file->is_open() : false; }
    
      __filebuf_type* 
      open(const char* __s, ios_base::openmode __mode);
    
      __filebuf_type* 
      close(void);

    protected:
      // Allocate up pback and internal buffers.
      void 
      _M_allocate_buffers();

      // Create __file_type object and initialize it properly.
      void
      _M_filebuf_init();

      // Overridden virtual functions:
      virtual streamsize 
      showmanyc(void);
   
      // Stroustrup, 1998, p. 628 
      // underflow() and uflow() functions are called to get the next
      // charater from the real input source when the buffer is empty.
      // Buffered input uses underflow()
      virtual int_type 
      underflow(void);

      virtual int_type 
      pbackfail(int_type __c = _Traits::eof());

      // NB: For what the standard expects of the overflow function,
      // see _M_really_overflow(), below. Because basic_streambuf's
      // sputc/sputn call overflow directly, and the complications of
      // this implementation's setting of the initial pointers all
      // equal to _M_buf when initializing, it seems essential to have
      // this in actuality be a helper function that checks for the
      // eccentricities of this implementation, and then call
      // overflow() if indeed the buffer is full.
      virtual int_type 
      overflow(int_type __c = _Traits::eof());

      // Stroustrup, 1998, p 648
      // The overflow() function is called to transfer characters to the
      // real output destination when the buffer is full. A call to
      // overflow(c) outputs the contents of the buffer plus the
      // character c.
      // 27.5.2.4.5 
      // Consume some sequence of the characters in the pending sequence.
      int_type 
      _M_really_overflow(int_type __c = _Traits::eof());
    
      virtual __streambuf_type* 
      setbuf(char_type* __s, streamsize __n)
      {
	if (!this->is_open() && __s == 0 && __n == 0)
	  {
	    _M_buf_size = 0;
	    _M_buf_size_opt = 0;
	  }
	_M_last_overflowed = false;	
	return this; 
      }
    
      virtual pos_type 
      seekoff(off_type __off, ios_base::seekdir __way,
	      ios_base::openmode __mode = ios_base::in | ios_base::out);

      virtual pos_type 
      seekpos(pos_type __pos,
	      ios_base::openmode __mode = ios_base::in | ios_base::out);

      virtual int 
      sync(void)
      {
	bool __testput = _M_out_cur && _M_out_beg < _M_out_end;
	if (__testput)
	  {
            // Make sure that libio resyncs its idea of the file position
            // with the external file.
            _M_file->sync();

	    // Need to restore current position. This interpreted as
	    // the position of the external byte sequence (_M_file)
	    // plus the offset in the current internal buffer
	    // (_M_out_beg - _M_out_cur)
	    streamoff __cur = _M_file->seekoff(0, ios_base::cur);
	    off_type __off = _M_out_cur - _M_out_beg;
	    _M_really_overflow();
	    _M_file->seekpos(__cur + __off);
	  }
	_M_last_overflowed = false;	
	return 0;
      }
      
      virtual void 
      imbue(const locale& __loc);

      virtual streamsize 
      xsgetn(char_type* __s, streamsize __n)
      {
	streamsize __ret = 0;
	// Clear out pback buffer before going on to the real deal...
	if (_M_pback_init)
	  {
	    while (__ret < __n && _M_in_cur < _M_in_end)
	      {
		*__s = *_M_in_cur;
		++__ret;
		++__s;
		++_M_in_cur;
	      }
	    _M_pback_destroy();
	  }
	if (__ret < __n)
	  __ret += __streambuf_type::xsgetn(__s, __n - __ret);
	return __ret;
      }
 
      virtual streamsize 
      xsputn(const char_type* __s, streamsize __n)
      {
	_M_pback_destroy();
	return __streambuf_type::xsputn(__s, __n);
      }
       
      void
      _M_output_unshift();
    };


  // 27.8.1.5  Template class basic_ifstream
  template<typename _CharT, typename _Traits>
    class basic_ifstream : public basic_istream<_CharT, _Traits>
    {
    public:
      // Types:
      typedef _CharT 					char_type;
      typedef _Traits 					traits_type;
      typedef typename traits_type::int_type 		int_type;
      typedef typename traits_type::pos_type 		pos_type;
      typedef typename traits_type::off_type 		off_type;

      // Non-standard types:
      typedef basic_filebuf<char_type, traits_type> 	__filebuf_type;
      typedef basic_istream<char_type, traits_type>	__istream_type;
    
      // Constructors/Destructors:
      basic_ifstream()
      : __istream_type(new __filebuf_type())
      { }

      explicit 
      basic_ifstream(const char* __s, ios_base::openmode __mode = ios_base::in)
      : __istream_type(new __filebuf_type())
      { this->open(__s, __mode); }
    
      ~basic_ifstream()
      { 
	delete _M_streambuf; 
	_M_streambuf = NULL;
      }

      // Members:
      __filebuf_type* 
      rdbuf() const 
      { return static_cast<__filebuf_type*>(_M_streambuf); }

      bool 
      is_open(void) { return rdbuf()->is_open(); }

      void 
      open(const char* __s, ios_base::openmode __mode = ios_base::in)
      { 
	if (rdbuf()->open(__s, __mode | ios_base::in) == NULL)
	  this->setstate(ios_base::failbit); 
      }

      void 
      close(void)
      { 
	if (!rdbuf()->close())
	  this->setstate(ios_base::failbit);	
      }
    };

  
  // 27.8.1.8  Template class basic_ofstream
  template<typename _CharT, typename _Traits>
    class basic_ofstream : public basic_ostream<_CharT,_Traits>
    {
    public:
      // Types:
      typedef _CharT 					char_type;
      typedef _Traits 					traits_type;
      typedef typename traits_type::int_type 		int_type;
      typedef typename traits_type::pos_type 		pos_type;
      typedef typename traits_type::off_type 		off_type;

      // Non-standard types:
      typedef basic_filebuf<char_type, traits_type> 	__filebuf_type;
      typedef basic_ostream<char_type, traits_type>	__ostream_type;
      
      // Constructors:
      basic_ofstream()
      : __ostream_type(new __filebuf_type())
      { }
      
      explicit 
      basic_ofstream(const char* __s, 
		     ios_base::openmode __mode = ios_base::out|ios_base::trunc)
      : __ostream_type(new __filebuf_type())
      { this->open(__s, __mode); }

      ~basic_ofstream()
      { 
	delete _M_streambuf; 
	_M_streambuf = NULL;
      }

      // Members:
      __filebuf_type* 
      rdbuf(void) const
      { return static_cast<__filebuf_type*>(_M_streambuf); }
 
      bool 
      is_open(void) { return rdbuf()->is_open(); }

      void 
      open(const char* __s, 
	   ios_base::openmode __mode = ios_base::out | ios_base::trunc)
      { 
	if (!rdbuf()->open(__s, __mode | ios_base::out))
	  this->setstate(ios_base::failbit); 
      }

      void 
      close(void)
      { 
	if (!rdbuf()->close())
	  setstate(ios_base::failbit); 
      }
    };


  // 27.8.1.11  Template class basic_fstream
  template<typename _CharT, typename _Traits>
    class basic_fstream : public basic_iostream<_CharT, _Traits>
    {
    public:
      // Types:
      typedef _CharT 					char_type;
      typedef _Traits 					traits_type;
      typedef typename traits_type::int_type 		int_type;
      typedef typename traits_type::pos_type 		pos_type;
      typedef typename traits_type::off_type 		off_type;

      // Non-standard types:
      typedef basic_filebuf<char_type, traits_type> 	__filebuf_type;
      typedef basic_ios<char_type, traits_type>		__ios_type;
      typedef basic_iostream<char_type, traits_type>	__iostream_type;

      // Constructors/destructor:
      basic_fstream()
      : __iostream_type(new __filebuf_type())
      { }

      explicit 
      basic_fstream(const char* __s,
		    ios_base::openmode __mode = ios_base::in | ios_base::out)
      : __iostream_type(new __filebuf_type())
      { this->open(__s, __mode); }

      ~basic_fstream()
      { 
	delete _M_streambuf; 
	_M_streambuf = NULL;
      }
    
      // Members:
      __filebuf_type* 
      rdbuf(void) const 
      { return static_cast<__filebuf_type*>(_M_streambuf); }

      bool 
      is_open(void) { return rdbuf()->is_open(); }

      void 
      open(const char* __s, 
	   ios_base::openmode __mode = ios_base::in | ios_base::out)
      { 
	if (!rdbuf()->open(__s, __mode))
	  setstate (ios_base::failbit); 
      }

      void 
      close(void)
      { 
	if (!rdbuf()->close())
	  setstate (ios_base::failbit); 
      }
    };
} // namespace std


#ifdef _GLIBCPP_NO_TEMPLATE_EXPORT
# define export
#ifdef  _GLIBCPP_FULLY_COMPLIANT_HEADERS
# include <bits/fstream.tcc>
#endif
#endif

#endif	

