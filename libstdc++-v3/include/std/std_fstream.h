// File based streams -*- C++ -*-

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
// ISO C++ 14882: 27.8  File-based streams
//

/** @file fstream
 *  This is a Standard C++ Library header.  You should @c #include this header
 *  in your programs, rather than any of the "st[dl]_*.h" implementation files.
 */

#ifndef _CPP_FSTREAM
#define _CPP_FSTREAM	1

#pragma GCC system_header

#include <istream>
#include <ostream>
#include <locale>	// For codecvt
#include <bits/basic_file.h>
#include <bits/gthr.h>

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
      typedef __basic_file<char>		        __file_type;
      typedef typename traits_type::state_type          __state_type;
      typedef codecvt<char_type, char, __state_type>    __codecvt_type;
      typedef typename __codecvt_type::result 	        __res_type;
      typedef ctype<char_type>                          __ctype_type;

      friend class ios_base; // For sync_with_stdio.

    protected:
      // Data Members:
      // MT lock inherited from libio or other low-level io library.
      __c_lock          	_M_lock;

      // External buffer.
      __file_type 		_M_file;

      // Current and beginning state type for codecvt.
      __state_type		_M_state_cur;
      __state_type 		_M_state_beg;

      // Set iff _M_buf is allocated memory from _M_allocate_internal_buffer..
      bool			_M_buf_allocated;
      
      // Stack-based buffer for unbuffered input.
      char_type			_M_unbuf[4];

      // XXX Needed?
      bool			_M_last_overflowed;

    public:
      // Constructors/destructor:
      basic_filebuf();

      // Non-standard ctor:
      basic_filebuf(__c_file* __f, ios_base::openmode __mode,
		    int_type __s = static_cast<int_type>(BUFSIZ));

      // Non-standard member:
      int
      fd();

      virtual
      ~basic_filebuf()
      {
	this->close();
	_M_last_overflowed = false;
      }

      // Members:
      bool
      is_open() const { return _M_file.is_open(); }

      __filebuf_type*
      open(const char* __s, ios_base::openmode __mode);

      __filebuf_type*
      close();

    protected:
      void
      _M_allocate_internal_buffer();

      void
      _M_destroy_internal_buffer();

      // Overridden virtual functions:
      virtual streamsize
      showmanyc();

      // Stroustrup, 1998, p. 628
      // underflow() and uflow() functions are called to get the next
      // charater from the real input source when the buffer is empty.
      // Buffered input uses underflow()
      virtual int_type
      underflow();

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

      // Convert internal byte sequence to external, char-based
      // sequence via codecvt.
      void
      _M_convert_to_external(char_type*, streamsize, streamsize&, streamsize&);

      virtual __streambuf_type*
      setbuf(char_type* __s, streamsize __n);

      virtual pos_type
      seekoff(off_type __off, ios_base::seekdir __way,
	      ios_base::openmode __mode = ios_base::in | ios_base::out);

      virtual pos_type
      seekpos(pos_type __pos,
	      ios_base::openmode __mode = ios_base::in | ios_base::out);

      virtual int
      sync()
      {
	bool __testput = _M_out_cur && _M_out_beg < _M_out_end;

	// Make sure that the internal buffer resyncs its idea of
	// the file position with the external file.
	if (__testput && !_M_file.sync())
	  {
	    // Need to restore current position. This interpreted as
	    // the position of the external byte sequence (_M_file)
	    // plus the offset in the current internal buffer
	    // (_M_out_beg - _M_out_cur)
	    streamoff __cur = _M_file.seekoff(0, ios_base::cur);
	    off_type __off = _M_out_cur - _M_out_beg;
	    _M_really_overflow();
	    _M_file.seekpos(__cur + __off);
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
  /**
   *  Derivation of general input streams, specific to files.
  */
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

    private:
      __filebuf_type	_M_filebuf;

    public:
     // Constructors/Destructors:
     /** Default constructor.  Create an input file stream.  */
      basic_ifstream()
      : __istream_type(NULL), _M_filebuf()
      { this->init(&_M_filebuf); }

      /**
       *  @brief Create an input file stream.
       *  @param  s  Null terminated string specifying filename.
       *  @param  mode  Open file in specified mode (see std::ios_base).
       *
       *  Tip:  When using std::string to hold the filename, you must use
       *  .c_str() before passing it to this constructor.
      */
      explicit
      basic_ifstream(const char* __s, ios_base::openmode __mode = ios_base::in)
      : __istream_type(NULL), _M_filebuf()
      {
	this->init(&_M_filebuf);
	this->open(__s, __mode);
      }

      ~basic_ifstream()
      { }

      // Members:
      /**
       *  @brief  Get a pointer to the file stream's buffer.
       *  @return Pointer to basic_filebuf.
      */
      __filebuf_type*
      rdbuf() const
      { return const_cast<__filebuf_type*>(&_M_filebuf); }

      bool
      is_open() { return _M_filebuf.is_open(); }

      void
      open(const char* __s, ios_base::openmode __mode = ios_base::in)
      {
	if (!_M_filebuf.open(__s, __mode | ios_base::in))
	  this->setstate(ios_base::failbit);
      }

      /** Close the file.  */
      void
      close()
      {
	if (!_M_filebuf.close())
	  this->setstate(ios_base::failbit);
      }
    };


  // 27.8.1.8  Template class basic_ofstream
  /**
   *  Derivation of general output streams, specific to files.
  */
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

    private:
      __filebuf_type	_M_filebuf;

    public:
      // Constructors:
      /** Default constructor for output file_stream.  */
      basic_ofstream()
      : __ostream_type(NULL), _M_filebuf()
      { this->init(&_M_filebuf); }

      /**
       *  @brief  Create an output stream.
       *  @param  s  Null terminated string specifying filename.
       *  @param  mode  Open file in specified mode (see std::ios_base).
       *
       *  Tip:  When using std::string to hold the filename, you must use
       *  .c_str() before passing it to this constructor.
      */
      explicit
      basic_ofstream(const char* __s,
		     ios_base::openmode __mode = ios_base::out|ios_base::trunc)
      : __ostream_type(NULL), _M_filebuf()
      {
	this->init(&_M_filebuf);
	this->open(__s, __mode);
      }

      ~basic_ofstream()
      { }

      // Members:
      /**
       *  @brief  Get a pointer to the file stream's buffer.
       *  @return Pointer to basic_filebuf.
      */
      __filebuf_type*
      rdbuf() const
      { return const_cast<__filebuf_type*>(&_M_filebuf); }

      /**
       *  @brief Query to see if file stream is open.
       *  @return True if stream is open.
      */
      bool
      is_open() { return _M_filebuf.is_open(); }

      /**
       *  @brief Specify a file to open for output.
       *  @param  s  Null terminated string specifying filename.
       *  @param  mode  Mode in which to open file (see std::ios_base).
       *
       *  Tip:  When using std::string to hold the filename, you must use
       *  .c_str() before passing it to this constructor.
      */
      void
      open(const char* __s,
	   ios_base::openmode __mode = ios_base::out | ios_base::trunc)
      {
	if (!_M_filebuf.open(__s, __mode | ios_base::out))
	  this->setstate(ios_base::failbit);
      }

      /** Close the file stream.  */
      void
      close()
      {
	if (!_M_filebuf.close())
	  this->setstate(ios_base::failbit);
      }
    };


  // 27.8.1.11  Template class basic_fstream
  /**
   *  Derivation of general input/output streams, specific to files.
  */
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

    private:
      __filebuf_type	_M_filebuf;

    public:
      // Constructors/destructor:
      /** Default constructor.  Create a file stream.  */
      basic_fstream()
      : __iostream_type(NULL), _M_filebuf()
      { this->init(&_M_filebuf); }

      /**
       *  @brief Create an input/output stream.
       *  @param  s  Null terminated string specifying filename.
       *  @param  mode  Open file in specified mode (see std::ios_base).
       *
       *  Tip:  When using std::string to hold the filename, you must use
       *  .c_str() before passing it to this constructor.
      */
      explicit
      basic_fstream(const char* __s,
		    ios_base::openmode __mode = ios_base::in | ios_base::out)
      : __iostream_type(NULL), _M_filebuf()
      {
	this->init(&_M_filebuf);
	this->open(__s, __mode);
      }

      ~basic_fstream()
      { }

      // Members:
      /**
       *  @brief  Get a pointer to the file stream's buffer.
       *  @return Pointer to basic_filebuf.
      */
      __filebuf_type*
      rdbuf() const
      { return const_cast<__filebuf_type*>(&_M_filebuf); }

      /**
       *  @brief Query to see if file stream is open.
       *  @return True if stream is open.
      */
      bool
      is_open() { return _M_filebuf.is_open(); }

      /**
       *  @brief Specify a file to open for input and/or output.
       *  @param  s  Null terminated string specifying filename.
       *  @param  mode  Mode in which to open file (see std::ios_base).
       *
       *  Tip:  When using std::string to hold the filename, you must use
       *  .c_str() before passing it to this constructor.
      */
      void
      open(const char* __s,
	   ios_base::openmode __mode = ios_base::in | ios_base::out)
      {
	if (!_M_filebuf.open(__s, __mode))
	  setstate(ios_base::failbit);
      }

      /** Close the file stream.  */
      void
      close()
      {
	if (!_M_filebuf.close())
	  setstate(ios_base::failbit);
      }
    };
} // namespace std

#ifdef _GLIBCPP_NO_TEMPLATE_EXPORT
# define export
#endif
#ifdef  _GLIBCPP_FULLY_COMPLIANT_HEADERS
# include <bits/fstream.tcc>
#endif

#endif
