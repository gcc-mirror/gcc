// Iostreams base classes -*- C++ -*-

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

#ifndef _CPP_BITS_BASICIOS_H
#define _CPP_BITS_BASICIOS_H 1

#include <bits/sbuf_iter.h>

namespace std {

  // 27.4.5  Template class basic_ios
  template<typename _CharT, typename _Traits>
    class basic_ios : public ios_base
    {
    public:

      // Types:
      typedef _CharT 				char_type;
      typedef typename _Traits::int_type 	int_type;
      typedef typename _Traits::pos_type 	pos_type;
      typedef typename _Traits::off_type 	off_type;
      typedef _Traits 				traits_type;

      // Non-standard Types:
      typedef ctype<_CharT>           		__ctype_type;
      // From ostream
      typedef ostreambuf_iterator<_CharT>		__ostreambuf_iter;
      typedef num_put<_CharT, __ostreambuf_iter>        __numput_type;
      typedef istreambuf_iterator<_CharT>		__istreambuf_iter;
      typedef num_get<_CharT, __istreambuf_iter>        __numget_type;
      
      // Data members:
    private:
      basic_ostream<_CharT, _Traits>* 	_M_tie;
      char_type 			_M_fill;
      iostate 				_M_exception;

    protected:
      basic_streambuf<_CharT, _Traits>* _M_streambuf;
      iostate 				_M_streambuf_state;

      // Cached use_facet<ctype>, which is based on the current locale info.
      const __ctype_type*		_M_fctype_ios;      
      // From ostream.
      const __numput_type* 		_M_fnumput;
      // From istream.
      const __numget_type* 		_M_fnumget;

    public:

      inline const __ctype_type*	
      _M_get_fctype_ios(void)
      { return _M_fctype_ios; }

      inline const __numget_type* 
      _M_get_fnumget(void)
      { return _M_fnumget; }

      inline const __numput_type* 
      _M_get_fnumput(void)
      { return _M_fnumput; }

      operator void*() const 
      { return this->fail() ? 0 : const_cast<basic_ios*>(this); }

      inline bool 
      operator!() const 
      { return this->fail(); }

      inline iostate 
      rdstate() const 
      { return _M_streambuf_state; }

      inline void 
      clear(iostate __state = goodbit)
      { 
	if (this->rdbuf())
	  _M_streambuf_state = __state;
	else
	  _M_streambuf_state = __state | badbit;
	if ((this->rdstate() & this->exceptions()))
	  throw failure("basic_ios::clear(iostate) caused exception");
      }

      inline void 
      setstate(iostate __state) 
      { this->clear(this->rdstate() | __state); }

      inline bool 
      good() const 
      { return this->rdstate() == 0; }

      inline bool 
      eof() const 
      { return (this->rdstate() & eofbit) != 0; }

      inline bool 
      fail() const 
      { return (this->rdstate() & (badbit | failbit)) != 0; }

      inline bool 
      bad() const 
      { return (this->rdstate() & badbit) != 0; }

      inline iostate 
      exceptions() const 
      { return _M_exception; }

      inline void 
      exceptions(iostate __except) 
      { 
	_M_exception = __except; 
	this->clear(_M_streambuf_state); 
      }

      // Constructor/destructor:
      explicit 
      basic_ios(basic_streambuf<_CharT, _Traits>* __sb) : ios_base() 
      { this->init(__sb); }

      virtual 
      ~basic_ios() { }
      
      // Members:
      inline basic_ostream<_CharT, _Traits>*
      tie() const      
      { return _M_tie; }

      inline basic_ostream<_CharT, _Traits>*
      tie(basic_ostream<_CharT, _Traits>* __tiestr)
      {
	basic_ostream<_CharT, _Traits>* __old = _M_tie;
	_M_tie = __tiestr;
	return __old;
      }

      inline basic_streambuf<_CharT, _Traits>*
      rdbuf() const    
      { return _M_streambuf; }

      basic_streambuf<_CharT, _Traits>* 
      rdbuf(basic_streambuf<_CharT, _Traits>* __sb);

      basic_ios&
      copyfmt(const basic_ios& __rhs);

      inline char_type 
      fill() const 
      { return _M_fill; }

      inline char_type 
      fill(char_type __ch)
      {
	char_type __old = _M_fill;
	_M_fill = __ch;
	return __old;
      }

      // Locales:
      locale 
      imbue(const locale& __loc);

      char 
      narrow(char_type __c, char __dfault) const;

      char_type 
      widen(char __c) const;
     
    protected:
      // 27.4.5.1  basic_ios constructors
      basic_ios() : ios_base() 
      { }

      void 
      init(basic_streambuf<_CharT, _Traits>* __sb);
    };
  
} // namespace std

#ifdef _GLIBCPP_NO_TEMPLATE_EXPORT
# define export
//#include <bits/basic_ios.tcc>
#endif

#endif /* _CPP_BITS_BASICIOS_H */





