// Output streams -*- C++ -*-

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
// ISO C++ 14882: 27.6.2  Output streams
//

/** @file ostream
 *  This is a Standard C++ Library header.  You should @c #include this header
 *  in your programs, rather than any of the "st[dl]_*.h" implementation files.
 */

#ifndef _CPP_OSTREAM
#define _CPP_OSTREAM	1

#pragma GCC system_header

#include <ios>

namespace std
{
  // 27.6.2.1 Template class basic_ostream
  template<typename _CharT, typename _Traits>
    class basic_ostream : virtual public basic_ios<_CharT, _Traits>
    {
    public:
      // Types (inherited from basic_ios (27.4.4)):
      typedef _CharT                     		char_type;
      typedef typename _Traits::int_type 		int_type;
      typedef typename _Traits::pos_type 		pos_type;
      typedef typename _Traits::off_type 		off_type;
      typedef _Traits                    		traits_type;
      
      // Non-standard Types:
      typedef basic_streambuf<_CharT, _Traits> 		__streambuf_type;
      typedef basic_ios<_CharT, _Traits>		__ios_type;
      typedef basic_ostream<_CharT, _Traits>		__ostream_type;
      typedef ostreambuf_iterator<_CharT, _Traits>	__ostreambuf_iter;
      typedef num_put<_CharT, __ostreambuf_iter>        __numput_type;
      typedef ctype<_CharT>           			__ctype_type;

      // 27.6.2.2 Constructor/destructor:
      explicit 
      basic_ostream(__streambuf_type* __sb)
      { this->init(__sb); }

      virtual 
      ~basic_ostream() { }

      // 27.6.2.3 Prefix/suffix:
      class sentry;
      friend class sentry;
      
      // 27.6.2.5 Formatted output:
      // 27.6.2.5.3  basic_ostream::operator<<
      __ostream_type&
      operator<<(__ostream_type& (*__pf)(__ostream_type&));
      
      __ostream_type&
      operator<<(__ios_type& (*__pf)(__ios_type&));
      
      __ostream_type&
      operator<<(ios_base& (*__pf) (ios_base&));

      // 27.6.2.5.2 Arithmetic Inserters
      __ostream_type& 
      operator<<(long __n);
      
      __ostream_type& 
      operator<<(unsigned long __n);

      __ostream_type& 
      operator<<(bool __n);

      __ostream_type& 
      operator<<(short __n)
      { 
	ios_base::fmtflags __fmt = this->flags() & ios_base::basefield;
	if (__fmt & ios_base::oct || __fmt & ios_base::hex)
	  return this->operator<<(static_cast<unsigned long>
				  (static_cast<unsigned short>(__n)));
	else
	  return this->operator<<(static_cast<long>(__n));
      }

      __ostream_type& 
      operator<<(unsigned short __n)
      { return this->operator<<(static_cast<unsigned long>(__n)); }

      __ostream_type& 
      operator<<(int __n)
      { 
	ios_base::fmtflags __fmt = this->flags() & ios_base::basefield;
	if (__fmt & ios_base::oct || __fmt & ios_base::hex)
	  return this->operator<<(static_cast<unsigned long>
				  (static_cast<unsigned int>(__n)));
	else
	  return this->operator<<(static_cast<long>(__n));
      }

      __ostream_type& 
      operator<<(unsigned int __n)
      { return this->operator<<(static_cast<unsigned long>(__n)); }

#ifdef _GLIBCPP_USE_LONG_LONG
      __ostream_type& 
      operator<<(long long __n);

      __ostream_type& 
      operator<<(unsigned long long __n);
#endif

      __ostream_type& 
      operator<<(double __f);

      __ostream_type& 
      operator<<(float __f)
      { return this->operator<<(static_cast<double>(__f)); }

      __ostream_type& 
      operator<<(long double __f);

      __ostream_type& 
      operator<<(const void* __p);

      __ostream_type& 
      operator<<(__streambuf_type* __sb);

      // Unformatted output:
      __ostream_type& 
      put(char_type __c);

      __ostream_type& 
      write(const char_type* __s, streamsize __n);

      __ostream_type& 
      flush();

      // Seeks:
      pos_type 
      tellp();

      __ostream_type& 
      seekp(pos_type);

      __ostream_type& 
      seekp(off_type, ios_base::seekdir);
    };

  // 27.6.2.3  Class basic_ostream::sentry
  template <typename _CharT, typename _Traits>
    class basic_ostream<_CharT, _Traits>::sentry
    {
      // Data Members:
      bool 				_M_ok;
      basic_ostream<_CharT,_Traits>& 	_M_os;
      
    public:
      explicit
      sentry(basic_ostream<_CharT,_Traits>& __os);

      ~sentry()
      {
	// XXX MT
	if (_M_os.flags() & ios_base::unitbuf && !uncaught_exception())
	  {
	    // Can't call flush directly or else will get into recursive lock.
	    if (_M_os.rdbuf() && _M_os.rdbuf()->pubsync() == -1)
	      _M_os.setstate(ios_base::badbit);
	  }
      }

      operator bool() 
      { return _M_ok; }
    };

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, _CharT __c);

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, char __c)
    { return (__out << __out.widen(__c)); }

  // Specialization
  template <class _Traits> 
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, char __c);

  // Signed and unsigned
  template<class _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, signed char __c)
    { return (__out << static_cast<char>(__c)); }
  
  template<class _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, unsigned char __c)
    { return (__out << static_cast<char>(__c)); }
  
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, const _CharT* __s);

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits> &
    operator<<(basic_ostream<_CharT, _Traits>& __out, const char* __s);

  // Partial specializationss
  template<class _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, const char* __s);
 
  // Signed and unsigned
  template<class _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, const signed char* __s)
    { return (__out << reinterpret_cast<const char*>(__s)); }

  template<class _Traits>
    basic_ostream<char, _Traits> &
    operator<<(basic_ostream<char, _Traits>& __out, const unsigned char* __s)
    { return (__out << reinterpret_cast<const char*>(__s)); }

  // 27.6.2.7 Standard basic_ostream manipulators
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    endl(basic_ostream<_CharT, _Traits>& __os)
    { return flush(__os.put(__os.widen('\n'))); }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    ends(basic_ostream<_CharT, _Traits>& __os)
    { return __os.put(_CharT()); }
  
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    flush(basic_ostream<_CharT, _Traits>& __os)
    { return __os.flush(); }

} // namespace std

#ifdef _GLIBCPP_NO_TEMPLATE_EXPORT
# define export
#endif
#ifdef  _GLIBCPP_FULLY_COMPLIANT_HEADERS
# include <bits/ostream.tcc>
#endif

#endif	/* _CPP_OSTREAM */
