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
// ISO C++ 14882: 27.6.2  Output streams
//

#include <bits/std_locale.h>

namespace std {

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>::sentry::
    sentry(basic_ostream<_CharT,_Traits>& __os)
    : _M_ok(__os.good()), _M_os(__os)
    {
      // XXX MT 
      if (_M_ok && __os.tie())
	  __os.tie()->flush();  
    }
  
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::
    operator<<(__ostream_type& (*__pf)(__ostream_type&))
    {
      sentry __cerb(*this);
      if (__cerb)
	{ 
	  try {
	      __pf(*this);
	  }
	  catch(exception& __fail){
	    // 27.6.2.5.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
		throw;
	  }
	}
      return *this;
    }
  
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::
    operator<<(__ios_type& (*__pf)(__ios_type&))
    {
      sentry __cerb(*this);
      if (__cerb)
	{ 
	  try {
	      __pf(*this);
	  }
	  catch(exception& __fail){
	    // 27.6.2.5.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
		throw;
	  }
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::
    operator<<(ios_base& (*__pf)(ios_base&))
    {
      sentry __cerb(*this);
      if (__cerb)
	{ 
	  try {
	      __pf(*this);
	  }
	  catch(exception& __fail){
	    // 27.6.2.5.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
		throw;
	  }
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(bool __n)
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  try {
	    if (_M_fnumput->put(*this, *this, this->fill(), __n).failed())
	      this->setstate(ios_base::badbit);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(long __n)
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  try {
	    bool __f;
	    ios_base::fmtflags __fmt = this->flags() & ios_base::basefield;
	    if (__fmt & ios_base::oct || __fmt & ios_base::hex)
	      __f = _M_fnumput->put(*this, *this, this->fill(), 
				    static_cast<unsigned long>(__n)).failed();
	    else
	      __f = _M_fnumput->put(*this, *this, this->fill(), __n).failed();

	    if (__f)  
	      this->setstate(ios_base::badbit);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(unsigned long __n)
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  try {
	    if (_M_fnumput->put(*this, *this, this->fill(), __n).failed())
	      this->setstate(ios_base::badbit);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return *this;
    }

#ifdef _GLIBCPP_USE_LONG_LONG
  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(long long __n)
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  try {
	    bool __f;
	    ios_base::fmtflags __fmt = this->flags() & ios_base::basefield;
	    if (__fmt & ios_base::oct || __fmt & ios_base::hex)
	      __f = _M_fnumput->put(*this, *this, this->fill(), 
				    static_cast<unsigned long long>(__n)).failed();
	    else
	      __f = _M_fnumput->put(*this, *this, this->fill(), __n).failed();

	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(unsigned long long __n)
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  try {
	    if (_M_fnumput->put(*this, *this, this->fill(), __n).failed())
	      this->setstate(ios_base::badbit);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return *this;
    }
#endif

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(double __n)
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  try {
	    if (_M_fnumput->put(*this, *this, this->fill(), __n).failed())
	      this->setstate(ios_base::badbit);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(long double __n)
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  try {
	    if (_M_fnumput->put(*this, *this, this->fill(), __n).failed())
	      this->setstate(ios_base::badbit);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(const void* __n)
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  try {
	    if (_M_fnumput->put(*this, *this, this->fill(), __n).failed())
	      this->setstate(ios_base::badbit);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    this->setstate(ios_base::badbit);
	    if ((this->exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>& 
    basic_ostream<_CharT, _Traits>::operator<<(__streambuf_type* __sbin)
    {
      streamsize __xtrct = 0;
      __streambuf_type* __sbout = this->rdbuf();
      sentry __cerb(*this);
      if (__sbin && __cerb)
	__xtrct = _S_copy_streambufs(*this, __sbin, __sbout);
      if (!__sbin || !__xtrct)
	this->setstate(ios_base::failbit);
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    basic_ostream<_CharT, _Traits>::put(char_type __c)
    { 
      sentry __cerb(*this);
      if (__cerb) 
	{
	  int_type __put = rdbuf()->sputc(__c); 
	  if (__put != traits_type::to_int_type(__c))
	    this->setstate(ios_base::badbit);
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    basic_ostream<_CharT, _Traits>::write(const _CharT* __s, streamsize __n)
    {
      sentry __cerb(*this);
      if (__cerb)
	{
	  streamsize __put = this->rdbuf()->sputn(__s, __n);
	  if ( __put != __n)
	    this->setstate(ios_base::badbit);
	}
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    basic_ostream<_CharT, _Traits>::flush()
    {
      sentry __cerb(*this);
      if (__cerb) 
	{
	  if (this->rdbuf() && this->rdbuf()->pubsync() == -1)
	    this->setstate(ios_base::badbit);
	}
      return *this;
    }
  
  template<typename _CharT, typename _Traits>
    typename basic_ostream<_CharT, _Traits>::pos_type
    basic_ostream<_CharT, _Traits>::tellp()
    {
      pos_type __retval = pos_type(-1);
      bool __testok = this->fail() != true;
      
      if (__testok)
	__retval = this->rdbuf()->pubseekoff(0, ios_base::cur, ios_base::out);
      return __retval;
    }


  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    basic_ostream<_CharT, _Traits>::seekp(pos_type __pos)
    {
      bool __testok = this->fail() != true;
      
      if (__testok)
#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
// 136.  seekp, seekg setting wrong streams?
	this->rdbuf()->pubseekpos(__pos, ios_base::out);
#endif
      return *this;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    basic_ostream<_CharT, _Traits>::
    seekp(off_type __off, ios_base::seekdir __d)
    {
      bool __testok = this->fail() != true;
      
      if (__testok)
#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
// 136.  seekp, seekg setting wrong streams?
	rdbuf()->pubseekoff(__off, __d, ios_base::out);
#endif
      return *this;
    }

  // 27.6.2.5.4 Character inserters

  // Construct correctly padded string, as per 22.2.2.2.2
  // Similar in theory to _S_pad_numeric, from num_put, but it doesn't
  // use _S_fill: perhaps it should.
  // Assumes 
  // __newlen > __oldlen
  // __news is allocated for __newlen size
  template<typename _CharT, typename _Traits>
    static void
    _S_pad_char(basic_ios<_CharT, _Traits>& __ios, 
		_CharT* __news, const _CharT* __olds,
		const streamsize __newlen, const streamsize __oldlen)
    {
      typedef _CharT	char_type;
      typedef _Traits	traits_type;
      typedef typename traits_type::int_type int_type;
      
      int_type __plen = static_cast<size_t>(__newlen - __oldlen); 
      char_type __pads[__plen];
      traits_type::assign(__pads, __plen, __ios.fill()); 

      char_type* __beg;
      char_type* __end;
      size_t __mod = 0;
      size_t __beglen; //either __plen or __oldlen
      ios_base::fmtflags __fmt = __ios.flags() & ios_base::adjustfield;

      if (__fmt == ios_base::left)
	{
	  // Padding last.
	  __beg = const_cast<char_type*>(__olds);
	  __beglen = __oldlen;
	  __end = __pads;
	}
      else if (__fmt == ios_base::internal)
	{
	  // Pad after the sign, if there is one.
	  // Pad after 0[xX], if there is one.
	  // Who came up with these rules, anyway? Jeeze.
	  typedef _Format_cache<_CharT> __cache_type;
	  __cache_type const* __fmt = __cache_type::_S_get(__ios);
	  const char_type* __minus = traits_type::find(__olds, __oldlen, 
						       __fmt->_S_minus);
	  const char_type* __plus = traits_type::find(__olds, __oldlen, 
						      __fmt->_S_plus);
	  bool __testsign = __minus || __plus;
	  bool __testhex = __olds[0] == '0' 
	    		   && (__olds[1] == 'x' || __olds[1] == 'X');

	  if (__testhex)
	    {
	      __news[0] = __olds[0]; 
	      __news[1] = __olds[1];
	      __mod += 2;
	      __beg = const_cast<char_type*>(__olds + __mod);
	      __beglen = __oldlen - __mod;
	      __end = __pads;
	    }
	  else if (__testsign)
	    {
	      __mod += __plen;
	      const char_type* __sign = __minus ? __minus + 1: __plus + 1;
	      __beg = const_cast<char_type*>(__olds);
	      __beglen = __sign - __olds;
	      __end = const_cast<char_type*>(__sign + __plen);
	      traits_type::copy(__news + __beglen, __pads, __plen);
	    }
	  else
	    {
	      // Padding first.
	      __beg = __pads;
	      __beglen = __plen;
	      __end = const_cast<char_type*>(__olds);
	    }
	}
      else
	{
	  // Padding first.
	  __beg = __pads;
	  __beglen = __plen;
	  __end = const_cast<char_type*>(__olds);
	}

      traits_type::copy(__news, __beg, __beglen);
      traits_type::copy(__news + __beglen, __end, __newlen - __beglen - __mod);
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, _CharT __c)
    {
      typedef basic_ostream<_CharT, _Traits> __ostream_type;
      __ostream_type::sentry __cerb(__out);
      if (__cerb)
	{
	  try {
	    streamsize __w = __out.width();
	    _CharT __pads[__w];
	    __pads[0] = __c;
	    streamsize __len = 1;
	    if (__w > __len)
	      {
		_S_pad_char(__out, __pads, &__c, __w, __len);
		__len = __w;
	      }
	    __out.write(__pads, __len);
	    __out.width(0);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    __out.setstate(ios_base::badbit);
	    if ((__out.exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return __out;
    }

  // Specialization
  template <class _Traits> 
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, char __c)
    {
      typedef basic_ostream<char, _Traits> __ostream_type;
      __ostream_type::sentry __cerb(__out);
      if (__cerb)
	{
	  try {
	    streamsize __w = __out.width();
	    char __pads[__w + 1];
	    __pads[0] = __c;
	    streamsize __len = 1;
	    if (__w > __len)
	      {
		_S_pad_char(__out, __pads, &__c, __w, __len);
		__len = __w;
	      }
	    __out.write(__pads, __len);
	    __out.width(0);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    __out.setstate(ios_base::badbit);
	    if ((__out.exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return __out;
     }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, const _CharT* __s)
    {
      typedef basic_ostream<_CharT, _Traits> __ostream_type;
      __ostream_type::sentry __cerb(__out);
      if (__cerb)
	{
	  try {
	    streamsize __w = __out.width();
	    _CharT __pads[__w];
	    streamsize __len = static_cast<streamsize>(_Traits::length(__s));
	    if (__w > __len)
	      {
		_S_pad_char(__out, __pads, __s, __w, __len);
		__s = __pads;
		__len = __w;
	      }
	    __out.write(__s, __len);
	    __out.width(0);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    __out.setstate(ios_base::badbit);
	    if ((__out.exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return __out;
    }

  template<typename _CharT, typename _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out, const char* /*__s*/)
    {
#if 0
      typedef basic_ostream<_CharT, _Traits>   		__ostream_type;
      typedef typename _Traits::state_type              __state_type;
      typedef codecvt<char, _CharT, __state_type>       __codecvt_type;
      typedef typename __ostream_type::char_type	__char_type;

      __ostream_type::sentry __cerb(__out);
      if (__cerb)
	{
	  const __codecvt_type* __fcvt = &use_facet<__codecvt_type>(__out.getloc());
	  try {
	    streamsize __n = char_traits<char>::length(__s);
	    __char_type __conv[__n];
	    __state_type __state_cur;
	    __char_type __pbuf[__n];	      
	    __char_type* __pend;
	    char* __send;
	    __fcvt->out(__state_cur, 
			__pbuf, __pbuf + __n,
			const_cast<const __char_type*&>(__pend),
			const_cast<char*>(__s), 
			const_cast<char*>(__s + __n),
			__send);
	    __out.write(__pbuf, __n);
	  }
#endif
	  return __out;
	}

  // Partial specializationss
  template<class _Traits>
    basic_ostream<char, _Traits>&
    operator<<(basic_ostream<char, _Traits>& __out, const char* __s)
    {
      typedef basic_ostream<char, _Traits> __ostream_type;
      __ostream_type::sentry __cerb(__out);
      if (__cerb)
	{
	  try {
	    streamsize __w = __out.width();
	    char __pads[__w];
	    streamsize __len = static_cast<streamsize>(_Traits::length(__s));
	    if (__w > __len)
	      {
		_S_pad_char(__out, __pads, __s, __w, __len);
		__s = __pads;
		__len = __w;
	      }
	    __out.write(__s, __len);
	    __out.width(0);
	  }
	  catch(exception& __fail){
	    // 27.6.1.2.1 Common requirements.
	    // Turn this on without causing an ios::failure to be thrown.
	    __out.setstate(ios_base::badbit);
	    if ((__out.exceptions() & ios_base::badbit) != 0)
	      throw;
	  }
	}
      return __out;
    }

  // 21.3.7.8 basic_string::operator<<
  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out,
	       const basic_string<_CharT, _Traits, _Alloc>& __s)
    { return (__out << __s.c_str()); }

} // namespace std
 
// Local Variables:
// mode:C++
// End:










