// Input streams operating on strings-*- C++ -*-

// Copyright (C) 2004-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

//
// ISO C++ 14882: 27.6.1  Input streams
//

#ifndef _GLIBCXX_USE_CXX11_ABI
// Instantiations in this file use the new SSO std::string ABI unless included
// by another file which defines _GLIBCXX_USE_CXX11_ABI=0.
# define _GLIBCXX_USE_CXX11_ABI 1
#endif
#include <istream>
#include <string>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template<>
    basic_istream<char>&
    operator>>(basic_istream<char>& __in, basic_string<char>& __str)
    {
      typedef basic_istream<char>       	__istream_type;
      typedef __istream_type::int_type		__int_type;
      typedef __istream_type::traits_type	__traits_type;
      typedef __istream_type::__streambuf_type  __streambuf_type;
      typedef __istream_type::__ctype_type	__ctype_type;
      typedef basic_string<char>        	__string_type;
      typedef __string_type::size_type		__size_type;

      __size_type __extracted = 0;
      ios_base::iostate __err = ios_base::goodbit;
      __istream_type::sentry __cerb(__in, false);
      if (__cerb)
	{
	  __try
	    {
	      __str.erase();
	      const streamsize __w = __in.width();
	      const __size_type __n = __w > 0 ? static_cast<__size_type>(__w)
		                              : __str.max_size();
	      const __ctype_type& __ct = use_facet<__ctype_type>(__in.getloc());
	      const __int_type __eof = __traits_type::eof();
	      __streambuf_type* __sb = __in.rdbuf();
	      __int_type __c = __sb->sgetc();

	      while (__extracted < __n
		     && !__traits_type::eq_int_type(__c, __eof)
		     && !__ct.is(ctype_base::space,
				 __traits_type::to_char_type(__c)))
		{
		  streamsize __size = std::min(streamsize(__sb->egptr()
							  - __sb->gptr()),
					       streamsize(__n - __extracted));
		  if (__size > 1)
		    {
		      __size = (__ct.scan_is(ctype_base::space,
					     __sb->gptr() + 1,
					     __sb->gptr() + __size)
				- __sb->gptr());
		      __str.append(__sb->gptr(), __size);
		      __sb->__safe_gbump(__size);
		      __extracted += __size;
		      __c = __sb->sgetc();
		    }
		  else
		    {
		      __str += __traits_type::to_char_type(__c);
		      ++__extracted;
		      __c = __sb->snextc();
		    }
		}

	      if (__extracted < __n && __traits_type::eq_int_type(__c, __eof))
		__err |= ios_base::eofbit;
	      __in.width(0);
	    }
	  __catch(__cxxabiv1::__forced_unwind&)
	    {
	      __in._M_setstate(ios_base::badbit);
	      __throw_exception_again;
	    }
	  __catch(...)
	    {
	      // _GLIBCXX_RESOLVE_LIB_DEFECTS
	      // 91. Description of operator>> and getline() for string<>
	      // might cause endless loop
	      __in._M_setstate(ios_base::badbit);
	    }
	}
      if (!__extracted)
	__err |= ios_base::failbit;
      if (__err)
	__in.setstate(__err);
      return __in;
    }

  template<>
    basic_istream<char>&
    getline(basic_istream<char>& __in, basic_string<char>& __str,
	    char __delim)
    {
      typedef basic_istream<char>       	__istream_type;
      typedef __istream_type::int_type		__int_type;
      typedef __istream_type::char_type		__char_type;
      typedef __istream_type::traits_type	__traits_type;
      typedef __istream_type::__streambuf_type  __streambuf_type;
      typedef basic_string<char>        	__string_type;
      typedef __string_type::size_type		__size_type;

      __size_type __extracted = 0;
      const __size_type __n = __str.max_size();
      ios_base::iostate __err = ios_base::goodbit;
      __istream_type::sentry __cerb(__in, true);
      if (__cerb)
	{
	  __try
	    {
	      __str.erase();
	      const __int_type __idelim = __traits_type::to_int_type(__delim);
	      const __int_type __eof = __traits_type::eof();
	      __streambuf_type* __sb = __in.rdbuf();
	      __int_type __c = __sb->sgetc();

	      while (__extracted < __n
		     && !__traits_type::eq_int_type(__c, __eof)
		     && !__traits_type::eq_int_type(__c, __idelim))
		{
		  streamsize __size = std::min(streamsize(__sb->egptr()
							  - __sb->gptr()),
					       streamsize(__n - __extracted));
		  if (__size > 1)
		    {
		      const __char_type* __p = __traits_type::find(__sb->gptr(),
								   __size,
								   __delim);
		      if (__p)
			__size = __p - __sb->gptr();
		      __str.append(__sb->gptr(), __size);
		      __sb->__safe_gbump(__size);
		      __extracted += __size;
		      __c = __sb->sgetc();
		    }
		  else
		    {
		      __str += __traits_type::to_char_type(__c);
		      ++__extracted;
		      __c = __sb->snextc();
		    }
		}

	      if (__traits_type::eq_int_type(__c, __eof))
		__err |= ios_base::eofbit;
	      else if (__traits_type::eq_int_type(__c, __idelim))
		{
		  ++__extracted;
		  __sb->sbumpc();
		}
	      else
		__err |= ios_base::failbit;
	    }
	  __catch(__cxxabiv1::__forced_unwind&)
	    {
	      __in._M_setstate(ios_base::badbit);
	      __throw_exception_again;
	    }
	  __catch(...)
	    {
	      // _GLIBCXX_RESOLVE_LIB_DEFECTS
	      // 91. Description of operator>> and getline() for string<>
	      // might cause endless loop
	      __in._M_setstate(ios_base::badbit);
	    }
	}
      if (!__extracted)
	__err |= ios_base::failbit;
      if (__err)
	__in.setstate(__err);
      return __in;
    }

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    basic_istream<wchar_t>&
    getline(basic_istream<wchar_t>& __in, basic_string<wchar_t>& __str,
	    wchar_t __delim)
    {
      typedef basic_istream<wchar_t>       	__istream_type;
      typedef __istream_type::int_type		__int_type;
      typedef __istream_type::char_type		__char_type;
      typedef __istream_type::traits_type	__traits_type;
      typedef __istream_type::__streambuf_type  __streambuf_type;
      typedef basic_string<wchar_t>        	__string_type;
      typedef __string_type::size_type		__size_type;

      __size_type __extracted = 0;
      const __size_type __n = __str.max_size();
      ios_base::iostate __err = ios_base::goodbit;
      __istream_type::sentry __cerb(__in, true);
      if (__cerb)
	{
	  __try
	    {
	      __str.erase();
	      const __int_type __idelim = __traits_type::to_int_type(__delim);
	      const __int_type __eof = __traits_type::eof();
	      __streambuf_type* __sb = __in.rdbuf();
	      __int_type __c = __sb->sgetc();

	      while (__extracted < __n
		     && !__traits_type::eq_int_type(__c, __eof)
		     && !__traits_type::eq_int_type(__c, __idelim))
		{
		  streamsize __size = std::min(streamsize(__sb->egptr()
							  - __sb->gptr()),
					       streamsize(__n - __extracted));
		  if (__size > 1)
		    {
		      const __char_type* __p = __traits_type::find(__sb->gptr(),
								   __size,
								   __delim);
		      if (__p)
			__size = __p - __sb->gptr();
		      __str.append(__sb->gptr(), __size);
		      __sb->__safe_gbump(__size);
		      __extracted += __size;
		      __c = __sb->sgetc();
		    }
		  else
		    {
		      __str += __traits_type::to_char_type(__c);
		      ++__extracted;
		      __c = __sb->snextc();
		    }
		}

	      if (__traits_type::eq_int_type(__c, __eof))
		__err |= ios_base::eofbit;
	      else if (__traits_type::eq_int_type(__c, __idelim))
		{
		  ++__extracted;
		  __sb->sbumpc();
		}
	      else
		__err |= ios_base::failbit;
	    }
	  __catch(__cxxabiv1::__forced_unwind&)
	    {
	      __in._M_setstate(ios_base::badbit);
	      __throw_exception_again;
	    }
	  __catch(...)
	    {
	      // _GLIBCXX_RESOLVE_LIB_DEFECTS
	      // 91. Description of operator>> and getline() for string<>
	      // might cause endless loop
	      __in._M_setstate(ios_base::badbit);
	    }
	}
      if (!__extracted)
	__err |= ios_base::failbit;
      if (__err)
	__in.setstate(__err);
      return __in;
    }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
