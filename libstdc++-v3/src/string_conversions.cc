// String Conversions -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <string>
#include <limits>
#include <cerrno>
#include <cstdlib>
#include <cstdarg>

#ifdef _GLIBCXX_USE_C99

_GLIBCXX_BEGIN_NAMESPACE(std)

  // Helper for all the sto* functions.
  template<typename _TRet, typename _Ret = _TRet, typename _CharT,
	   typename... _Base>
    inline _Ret
    __stoa(_TRet (*__convf) (const _CharT*, _CharT**, _Base...),
	   const char* __name, const basic_string<_CharT>& __str,
	   size_t* __idx, _Base... __base)
    {
      _Ret __ret;

      _CharT* __endptr;
      errno = 0;
      const _TRet __tmp = __convf(__str.c_str(), &__endptr, __base...);

      if (__endptr == __str.c_str())
	__throw_invalid_argument(__name);
      else if (errno == ERANGE
	       || (__are_same<_Ret, int>::__value
		   && (__tmp < numeric_limits<_Ret>::min()
		       || __tmp > numeric_limits<_Ret>::max())))
	__throw_out_of_range(__name);
      else
	__ret = __tmp;

      if (__idx)
	*__idx = __endptr - __str.c_str();

      return __ret;
    }

  // Helper for the to_string / to_wstring functions.
  template<typename _CharT>
    inline basic_string<_CharT>
    __to_xstring(int (*__convf) (_CharT*, size_t, const _CharT*, va_list),
		 size_t __n, const _CharT* __fmt, ...)
    {
      // XXX Eventually the result will be constructed in place in
      // the C++0x string, likely with the help of internal hooks.
      _CharT* __s = static_cast<_CharT*>(__builtin_alloca(sizeof(_CharT)
							  * __n));

      va_list __args;
      va_start(__args, __fmt);

      const int __len = __convf(__s, __n, __fmt, __args);

      va_end(__args);

      return basic_string<_CharT>(__s, __s + __len);
    }


  int
  stoi(const string& __str, size_t* __idx, int __base)
  { return std::__stoa<long, int>(&std::strtol, "stoi", __str,
				  __idx, __base); }

  long
  stol(const string& __str, size_t* __idx, int __base)
  { return std::__stoa(&std::strtol, "stol", __str, __idx, __base); }

  unsigned long
  stoul(const string& __str, size_t* __idx, int __base)
  { return std::__stoa(&std::strtoul, "stoul", __str, __idx, __base); }

  long long
  stoll(const string& __str, size_t* __idx, int __base)
  { return std::__stoa(&std::strtoll, "stoll", __str, __idx, __base); }

  unsigned long long
  stoull(const string& __str, size_t* __idx, int __base)
  { return std::__stoa(&std::strtoull, "stoull", __str, __idx, __base); }

  // NB: strtof vs strtod.
  float
  stof(const string& __str, size_t* __idx)
  { return std::__stoa(&std::strtof, "stof", __str, __idx); }

  double
  stod(const string& __str, size_t* __idx)
  { return std::__stoa(&std::strtod, "stod", __str, __idx); }

  long double
  stold(const string& __str, size_t* __idx)
  { return std::__stoa(&std::strtold, "stold", __str, __idx); }

  // NB: (v)snprintf vs sprintf.
  string
  to_string(long long __val)
  { return std::__to_xstring(&std::vsnprintf, 4 * sizeof(long long),
			     "%lld", __val); }

  string
  to_string(unsigned long long __val)
  { return std::__to_xstring(&std::vsnprintf, 4 * sizeof(unsigned long long),
			     "%llu", __val); }

  string
  to_string(long double __val)
  {
    const int __n = numeric_limits<long double>::max_exponent10 + 20;
    return std::__to_xstring(&std::vsnprintf, __n, "%Lf", __val);
  }

#ifdef _GLIBCXX_USE_WCHAR_T
  int 
  stoi(const wstring& __str, size_t* __idx, int __base)
  { return std::__stoa<long, int>(&std::wcstol, "stoi", __str,
				  __idx, __base); }

  long 
  stol(const wstring& __str, size_t* __idx, int __base)
  { return std::__stoa(&std::wcstol, "stol", __str, __idx, __base); }

  unsigned long
  stoul(const wstring& __str, size_t* __idx, int __base)
  { return std::__stoa(&std::wcstoul, "stoul", __str, __idx, __base); }

  long long
  stoll(const wstring& __str, size_t* __idx, int __base)
  { return std::__stoa(&std::wcstoll, "stoll", __str, __idx, __base); }

  unsigned long long
  stoull(const wstring& __str, size_t* __idx, int __base)
  { return std::__stoa(&std::wcstoull, "stoull", __str, __idx, __base); }

  // NB: wcstof vs wcstod.
  float
  stof(const wstring& __str, size_t* __idx)
  { return std::__stoa(&std::wcstof, "stof", __str, __idx); }

  double
  stod(const wstring& __str, size_t* __idx)
  { return std::__stoa(&std::wcstod, "stod", __str, __idx); }

  long double
  stold(const wstring& __str, size_t* __idx)
  { return std::__stoa(&std::wcstold, "stold", __str, __idx); }

  wstring
  to_wstring(long long __val)
  { return std::__to_xstring(&std::vswprintf, 4 * sizeof(long long),
			     L"%lld", __val); }

  wstring
  to_wstring(unsigned long long __val)
  { return std::__to_xstring(&std::vswprintf, 4 * sizeof(unsigned long long),
			     L"%llu", __val); }

  wstring
  to_wstring(long double __val)
  {
    const int __n = numeric_limits<long double>::max_exponent10 + 20;
    return std::__to_xstring(&std::vswprintf, __n, L"%Lf", __val);
  }
#endif

_GLIBCXX_END_NAMESPACE

#endif
