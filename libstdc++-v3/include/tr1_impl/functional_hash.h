// TR1 functional -*- C++ -*-

// Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
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

/** @file tr1_impl/functional_hash.h
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_TR1

  /// Class template hash.
  // Declaration of default hash functor std::tr1::hash.  The types for
  // which std::tr1::hash<T> is well-defined is in clause 6.3.3. of the PDTR.
  template<typename _Tp>
    struct hash : public std::unary_function<_Tp, size_t>
    {
      size_t
      operator()(_Tp __val) const;
    };

  /// Partial specializations for pointer types.
  template<typename _Tp>
    struct hash<_Tp*> : public std::unary_function<_Tp*, size_t>
    {
      size_t
      operator()(_Tp* __p) const
      { return reinterpret_cast<size_t>(__p); }
    };

  /// Explicit specializations for integer types.
#define _TR1_hashtable_define_trivial_hash(_Tp) 	\
  template<>						\
    inline size_t					\
    hash<_Tp>::operator()(_Tp __val) const		\
    { return static_cast<size_t>(__val); }

  _TR1_hashtable_define_trivial_hash(bool);
  _TR1_hashtable_define_trivial_hash(char);
  _TR1_hashtable_define_trivial_hash(signed char);
  _TR1_hashtable_define_trivial_hash(unsigned char);
  _TR1_hashtable_define_trivial_hash(wchar_t);
#ifdef _GLIBCXX_INCLUDE_AS_CXX0X
  _TR1_hashtable_define_trivial_hash(char16_t);
  _TR1_hashtable_define_trivial_hash(char32_t);
#endif
  _TR1_hashtable_define_trivial_hash(short);
  _TR1_hashtable_define_trivial_hash(int);
  _TR1_hashtable_define_trivial_hash(long);
  _TR1_hashtable_define_trivial_hash(long long);
  _TR1_hashtable_define_trivial_hash(unsigned short);
  _TR1_hashtable_define_trivial_hash(unsigned int);
  _TR1_hashtable_define_trivial_hash(unsigned long);
  _TR1_hashtable_define_trivial_hash(unsigned long long);

#undef _TR1_hashtable_define_trivial_hash

  // Fowler / Noll / Vo (FNV) Hash (type FNV-1a)
  // (Used by the next specializations of std::tr1::hash.)

  /// Dummy generic implementation (for sizeof(size_t) != 4, 8).
  template<size_t = sizeof(size_t)>
    struct _Fnv_hash
    {
      static size_t
      hash(const char* __first, size_t __length)
      {
	size_t __result = 0;
	for (; __length > 0; --__length)
	  __result = (__result * 131) + *__first++;
	return __result;
      }
    };

  template<>
    struct _Fnv_hash<4>
    {
      static size_t
      hash(const char* __first, size_t __length)
      {
	size_t __result = static_cast<size_t>(2166136261UL);
	for (; __length > 0; --__length)
	  {
	    __result ^= static_cast<size_t>(*__first++);
	    __result *= static_cast<size_t>(16777619UL);
	  }
	return __result;
      }
    };
  
  template<>
    struct _Fnv_hash<8>
    {
      static size_t
      hash(const char* __first, size_t __length)
      {
	size_t __result =
	  static_cast<size_t>(14695981039346656037ULL);
	for (; __length > 0; --__length)
	  {
	    __result ^= static_cast<size_t>(*__first++);
	    __result *= static_cast<size_t>(1099511628211ULL);
	  }
	return __result;
      }
    };

  /// Explicit specializations for float.
  template<>
    inline size_t
    hash<float>::operator()(float __val) const
    {
      size_t __result = 0;
      
      // 0 and -0 both hash to zero.
      if (__val != 0.0f)
	__result = _Fnv_hash<>::hash(reinterpret_cast<const char*>(&__val),
				     sizeof(__val));
      return __result;
    };

  /// Explicit specializations for double.
  template<>
    inline size_t
    hash<double>::operator()(double __val) const
    {
	size_t __result = 0;

	// 0 and -0 both hash to zero.
	if (__val != 0.0)
	  __result = _Fnv_hash<>::hash(reinterpret_cast<const char*>(&__val),
				       sizeof(__val));
	return __result;
    };

  /// Explicit specializations for long double.
  template<>
    size_t
    hash<long double>::operator()(long double __val) const;

  /// Explicit specialization of member operator for non-builtin types.
  template<>
    size_t
    hash<string>::operator()(string) const;

  template<>
    size_t
    hash<const string&>::operator()(const string&) const;

#ifdef _GLIBCXX_USE_WCHAR_T
  template<>
    size_t
    hash<wstring>::operator()(wstring) const;

  template<>
    size_t
    hash<const wstring&>::operator()(const wstring&) const;
#endif

_GLIBCXX_END_NAMESPACE_TR1
}
