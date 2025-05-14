// The template and inlines for the -*- C++ -*- internal _Array helper class.

// Copyright (C) 1997-2025 Free Software Foundation, Inc.
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

/** @file bits/valarray_array.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{valarray}
 */

// Written by Gabriel Dos Reis <Gabriel.Dos-Reis@DPTMaths.ENS-Cachan.Fr>

#ifndef _VALARRAY_ARRAY_H
#define _VALARRAY_ARRAY_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/c++config.h>
#include <bits/cpp_type_traits.h>
#include <cstdlib>
#include <new>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  //
  // Helper functions on raw pointers
  //

  // We get memory the old fashioned way
  template<typename _Tp>
    _Tp*
    __valarray_get_storage(size_t) __attribute__((__malloc__));

  template<typename _Tp>
    inline _Tp*
    __valarray_get_storage(size_t __n)
    { return static_cast<_Tp*>(operator new(__n * sizeof(_Tp))); }

  // Return memory to the system
  inline void
  __valarray_release_memory(void* __p)
  { operator delete(__p); }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++17-extensions" // if constexpr

  // Turn raw-memory into an array of _Tp filled with _Tp().
  // This is used in `valarray<T> v(n);` and in `valarray<T>::shift(n)`.
  template<typename _Tp>
    inline void
    __valarray_default_construct(_Tp* __b, _Tp* __e)
    {
      if _GLIBCXX_CONSTEXPR (__is_trivial(_Tp))
	__builtin_memset(__b, 0, (__e - __b) * sizeof(_Tp));
      else
	while (__b != __e)
	  ::new(static_cast<void*>(__b++)) _Tp();
    }

  // Turn a raw-memory into an array of _Tp filled with __t
  // This is the required in valarray<T> v(n, t).  Also
  // used in valarray<>::resize().
  template<typename _Tp>
    inline void
    __valarray_fill_construct(_Tp* __b, _Tp* __e, const _Tp __t)
    {
      while (__b != __e)
	::new(static_cast<void*>(__b++)) _Tp(__t);
    }

  // copy-construct raw array [__o, *) from plain array [__b, __e)
  template<typename _Tp>
    inline void
    __valarray_copy_construct(const _Tp* __b, const _Tp* __e,
			      _Tp* __restrict__ __o)
    {
      if _GLIBCXX_CONSTEXPR (__is_trivial(_Tp))
	{
	  if (__b)
	    __builtin_memcpy(__o, __b, (__e - __b) * sizeof(_Tp));
	}
      else
	while (__b != __e)
	  ::new(static_cast<void*>(__o++)) _Tp(*__b++);
    }

  // copy-construct raw array [__o, *) from strided array __a[<__n : __s>]
  template<typename _Tp>
    inline void
    __valarray_copy_construct (const _Tp* __restrict__ __a, size_t __n,
			       size_t __s, _Tp* __restrict__ __o)
    {
      if _GLIBCXX_CONSTEXPR (__is_trivial(_Tp))
	while (__n--)
	  {
	    *__o++ = *__a;
	    __a += __s;
	  }
      else
	while (__n--)
	  {
	    new(__o++) _Tp(*__a);
	    __a += __s;
	  }
    }

  // copy-construct raw array [__o, *) from indexed array __a[__i[<__n>]]
  template<typename _Tp>
    inline void
    __valarray_copy_construct (const _Tp* __restrict__ __a,
			       const size_t* __restrict__ __i,
			       _Tp* __restrict__ __o, size_t __n)
    {
      if _GLIBCXX_CONSTEXPR (__is_trivial(_Tp))
	while (__n--)
	  *__o++ = __a[*__i++];
      else
	while (__n--)
	  new (__o++) _Tp(__a[*__i++]);
    }

  // Do the necessary cleanup when we're done with arrays.
  template<typename _Tp>
    inline void
    __valarray_destroy_elements(_Tp* __b, _Tp* __e)
    {
      if _GLIBCXX_CONSTEXPR (!__is_trivial(_Tp))
	while (__b != __e)
	  {
	    __b->~_Tp();
	    ++__b;
	  }
    }

#pragma GCC diagnostic pop

  // Fill a plain array __a[<__n>] with __t
  template<typename _Tp>
    inline void
    __valarray_fill(_Tp* __restrict__ __a, size_t __n, const _Tp& __t)
    {
      while (__n--)
	*__a++ = __t;
    }

  // fill strided array __a[<__n-1 : __s>] with __t
  template<typename _Tp>
    inline void
    __valarray_fill(_Tp* __restrict__ __a, size_t __n,
		    size_t __s, const _Tp& __t)
    {
      for (size_t __i = 0; __i < __n; ++__i, __a += __s)
	*__a = __t;
    }

  // fill indirect array __a[__i[<__n>]] with __i
  template<typename _Tp>
    inline void
    __valarray_fill(_Tp* __restrict__ __a, const size_t* __restrict__ __i,
		    size_t __n, const _Tp& __t)
    {
      for (size_t __j = 0; __j < __n; ++__j, ++__i)
	__a[*__i] = __t;
    }

  // copy plain array __a[<__n>] in __b[<__n>]
  // For non-fundamental types, it is wrong to say 'memcpy()'
  template<typename _Tp, bool>
    struct _Array_copier
    {
      inline static void
      _S_do_it(const _Tp* __restrict__ __a, size_t __n, _Tp* __restrict__ __b)
      {
	while(__n--)
	  *__b++ = *__a++;
      }
    };

  template<typename _Tp>
    struct _Array_copier<_Tp, true>
    {
      inline static void
      _S_do_it(const _Tp* __restrict__ __a, size_t __n, _Tp* __restrict__ __b)
      {
	if (__n != 0)
	  __builtin_memcpy(__b, __a, __n * sizeof (_Tp));
      }
    };

  // Copy a plain array __a[<__n>] into a play array __b[<>]
  template<typename _Tp>
    inline void
    __valarray_copy(const _Tp* __restrict__ __a, size_t __n,
		    _Tp* __restrict__ __b)
    {
      _Array_copier<_Tp, __is_trivial(_Tp)>::_S_do_it(__a, __n, __b);
    }

  // Copy strided array __a[<__n : __s>] in plain __b[<__n>]
  template<typename _Tp>
    inline void
    __valarray_copy(const _Tp* __restrict__ __a, size_t __n, size_t __s,
		    _Tp* __restrict__ __b)
    {
      for (size_t __i = 0; __i < __n; ++__i, ++__b, __a += __s)
	*__b = *__a;
    }

  // Copy a plain array  __a[<__n>] into a strided array __b[<__n : __s>]
  template<typename _Tp>
    inline void
    __valarray_copy(const _Tp* __restrict__ __a, _Tp* __restrict__ __b,
		    size_t __n, size_t __s)
    {
      for (size_t __i = 0; __i < __n; ++__i, ++__a, __b += __s)
	*__b = *__a;
    }

  // Copy strided array __src[<__n : __s1>] into another
  // strided array __dst[< : __s2>].  Their sizes must match.
  template<typename _Tp>
    inline void
    __valarray_copy(const _Tp* __restrict__ __src, size_t __n, size_t __s1,
		    _Tp* __restrict__ __dst, size_t __s2)
    {
      for (size_t __i = 0; __i < __n; ++__i)
	__dst[__i * __s2] = __src[__i * __s1];
    }

  // Copy an indexed array __a[__i[<__n>]] in plain array __b[<__n>]
  template<typename _Tp>
    inline void
    __valarray_copy(const _Tp* __restrict__ __a,
		    const size_t* __restrict__ __i,
		    _Tp* __restrict__ __b, size_t __n)
    {
      for (size_t __j = 0; __j < __n; ++__j, ++__b, ++__i)
	*__b = __a[*__i];
    }

  // Copy a plain array __a[<__n>] in an indexed array __b[__i[<__n>]]
  template<typename _Tp>
    inline void
    __valarray_copy(const _Tp* __restrict__ __a, size_t __n,
		    _Tp* __restrict__ __b, const size_t* __restrict__ __i)
    {
      for (size_t __j = 0; __j < __n; ++__j, ++__a, ++__i)
	__b[*__i] = *__a;
    }

  // Copy the __n first elements of an indexed array __src[<__i>] into
  // another indexed array __dst[<__j>].
  template<typename _Tp>
    inline void
    __valarray_copy(const _Tp* __restrict__ __src, size_t __n,
		    const size_t* __restrict__ __i,
		    _Tp* __restrict__ __dst, const size_t* __restrict__ __j)
    {
      for (size_t __k = 0; __k < __n; ++__k)
	__dst[*__j++] = __src[*__i++];
    }

  //
  // Compute the sum of elements in range [__f, __l) which must not be empty.
  // This is a naive algorithm.  It suffers from cancelling.
  // In the future try to specialize for _Tp = float, double, long double
  // using a more accurate algorithm.
  //
  template<typename _Tp>
    inline _Tp
    __valarray_sum(const _Tp* __f, const _Tp* __l)
    {
      _Tp __r = *__f++;
      while (__f != __l)
	__r += *__f++;
      return __r;
    }

  // Compute the min/max of an array-expression
  template<typename _Ta>
    inline typename _Ta::value_type
    __valarray_min(const _Ta& __a)
    {
      size_t __s = __a.size();
      typedef typename _Ta::value_type _Value_type;
      _Value_type __r = __s == 0 ? _Value_type() : __a[0];
      for (size_t __i = 1; __i < __s; ++__i)
	{
	  _Value_type __t = __a[__i];
	  if (__t < __r)
	    __r = __t;
	}
      return __r;
    }

  template<typename _Ta>
    inline typename _Ta::value_type
    __valarray_max(const _Ta& __a)
    {
      size_t __s = __a.size();
      typedef typename _Ta::value_type _Value_type;
      _Value_type __r = __s == 0 ? _Value_type() : __a[0];
      for (size_t __i = 1; __i < __s; ++__i)
	{
	  _Value_type __t = __a[__i];
	  if (__t > __r)
	    __r = __t;
	}
      return __r;
    }

  //
  // Helper class _Array, first layer of valarray abstraction.
  // All operations on valarray should be forwarded to this class
  // whenever possible. -- gdr
  //

  template<typename _Tp>
    struct _Array
    {
      explicit _Array(_Tp* const __restrict__);
      explicit _Array(const valarray<_Tp>&);
      _Array(const _Tp* __restrict__, size_t);

      _Tp* begin() const;

      _Tp* const __restrict__ _M_data;
    };


  // Copy-construct plain array __b[<__n>] from indexed array __a[__i[<__n>]]
  template<typename _Tp>
    inline void
    __valarray_copy_construct(_Array<_Tp> __a, _Array<size_t> __i,
			      _Array<_Tp> __b, size_t __n)
    { std::__valarray_copy_construct(__a._M_data, __i._M_data,
				     __b._M_data, __n); }

  // Copy-construct plain array __b[<__n>] from strided array __a[<__n : __s>]
  template<typename _Tp>
    inline void
    __valarray_copy_construct(_Array<_Tp> __a, size_t __n, size_t __s,
			      _Array<_Tp> __b)
    { std::__valarray_copy_construct(__a._M_data, __n, __s, __b._M_data); }

  template<typename _Tp>
    inline void
    __valarray_fill (_Array<_Tp> __a, size_t __n, const _Tp& __t)
    { std::__valarray_fill(__a._M_data, __n, __t); }

  template<typename _Tp>
    inline void
    __valarray_fill(_Array<_Tp> __a, size_t __n, size_t __s, const _Tp& __t)
    { std::__valarray_fill(__a._M_data, __n, __s, __t); }

  template<typename _Tp>
    inline void
    __valarray_fill(_Array<_Tp> __a, _Array<size_t> __i,
		    size_t __n, const _Tp& __t)
    { std::__valarray_fill(__a._M_data, __i._M_data, __n, __t); }

  // Copy a plain array __a[<__n>] into a play array __b[<>]
  template<typename _Tp>
    inline void
    __valarray_copy(_Array<_Tp> __a, size_t __n, _Array<_Tp> __b)
    { std::__valarray_copy(__a._M_data, __n, __b._M_data); }

  // Copy strided array __a[<__n : __s>] in plain __b[<__n>]
  template<typename _Tp>
    inline void
    __valarray_copy(_Array<_Tp> __a, size_t __n, size_t __s, _Array<_Tp> __b)
    { std::__valarray_copy(__a._M_data, __n, __s, __b._M_data); }

  // Copy a plain array  __a[<__n>] into a strided array __b[<__n : __s>]
  template<typename _Tp>
    inline void
    __valarray_copy(_Array<_Tp> __a, _Array<_Tp> __b, size_t __n, size_t __s)
    { __valarray_copy(__a._M_data, __b._M_data, __n, __s); }

  // Copy strided array __src[<__n : __s1>] into another
  // strided array __dst[< : __s2>].  Their sizes must match.
  template<typename _Tp>
    inline void
    __valarray_copy(_Array<_Tp> __a, size_t __n, size_t __s1,
                    _Array<_Tp> __b, size_t __s2)
    { std::__valarray_copy(__a._M_data, __n, __s1, __b._M_data, __s2); }

  // Copy an indexed array __a[__i[<__n>]] in plain array __b[<__n>]
  template<typename _Tp>
    inline void
    __valarray_copy(_Array<_Tp> __a, _Array<size_t> __i,
		    _Array<_Tp> __b, size_t __n)
    { std::__valarray_copy(__a._M_data, __i._M_data, __b._M_data, __n); }

  // Copy a plain array __a[<__n>] in an indexed array __b[__i[<__n>]]
  template<typename _Tp>
    inline void
    __valarray_copy(_Array<_Tp> __a, size_t __n, _Array<_Tp> __b,
		    _Array<size_t> __i)
    { std::__valarray_copy(__a._M_data, __n, __b._M_data, __i._M_data); }

  // Copy the __n first elements of an indexed array __src[<__i>] into
  // another indexed array __dst[<__j>].
  template<typename _Tp>
    inline void
    __valarray_copy(_Array<_Tp> __src, size_t __n, _Array<size_t> __i,
                    _Array<_Tp> __dst, _Array<size_t> __j)
    {
      std::__valarray_copy(__src._M_data, __n, __i._M_data,
		    __dst._M_data, __j._M_data);
    }

  template<typename _Tp>
    inline
    _Array<_Tp>::_Array(_Tp* const __restrict__ __p)
    : _M_data (__p) {}

  template<typename _Tp>
    inline
    _Array<_Tp>::_Array(const valarray<_Tp>& __v)
    : _M_data (__v._M_data) {}

  template<typename _Tp>
    inline
    _Array<_Tp>::_Array(const _Tp* __restrict__ __b, size_t __s)
    : _M_data(__valarray_get_storage<_Tp>(__s))
    { std::__valarray_copy_construct(__b, __s, _M_data); }

  template<typename _Tp>
    inline _Tp*
    _Array<_Tp>::begin () const
    { return _M_data; }

#define _DEFINE_ARRAY_FUNCTION(_Op, _Name)				\
  template<typename _Tp>		        			\
    inline void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, size_t __n, const _Tp& __t) \
    {									\
      for (_Tp* __p = __a._M_data; __p < __a._M_data + __n; ++__p)	\
        *__p _Op##= __t;						\
    }									\
									\
  template<typename _Tp>						\
    inline void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, size_t __n, _Array<_Tp> __b) \
    {									\
      _Tp* __p = __a._M_data;						\
      for (_Tp* __q = __b._M_data; __q < __b._M_data + __n; ++__p, ++__q) \
        *__p _Op##= *__q;						\
    }									\
									\
  template<typename _Tp, class _Dom>					\
    void								\
    _Array_augmented_##_Name(_Array<_Tp> __a,	        		\
                             const _Expr<_Dom, _Tp>& __e, size_t __n)	\
    {									\
      _Tp* __p(__a._M_data);						\
      for (size_t __i = 0; __i < __n; ++__i, ++__p)                     \
        *__p _Op##= __e[__i];                                          	\
    }									\
									\
  template<typename _Tp>						\
    inline void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, size_t __n, size_t __s,	\
	                     _Array<_Tp> __b)				\
    {									\
      _Tp* __q(__b._M_data);						\
      for (_Tp* __p = __a._M_data; __p < __a._M_data + __s * __n;       \
	   __p += __s, ++__q)                                           \
        *__p _Op##= *__q;						\
    }									\
									\
  template<typename _Tp>						\
    inline void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, _Array<_Tp> __b,		\
		             size_t __n, size_t __s)			\
    {									\
      _Tp* __q(__b._M_data);						\
      for (_Tp* __p = __a._M_data; __p < __a._M_data + __n;             \
	   ++__p, __q += __s)                                           \
        *__p _Op##= *__q;						\
    }									\
									\
  template<typename _Tp, class _Dom>					\
    void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, size_t __s,		\
                             const _Expr<_Dom, _Tp>& __e, size_t __n)	\
    {									\
      _Tp* __p(__a._M_data);						\
      for (size_t __i = 0; __i < __n; ++__i, __p += __s)                \
        *__p _Op##= __e[__i];                                          	\
    }									\
									\
  template<typename _Tp>						\
    inline void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, _Array<size_t> __i,	\
                             _Array<_Tp> __b, size_t __n)		\
    {									\
      _Tp* __q(__b._M_data);						\
      for (size_t* __j = __i._M_data; __j < __i._M_data + __n;          \
           ++__j, ++__q)                                                \
        __a._M_data[*__j] _Op##= *__q;					\
    }									\
									\
  template<typename _Tp>						\
    inline void					        		\
    _Array_augmented_##_Name(_Array<_Tp> __a, size_t __n,		\
                             _Array<_Tp> __b, _Array<size_t> __i)	\
    {									\
      _Tp* __p(__a._M_data);						\
      for (size_t* __j = __i._M_data; __j<__i._M_data + __n;            \
	   ++__j, ++__p)                                                \
        *__p _Op##= __b._M_data[*__j];					\
    }									\
									\
  template<typename _Tp, class _Dom>					\
    void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, _Array<size_t> __i,	\
                             const _Expr<_Dom, _Tp>& __e, size_t __n)	\
    {									\
      size_t* __j(__i._M_data);	        				\
      for (size_t __k = 0; __k<__n; ++__k, ++__j)			\
        __a._M_data[*__j] _Op##= __e[__k];				\
    }									\
									\
  template<typename _Tp>						\
    void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, _Array<bool> __m,         \
                             _Array<_Tp> __b, size_t __n)		\
    {									\
      bool* __ok(__m._M_data);						\
      _Tp* __p(__a._M_data);						\
      for (_Tp* __q = __b._M_data; __q < __b._M_data + __n;             \
	   ++__q, ++__ok, ++__p)                                        \
        {                                                               \
          while (! *__ok)                                               \
            {						        	\
              ++__ok;							\
              ++__p;							\
            }								\
          *__p _Op##= *__q;						\
        }								\
    }									\
									\
  template<typename _Tp>						\
    void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, size_t __n,		\
                             _Array<_Tp> __b, _Array<bool> __m)   	\
    {									\
      bool* __ok(__m._M_data);						\
      _Tp* __q(__b._M_data);						\
      for (_Tp* __p = __a._M_data; __p < __a._M_data + __n;             \
	   ++__p, ++__ok, ++__q)                                        \
        {                                                               \
          while (! *__ok)                                               \
            {					        		\
              ++__ok;							\
              ++__q;							\
            }								\
          *__p _Op##= *__q;						\
        }								\
    }									\
									\
  template<typename _Tp, class _Dom>					\
    void								\
    _Array_augmented_##_Name(_Array<_Tp> __a, _Array<bool> __m,  	\
                             const _Expr<_Dom, _Tp>& __e, size_t __n)	\
    {									\
      bool* __ok(__m._M_data);						\
      _Tp* __p(__a._M_data);						\
      for (size_t __i = 0; __i < __n; ++__i, ++__ok, ++__p)             \
        {	                                           		\
          while (! *__ok)                                               \
            {		         					\
	      ++__ok;							\
              ++__p;							\
            }								\
          *__p _Op##= __e[__i];						\
        }								\
    }

   _DEFINE_ARRAY_FUNCTION(+, __plus)
   _DEFINE_ARRAY_FUNCTION(-, __minus)
   _DEFINE_ARRAY_FUNCTION(*, __multiplies)
   _DEFINE_ARRAY_FUNCTION(/, __divides)
   _DEFINE_ARRAY_FUNCTION(%, __modulus)
   _DEFINE_ARRAY_FUNCTION(^, __bitwise_xor)
   _DEFINE_ARRAY_FUNCTION(|, __bitwise_or)
   _DEFINE_ARRAY_FUNCTION(&, __bitwise_and)
   _DEFINE_ARRAY_FUNCTION(<<, __shift_left)
   _DEFINE_ARRAY_FUNCTION(>>, __shift_right)

#undef _DEFINE_ARRAY_FUNCTION

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

# include <bits/valarray_array.tcc>

#endif /* _ARRAY_H */
