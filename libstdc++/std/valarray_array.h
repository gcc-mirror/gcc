// The template and inlines for the -*- C++ -*- internal _Array helper class.

// Copyright (C) 1997-1999 Cygnus Solutions
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

// Written by Gabriel Dos Reis <Gabriel.Dos-Reis@DPTMaths.ENS-Cachan.Fr>

#ifndef __VALARRAY_ARRAY__
#define __VALARRAY_ARRAY__

#include <cstdlib>
#include <cstring>

extern "C++" {

//
// Helper functions on raw pointers
//

// fill plain array __a[<__n>] with __t
template<typename _Tp>
inline void
__valarray_fill (_Tp* __restrict__ __a, size_t __n, const _Tp& __t)
{ while (__n--) *__a++ = __t; }

// fill strided array __a[<__n-1 : __s>] with __t
template<typename _Tp>
inline void
__valarray_fill (_Tp* __restrict__ __a, size_t __n,
                 size_t __s, const _Tp& __t)
{ for (size_t __i=0; __i<__n; ++__i, __a+=__s) *__a = __t; }

// fill indirect array __a[__i[<__n>]] with __i
template<typename _Tp>
inline void
__valarray_fill(_Tp* __restrict__ __a, const size_t* __restrict__ __i,
                size_t __n, const _Tp& __t)
{ for (size_t __j=0; __j<__n; ++__j, ++__i) __a[*__i] = __t; }

// copy plain array __a[<__n>] in __b[<__n>]
template<typename _Tp>
inline void
__valarray_copy (const _Tp* __restrict__ __a, size_t __n,
                 _Tp* __restrict__ __b)
{ memcpy (__b, __a, __n * sizeof(_Tp)); }

// copy strided array __a[<__n : __s>] in plain __b[<__n>]
template<typename _Tp>
inline void
__valarray_copy (const _Tp* __restrict__ __a, size_t __n, size_t __s,
                 _Tp* __restrict__ __b)
{ for (size_t __i=0; __i<__n; ++__i, ++__b, __a += __s) *__b += *__a; }

// copy plain __a[<__n>] in strided __b[<__n : __s>]
template<typename _Tp>
inline void
__valarray_copy (const _Tp* __restrict__ __a, _Tp* __restrict__ __b,
                 size_t __n, size_t __s)
{ for (size_t __i=0; __i<__n; ++__i, ++__a, __b+=__s) *__b = *__a; }

// copy indexed __a[__i[<__n>]] in plain __b[<__n>]
template<typename _Tp>
inline void
__valarray_copy (const _Tp* __restrict__ __a,
                 const size_t* __restrict__ __i,
                 _Tp* __restrict__ __b, size_t __n)
{ for (size_t __j=0; __j<__n; ++__j, ++__b, ++__i) *__b = __a[*__i]; }

// copy plain __a[<__n>] in indexed __b[__i[<__n>]]
template<typename _Tp>
inline void
__valarray_copy (const _Tp* __restrict__ __a, size_t __n,
                 _Tp* __restrict__ __b, const size_t* __restrict__ __i)
{ for (size_t __j=0; __j<__n; ++__j, ++__a, ++__i) __b[*__i] = *__a; }

//
// Helper class _Array, first layer of valarray abstraction.
// All operations on valarray should be forwarded to this class
// whenever possible. -- gdr
//

template<typename _Tp> struct _Array {
    
    explicit _Array (size_t);
    explicit _Array (_Tp* const __restrict__);
    explicit _Array (const valarray<_Tp>&);
    _Array (const _Tp* __restrict__, size_t);
    
    void free_data() const;
    _Tp* begin () const;
    
    _Tp* const __restrict__ _M_data;
};

template<typename _Tp>
inline void
__valarray_fill (_Array<_Tp> __a, size_t __n, const _Tp& __t)
{ __valarray_fill (__a._M_data, __n, __t); }

template<typename _Tp>
inline void
__valarray_fill (_Array<_Tp> __a, size_t __n, size_t __s, const _Tp& __t)
{ __valarray_fill (__a._M_data, __n, __s, __t); }

template<typename _Tp>
inline void
__valarray_fill (_Array<_Tp> __a, _Array<size_t> __i, 
                 size_t __n, const _Tp& __t)
{ __valarray_fill (__a._M_data, __i._M_data, __n, __t); }

template<typename _Tp>
inline void
__valarray_copy (_Array<_Tp> __a, size_t __n, _Array<_Tp> __b)
{ __valarray_copy (__a._M_data, __n, __b._M_data); }

template<typename _Tp>
inline void
__valarray_copy (_Array<_Tp> __a, size_t __n, size_t __s, _Array<_Tp> __b)
{ __valarray_copy(__a._M_data, __n, __s, __b._M_data); }

template<typename _Tp>
inline void
__valarray_copy (_Array<_Tp> __a, _Array<_Tp> __b, size_t __n, size_t __s)
{ __valarray_copy (__a._M_data, __b._M_data, __n, __s); }

template<typename _Tp>
inline void
__valarray_copy (_Array<_Tp> __a, _Array<size_t> __i, 
                 _Array<_Tp> __b, size_t __n)
{ __valarray_copy (__a._M_data, __i._M_data, __b._M_data, __n); }

template<typename _Tp>
inline void
__valarray_copy (_Array<_Tp> __a, size_t __n, _Array<_Tp> __b, 
                 _Array<size_t> __i)
{ __valarray_copy (__a._M_data, __n, __b._M_data, __i._M_data); }

template<typename _Tp>
inline
_Array<_Tp>::_Array (size_t __n) : _M_data (new _Tp[__n]) {}

template<typename _Tp>
inline
_Array<_Tp>::_Array (_Tp* const __restrict__ __p) : _M_data (__p) {}

template<typename _Tp>
inline _Array<_Tp>::_Array (const valarray<_Tp>& __v) 
        : _M_data (__v._M_data) {}

template<typename _Tp>
inline
_Array<_Tp>::_Array (const _Tp* __restrict__ __b, size_t __s) 
        : _M_data (new _Tp[__s]) { __valarray_copy (__b, __s, _M_data); }

template<typename _Tp>
inline void
_Array<_Tp>::free_data() const { delete[] _M_data; }

template<typename _Tp>
inline _Tp*
_Array<_Tp>::begin () const
{ return _M_data; }

#define _DEFINE_ARRAY_FUNCTION(_Op, _Name)				\
template<typename _Tp>							\
inline void								\
_Array_augmented_##_Name (_Array<_Tp> __a, size_t __n, const _Tp& __t)	\
{									\
    for (_Tp* __p=__a._M_data; __p<__a._M_data+__n; ++__p) 		\
      *__p _Op##= __t;							\
}									\
									\
template<typename _Tp>							\
inline void								\
_Array_augmented_##_Name (_Array<_Tp> __a, size_t __n, _Array<_Tp> __b)	\
{									\
    _Tp* __p (__a._M_data);						\
    for (_Tp* __q=__b._M_data; __q<__b._M_data+__n; ++__p, ++__q) 	\
      *__p _Op##= *__q;							\
}									\
									\
template<typename _Tp, class _Dom>					\
void									\
_Array_augmented_##_Name (_Array<_Tp> __a, 				\
                         const _Expr<_Dom,_Tp>& __e, size_t __n)	\
{									\
    _Tp* __p (__a._M_data);						\
    for (size_t __i=0; __i<__n; ++__i, ++__p) *__p _Op##= __e[__i];	\
}									\
									\
template<typename _Tp>							\
inline void								\
_Array_augmented_##_Name (_Array<_Tp> __a, size_t __n, size_t __s, 	\
			 _Array<_Tp> __b)				\
{					       				\
    _Tp* __q (__b._M_data);						\
    for (_Tp* __p=__a._M_data; __p<__a._M_data+__s*__n; __p+=__s, ++__q) \
      *__p _Op##= *__q;							\
}									\
									\
template<typename _Tp>							\
inline void								\
_Array_augmented_##_Name (_Array<_Tp> __a, _Array<_Tp> __b, 		\
			 size_t __n, size_t __s)			\
{									\
    _Tp* __q (__b._M_data);						\
    for (_Tp* __p=__a._M_data; __p<__a._M_data+__n; ++__p, __q+=__s)	\
      *__p _Op##= *__q;							\
}									\
									\
template<typename _Tp, class _Dom>					\
void									\
_Array_augmented_##_Name (_Array<_Tp> __a, size_t __s,			\
                          const _Expr<_Dom,_Tp>& __e, size_t __n)	\
{									\
    _Tp* __p (__a._M_data);						\
    for (size_t __i=0; __i<__n; ++__i, __p+=__s) *__p _Op##= __e[__i];	\
}									\
									\
template<typename _Tp>							\
inline void								\
_Array_augmented_##_Name (_Array<_Tp> __a, _Array<size_t> __i,		\
                          _Array<_Tp> __b, size_t __n)			\
{									\
    _Tp* __q (__b._M_data);						\
    for (size_t* __j=__i._M_data; __j<__i._M_data+__n; ++__j, ++__q)	\
        __a._M_data[*__j] _Op##= *__q;					\
}									\
									\
template<typename _Tp>							\
inline void								\
_Array_augmented_##_Name (_Array<_Tp> __a, size_t __n,			\
                          _Array<_Tp> __b, _Array<size_t> __i)		\
{									\
    _Tp* __p (__a._M_data);						\
    for (size_t* __j=__i._M_data; __j<__i._M_data+__n; ++__j, ++__p)	\
        *__p _Op##= __b._M_data[*__j];					\
}									\
									\
template<typename _Tp, class _Dom>					\
void									\
_Array_augmented_##_Name (_Array<_Tp> __a, _Array<size_t> __i,		\
                          const _Expr<_Dom, _Tp>& __e, size_t __n)	\
{									\
    size_t* __j (__i._M_data);						\
    for (size_t __k=0; __k<__n; ++__k, ++__j) 				\
      __a._M_data[*__j] _Op##= __e[__k];				\
}									\
									\
template<typename _Tp>							\
void									\
_Array_augmented_##_Name (_Array<_Tp> __a, _Array<bool> __m,		\
                          _Array<_Tp> __b, size_t __n)			\
{									\
    bool* ok (__m._M_data);						\
    _Tp* __p (__a._M_data);						\
    for (_Tp* __q=__b._M_data; __q<__b._M_data+__n; ++__q, ++ok, ++__p) { \
        while (! *ok) {							\
            ++ok;							\
            ++__p;							\
        }								\
        *__p _Op##= *__q;						\
    }									\
}									\
									\
template<typename _Tp>							\
void									\
_Array_augmented_##_Name (_Array<_Tp> __a, size_t __n,			\
                         _Array<_Tp> __b, _Array<bool> __m)		\
{									\
    bool* ok (__m._M_data);						\
    _Tp* __q (__b._M_data);						\
    for (_Tp* __p=__a._M_data; __p<__a._M_data+__n; ++__p, ++ok, ++__q) { \
        while (! *ok) {							\
            ++ok;							\
            ++__q;							\
        }								\
        *__p _Op##= *__q;						\
    }									\
}									\
									\
template<typename _Tp, class _Dom>					\
void									\
_Array_augmented_##_Name (_Array<_Tp> __a, _Array<bool> __m,		\
                          const _Expr<_Dom, _Tp>& __e, size_t __n)	\
{									\
    bool* ok(__m._M_data);						\
    _Tp* __p (__a._M_data);						\
    for (size_t __i=0; __i<__n; ++__i, ++ok, ++__p) {			\
        while (! *ok) {							\
            ++ok;							\
            ++__p;							\
        }								\
        *__p _Op##= __e[__i];						\
    }									\
}

_DEFINE_ARRAY_FUNCTION(+, plus)
_DEFINE_ARRAY_FUNCTION(-, minus)
_DEFINE_ARRAY_FUNCTION(*, multiplies)
_DEFINE_ARRAY_FUNCTION(/, divides)
_DEFINE_ARRAY_FUNCTION(%, modulus)
_DEFINE_ARRAY_FUNCTION(^, xor)
_DEFINE_ARRAY_FUNCTION(|, or)
_DEFINE_ARRAY_FUNCTION(&, and)    
_DEFINE_ARRAY_FUNCTION(<<, shift_left)
_DEFINE_ARRAY_FUNCTION(>>, shift_right)

#undef _DEFINE_ARRAY_FUNCTION    

} // extern "C++"
    
#ifdef _G_NO_VALARRAY_TEMPLATE_EXPORT
# define export 
# include <std/valarray_array.tcc>    
#endif
           
#endif // __VALARRAY_ARRAY__

// Local Variables:
// mode:c++
// End:
