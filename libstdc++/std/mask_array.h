// The template and inlines for the -*- C++ -*- mask_array class.

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

#ifndef __MASK_ARRAY__
#define __MASK_ARRAY__

extern "C++" {

template <class _T> class mask_array
{ 
public:
    typedef _T value_type;
    
    void operator=  (const valarray<_T>&) const;
    void operator*= (const valarray<_T>&) const;
    void operator/= (const valarray<_T>&) const;
    void operator%= (const valarray<_T>&) const;
    void operator+= (const valarray<_T>&) const; 
    void operator-= (const valarray<_T>&) const;
    void operator^= (const valarray<_T>&) const;  
    void operator&= (const valarray<_T>&) const;
    void operator|= (const valarray<_T>&) const;
    void operator<<=(const valarray<_T>&) const;  
    void operator>>=(const valarray<_T>&) const; 
    void operator= (const _T&);
    
    template<class _Dom>
    void operator=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator*= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator/= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator%= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator+= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator-= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator^= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator&= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator|= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator<<=(const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator>>=(const _Expr<_Dom,_T>&) const; 
    
private:
    mask_array (_Array<_T>, size_t, _Array<bool>);
    friend class valarray<_T>;
    
    const size_t       _M_sz;
    const _Array<bool> _M_mask;
    const _Array<_T>   _M_array;
    
    mask_array (const mask_array&);
    
    // not implemented
    mask_array ();
    mask_array& operator= (const mask_array&);
};

template<typename _Tp>
inline mask_array<_Tp>::mask_array (const mask_array<_Tp>& a)
        : _M_sz (a._M_sz), _M_mask (a._M_mask), _M_array (a._M_array) {}

template<typename _T>
inline 
mask_array<_T>::mask_array (_Array<_T> __a, size_t __s, _Array<bool> __m)
        : _M_sz (__s), _M_mask (__m), _M_array (__a) {}

template<typename _T>
inline void
mask_array<_T>::operator= (const _T& __t)
{ __valarray_fill (_M_array, _M_sz, _M_mask, __t); }
    
template<typename _T>
inline void
mask_array<_T>::operator= (const valarray<_T>& __v) const
{ __valarray_copy (_Array<_T> (__v), __v.size (), _M_array, _M_mask); }

template<typename _T>
template<class E>
inline void
mask_array<_T>::operator= (const _Expr<E, _T>& __e) const
{ __valarray_copy (__e, __e.size (), _M_array, _M_mask); }

#undef _DEFINE_VALARRAY_OPERATOR
#define _DEFINE_VALARRAY_OPERATOR(op, name)				\
template<typename _T>							\
inline void								\
mask_array<_T>::operator##op##= (const valarray<_T>& __v) const		\
{									\
  _Array_augmented_##name (_M_array, _M_mask, 				\
                           _Array<_T> (__v), __v.size ());		\
}									\
									\
template<typename _T> template<class E>					\
inline void								\
mask_array<_T>::operator##op##= (const _Expr<E, _T>& __e) const		\
{									\
  _Array_augmented_##name (_M_array, _M_mask, __e, __e.size ());	\
}

_DEFINE_VALARRAY_OPERATOR(*, multiplies)
_DEFINE_VALARRAY_OPERATOR(/, divides)
_DEFINE_VALARRAY_OPERATOR(%, modulus)
_DEFINE_VALARRAY_OPERATOR(+, plus)
_DEFINE_VALARRAY_OPERATOR(-, minus)
_DEFINE_VALARRAY_OPERATOR(^, xor)
_DEFINE_VALARRAY_OPERATOR(&, and)
_DEFINE_VALARRAY_OPERATOR(|, or)
_DEFINE_VALARRAY_OPERATOR(<<, shift_left)
_DEFINE_VALARRAY_OPERATOR(>>, shift_right)

#undef _DEFINE_VALARRAY_OPERATOR    

} // extern "C++"
    
#endif // __MASK_ARRAY__ 

// Local Variables:
// mode:c++
// End:
