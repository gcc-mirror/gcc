// The template and inlines for the -*- C++ -*- slice_array class.

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

#ifndef __SLICE_ARRAY__
#define __SLICE_ARRAY__

extern "C++" {

template<typename _T>
class slice_array
{
public:
    typedef _T value_type;
    
    void operator=   (const valarray<_T>&) const;
    void operator*=  (const valarray<_T>&) const;
    void operator/=  (const valarray<_T>&) const;
    void operator%=  (const valarray<_T>&) const;
    void operator+=  (const valarray<_T>&) const;
    void operator-=  (const valarray<_T>&) const;
    void operator^=  (const valarray<_T>&) const;
    void operator&=  (const valarray<_T>&) const;
    void operator|=  (const valarray<_T>&) const;
    void operator<<= (const valarray<_T>&) const;
    void operator>>= (const valarray<_T>&) const;
    void operator= (const _T &);
    
    template<class _Dom>
    void operator=   (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator*=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator/=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator%=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator+=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator-=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator^=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator&=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator|=  (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator<<= (const _Expr<_Dom,_T>&) const;
    template<class _Dom>
    void operator>>= (const _Expr<_Dom,_T>&) const;
    
private:
    friend class valarray<_T>;
    slice_array(_Array<_T>, const slice&);
    
    const size_t     _M_sz;
    const size_t     _M_stride;
    const _Array<_T> _M_array;
    
    // this constructor is implemented since we need to return a value.
    slice_array (const slice_array&);
    
    // not implemented
    slice_array ();
    slice_array& operator= (const slice_array&);
};

template<typename _T>
inline slice_array<_T>::slice_array (_Array<_T> __a, const slice& __s)
        : _M_sz (__s.size ()), _M_stride (__s.stride ()),
          _M_array (__a.begin () + __s.start ()) {}

template<typename _Tp>
inline slice_array<_Tp>::slice_array(const slice_array<_Tp>& a)
        : _M_sz(a._M_sz), _M_stride(a._M_stride), _M_array(a._M_array) {}

template<typename _T>
inline void
slice_array<_T>::operator= (const _T& __t) 
{ __valarray_fill (_M_array, _M_sz, _M_stride, __t); }

template<typename _T>
inline void
slice_array<_T>::operator= (const valarray<_T>& __v) const
{ __valarray_copy (_Array<_T> (__v), _M_array, _M_sz, _M_stride); }

template<typename _T>
template<class _Dom>
inline void
slice_array<_T>::operator= (const _Expr<_Dom,_T>& __e) const
{ __valarray_copy (__e, _M_sz, _M_array, _M_stride); }

#undef _DEFINE_VALARRAY_OPERATOR
#define _DEFINE_VALARRAY_OPERATOR(op, name)				\
template<typename _T>							\
inline void								\
slice_array<_T>::operator##op##= (const valarray<_T>& __v) const	\
{									\
  _Array_augmented_##name (_M_array, _M_sz, _M_stride, _Array<_T> (__v));\
}									\
									\
template<typename _T> template<class _Dom>				\
inline void								\
slice_array<_T>::operator##op##= (const _Expr<_Dom,_T>& __e) const	\
{									\
    _Array_augmented_##name (_M_array, _M_stride, __e, _M_sz);		\
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
    
#endif // __SLICE_ARRAY__

// Local Variables:
// mode:c++
// End:
