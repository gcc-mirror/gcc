// The template and inlines for the -*- C++ -*- gslice_array class.

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

#ifndef __GSLICE_ARRAY__
#define __GSLICE_ARRAY__

extern "C++" {

template<typename _Tp> class gslice_array
{
public:
    typedef _Tp value_type;
    
    void operator=  (const valarray<_Tp>&) const;
    void operator*= (const valarray<_Tp>&) const;
    void operator/= (const valarray<_Tp>&) const;
    void operator%= (const valarray<_Tp>&) const;
    void operator+= (const valarray<_Tp>&) const;
    void operator-= (const valarray<_Tp>&) const;
    void operator^= (const valarray<_Tp>&) const;
    void operator&= (const valarray<_Tp>&) const;
    void operator|= (const valarray<_Tp>&) const;
    void operator<<=(const valarray<_Tp>&) const;
    void operator>>=(const valarray<_Tp>&) const;
    void operator=(const _Tp&);
    
    template<class _Dom>
    void operator= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator*= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator/= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator%= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator+= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator-= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator^= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator&= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator|= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator<<= (const _Expr<_Dom,_Tp>&) const;
    template<class _Dom>
    void operator>>= (const _Expr<_Dom,_Tp>&) const;
    
private:
    _Array<_Tp>    _M_array;
    const valarray<size_t>& _M_index;
    
    friend class valarray<_Tp>;
    
    gslice_array (_Array<_Tp>, const valarray<size_t>&);
    
    // this constructor needs to be implemented.
    gslice_array (const gslice_array&);
    
    // not implemented
    gslice_array();
    gslice_array& operator= (const gslice_array&);
};

template<typename _Tp>
inline
gslice_array<_Tp>::gslice_array (_Array<_Tp> __a,
                                 const valarray<size_t>& __i)
        : _M_array (__a), _M_index (__i) {}


template<typename _Tp>
inline
gslice_array<_Tp>::gslice_array (const gslice_array<_Tp>& __a)
        : _M_array (__a._M_array), _M_index (__a._M_index) {}


template<typename _Tp>
inline void
gslice_array<_Tp>::operator= (const _Tp& __t) 
{ 
    __valarray_fill (_M_array, _Array<size_t>(_M_index),
                     _M_index.size(), __t); 
}

template<typename _Tp>
inline void
gslice_array<_Tp>::operator= (const valarray<_Tp>& __v) const
{
    __valarray_copy (_Array<_Tp> (__v), __v.size (),
                     _M_array, _Array<size_t>(_M_index));
}

template<typename _Tp>
template<class E>
inline void
gslice_array<_Tp>::operator= (const _Expr<E, _Tp>& __e) const
{
    __valarray_copy (__e, _M_index.size(), _M_array,
                     _Array<size_t>(_M_index));
}

#undef _DEFINE_VALARRAY_OPERATOR
#define _DEFINE_VALARRAY_OPERATOR(op, name)				\
template<typename _Tp>							\
inline void								\
gslice_array<_Tp>::operator##op##= (const valarray<_Tp>& __v) const	\
{									\
    _Array_augmented_##name (_M_array, _Array<size_t>(_M_index),	\
                              _Array<_Tp> (__v), __v.size ());		\
}									\
									\
template<typename _Tp> template<class E>        			\
inline void 								\
gslice_array<_Tp>::operator##op##= (const _Expr<E, _Tp>& __e) const	\
{									\
    _Array_augmented_##name (_M_array, _Array<size_t>(_M_index), __e,	\
                              _M_index.size());				\
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

    
#endif // __GSLICE_ARRAY__

// Local Variables:
// mode:c++
// End:
