// The template and inlines for the -*- C++ -*- complex number classes.

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
// ISO 14882/26.2.1
// Note: this is not a conforming implementation.
// Initially implemented by Ulrich Drepper <drepper@cygnus.com>
// Improved by Gabriel Dos Reis <dosreis@cmla.ens-cachan.fr>
//

#ifndef _CPP_COMPLEX
#define _CPP_COMPLEX	1

#include <bits/c++config.h>
#include <bits/std_iosfwd.h>


namespace std
{
    // Forward declarations
    template<typename _Tp> class complex;
    template<> class complex<float>;
    template<> class complex<double>;
    template<> class complex<long double>;

    template<typename _Tp> _Tp abs(const complex<_Tp>&);
    template<typename _Tp>  _Tp arg(const complex<_Tp>&);

    template<typename _Tp> complex<_Tp> conj(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> polar(const _Tp&, const _Tp&);

    // Transcendentals:
    template<typename _Tp> complex<_Tp> cos(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> cosh(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> exp(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> log(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> log10(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> pow(const complex<_Tp>&, int);
    template<typename _Tp> complex<_Tp> pow(const complex<_Tp>&, const _Tp&);
    template<typename _Tp> complex<_Tp> pow (const complex<_Tp>&,
                                             const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> pow(const _Tp&, const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> sin(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> sinh(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> sqrt(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> tan(const complex<_Tp>&);
    template<typename _Tp> complex<_Tp> tanh(const complex<_Tp>&);
    
    
    //
    // 26.2.2  Primary template class complex
    //
    template <typename _Tp>
    class complex
    {
    public:
        typedef _Tp value_type;

        complex (const _Tp& = _Tp(), const _Tp & = _Tp());

        // Let's the compiler synthetize the copy constructor   
        // complex (const complex<_Tp>&);

        template <typename _Up>
           complex (const complex<_Up>&);
        
        _Tp real () const;
        _Tp imag () const;

        complex<_Tp>& operator= (const _Tp&);
        complex<_Tp>& operator+= (const _Tp&);
        complex<_Tp>& operator-= (const _Tp&);
        complex<_Tp>& operator*= (const _Tp&);
        complex<_Tp>& operator/= (const _Tp&);

        // Let's the compiler synthetize the
        // copy and assignment operator
        // complex<_Tp>& operator= (const complex<_Tp>&);

        template <typename _Up>
           complex<_Tp>& operator= (const complex<_Up>&);
        template <typename _Up>
           complex<_Tp>& operator+= (const complex<_Up>&);
        template <typename _Up>
           complex<_Tp>& operator-= (const complex<_Up>&);
        template <typename _Up>
           complex<_Tp>& operator*= (const complex<_Up>&);
        template <typename _Up>
           complex<_Tp>& operator/= (const complex<_Up>&);

    private:
        _Tp _M_real, _M_imag;
    };

    template<typename _Tp>
    inline _Tp
    complex<_Tp>::real() const { return _M_real; }

    template<typename _Tp>
    inline _Tp
    complex<_Tp>::imag() const { return _M_imag; }

    
    //
    // 26.2.3  complex specializations
    //

    //
    // complex<float> specialization
    //
    template<> class complex<float>
    {
    public:
        typedef float value_type;

        complex(float = 0.0f, float = 0.0f);
#ifdef _GLIBCPP_BUGGY_COMPLEX
	complex(const complex& __z) : _M_value(__z._M_value) {}
#endif
        explicit complex(const complex<double>&);
        explicit complex(const complex<long double>&);

        float real() const;
        float imag() const;

        complex<float>& operator= (float);
        complex<float>& operator+= (float);
        complex<float>& operator-= (float);
        complex<float>& operator*= (float);
        complex<float>& operator/= (float);
        
        // Let's the compiler synthetize the copy and assignment
        // operator.  It always does a pretty good job.
        // complex& operator= (const complex&);

        template <typename _Tp>
           complex<float>&operator= (const complex<_Tp>&);
        template <typename _Tp>
           complex<float>& operator+= (const complex<_Tp>&);
        template <class _Tp>
           complex<float>& operator-= (const complex<_Tp>&);
        template <class _Tp>
           complex<float>& operator*= (const complex<_Tp>&);
        template <class _Tp>
           complex<float>&operator/= (const complex<_Tp>&);

    private:
        typedef __complex__ float _ComplexT;
        _ComplexT _M_value;

        complex(_ComplexT __z) : _M_value(__z) {}
        
        friend class complex<double>;
        friend class complex<long double>;

        friend float abs<>(const complex<float>&);
        friend float arg<>(const complex<float>&);

        friend complex<float> conj<>(const complex<float>&);

        friend complex<float> cos<>(const complex<float>&);
        friend complex<float> cosh<>(const complex<float>&);
        friend complex<float> exp<>(const complex<float>&);
        friend complex<float> log<>(const complex<float>&);
        friend complex<float> log10<>(const complex<float>&);
        friend complex<float> pow<>(const complex<float>&, int);
        friend complex<float> pow<>(const complex<float>&, const float&);
        friend complex<float> pow<>(const complex<float>&,
                                    const complex<float>&);
        friend complex<float> pow<>(const float&, const complex<float>&);
        friend complex<float> sin<>(const complex<float>&);
        friend complex<float> sinh<>(const complex<float>&);
        friend complex<float> sqrt<>(const complex<float>&);
        friend complex<float> tan<>(const complex<float>&);
        friend complex<float> tanh<>(const complex<float>&);
  };

    inline float
    complex<float>::real() const
    { return __real__ _M_value; }

    inline float
    complex<float>::imag() const
    { return __imag__ _M_value; }


    //
    // complex<double> specialization
    //
    template<> class complex<double>
    {
    public:
        typedef double value_type;

        complex(double  =0.0, double =0.0);
#ifdef _GLIBCPP_BUGGY_COMPLEX
	complex(const complex& __z) : _M_value(__z._M_value) {}
#endif
        complex(const complex<float>&);
        explicit complex(const complex<long double>&);
        
        double real () const;
        double imag () const;
        
        complex<double>& operator= (double);
        complex<double>& operator+= (double);
        complex<double>& operator-= (double);
        complex<double>& operator*= (double);
        complex<double>& operator/= (double);

        // The compiler will synthetize this, efficiently.
        // complex& operator= (const complex&);

        template <typename _Tp>
           complex<double>& operator= (const complex<_Tp>&);
        template <typename _Tp>
           complex<double>& operator+= (const complex<_Tp>&);
        template <typename _Tp>
           complex<double>& operator-= (const complex<_Tp>&);
        template <typename _Tp>
           complex<double>& operator*= (const complex<_Tp>&);
        template <typename _Tp>
        complex<double>& operator/= (const complex<_Tp>&);

    private:
        typedef __complex__ double _ComplexT;
        _ComplexT _M_value;

        complex(_ComplexT __z) : _M_value(__z) {}
        
        friend class complex<float>;
        friend class complex<long double>;

        friend double abs<>(const complex<double>&);
        friend double arg<>(const complex<double>&);

        friend complex<double> conj<>(const complex<double>&);

        friend complex<double> cos<>(const complex<double>&);
        friend complex<double> cosh<>(const complex<double>&);
        friend complex<double> exp<>(const complex<double>&);
        friend complex<double> log<>(const complex<double>&);
        friend complex<double> log10<>(const complex<double>&);
        friend complex<double> pow<>(const complex<double>&, int);
        friend complex<double> pow<>(const complex<double>&, const double&);
        friend complex<double> pow<>(const complex<double>&,
                                    const complex<double>&);
        friend complex<double> pow<>(const double&, const complex<double>&);
        friend complex<double> sin<>(const complex<double>&);
        friend complex<double> sinh<>(const complex<double>&);
        friend complex<double> sqrt<>(const complex<double>&);
        friend complex<double> tan<>(const complex<double>&);
        friend complex<double> tanh<>(const complex<double>&);
    };

    inline double
    complex<double>::real() const
    { return __real__ _M_value; }

    inline double
    complex<double>::imag() const
    { return __imag__ _M_value; }


    //
    // complex<long double> specialization
    //
    template<> class complex<long double>
    {
    public:
        typedef long double value_type;

        complex(long double = 0.0L, long double = 0.0L);
#ifdef _GLIBCPP_BUGGY_COMPLEX
	complex(const complex& __z) : _M_value(__z._M_value) {}
#endif
        complex(const complex<float>&);
        complex(const complex<double>&);

        long double real() const;
        long double imag() const;

        complex<long double>& operator= (long double);
        complex<long double>& operator+= (long double);
        complex<long double>& operator-= (long double);
        complex<long double>& operator*= (long double);
        complex<long double>& operator/= (long double);

        // The compiler knows how to do this efficiently
        // complex& operator= (const complex&);

        template<typename _Tp>
           complex<long double>& operator= (const complex<_Tp>&);
        template<typename _Tp>
           complex<long double>& operator+= (const complex<_Tp>&);
        template<typename _Tp>
           complex<long double>& operator-= (const complex<_Tp>&);
        template<typename _Tp>
           complex<long double>& operator*= (const complex<_Tp>&);
        template<typename _Tp>
        complex<long double>& operator/= (const complex<_Tp>&);

    private:
        typedef __complex__ long double _ComplexT;
        _ComplexT _M_value;

        complex(_ComplexT __z) : _M_value(__z) {}

        friend class complex<float>;
        friend class complex<double>;

        friend long double abs<>(const complex<long double>&);
        friend long double arg<>(const complex<long double>&);

        friend complex<long double> conj<>(const complex<long double>&);

        friend complex<long double> cos<>(const complex<long double>&);
        friend complex<long double> cosh<>(const complex<long double>&);
        friend complex<long double> exp<>(const complex<long double>&);
        friend complex<long double> log<>(const complex<long double>&);
        friend complex<long double> log10<>(const complex<long double>&);
        friend complex<long double> pow<>(const complex<long double>&, int);
        friend complex<long double> pow<>(const complex<long double>&,
                                          const long double&);
        friend complex<long double> pow<>(const complex<long double>&,
                                    const complex<long double>&);
        friend complex<long double> pow<>(const long double&,
                                          const complex<long double>&);
        friend complex<long double> sin<>(const complex<long double>&);
        friend complex<long double> sinh<>(const complex<long double>&);
        friend complex<long double> sqrt<>(const complex<long double>&);
        friend complex<long double> tan<>(const complex<long double>&);
        friend complex<long double> tanh<>(const complex<long double>&);
    };

    inline
    complex<long double>::complex(long double __r, long double __i)
    {
        __real__ _M_value = __r;
        __imag__ _M_value = __i;
    }

    inline
    complex<long double>::complex(const complex<float>& __z)
            : _M_value(_ComplexT(__z._M_value)) {}

    inline
    complex<long double>::complex(const complex<double>& __z)
            : _M_value(_ComplexT(__z._M_value)) {}

    inline long double
    complex<long double>::real() const
    { return __real__ _M_value; }

    inline long double
    complex<long double>::imag() const
    { return __imag__ _M_value; }

    inline complex<long double>&
    complex<long double>::operator= (long double __r)
    {
        __real__ _M_value = __r;
        __imag__ _M_value = 0.0L;
        return *this;
    }

    inline complex<long double>&
    complex<long double>::operator+= (long double __r)
    {
        __real__ _M_value += __r;
        return *this;
    }

    inline complex<long double>&
    complex<long double>::operator-= (long double __r)
    {
        __real__ _M_value -= __r;
        return *this;
    }

    inline complex<long double>&
    complex<long double>::operator*= (long double __r)
    {
        __real__ _M_value *= __r;
        return *this;
    }

    inline complex<long double>&
    complex<long double>::operator/= (long double __r)
    {
        __real__ _M_value /= __r;
        return *this;
    }

    template<typename _Tp>
    inline complex<long double>&
    complex<long double>::operator= (const complex<_Tp>& __z)
    {
        __real__ _M_value = __z.real();
        __imag__ _M_value = __z.imag();
        return *this;
    }

    template<typename _Tp>
    inline complex<long double>&
    complex<long double>::operator+= (const complex<_Tp>& __z)
    {
        __real__ _M_value += __z.real();
        __imag__ _M_value += __z.imag();
        return *this;
    }

    template<typename _Tp>
    inline complex<long double>&
    complex<long double>::operator-= (const complex<_Tp>& __z)
    {
        __real__ _M_value -= __z.real();
        __imag__ _M_value -= __z.imag();
        return *this;
    }
    
    template<typename _Tp>
    inline complex<long double>&
    complex<long double>::operator*= (const complex<_Tp>& __z)
    {
        _ComplexT __t;
        __real__ __t = __z.real();
        __imag__ __t = __z.imag();
        _M_value *= __t;
        return *this;
    }

    template<typename _Tp>
    inline complex<long double>&
    complex<long double>::operator/= (const complex<_Tp>& __z)
    {
        _ComplexT __t;
        __real__ __t = __z.real();
        __imag__ __t = __z.imag();
        _M_value /= __t;
        return *this;
    }

    //
    // complex<float> continued.
    //
    inline
    complex<float>::complex(float r, float i)
    {
        __real__ _M_value = r;
        __imag__ _M_value = i;
    }

    inline
    complex<float>::complex(const complex<double>& __z)
            : _M_value(_ComplexT(__z._M_value)) {}

    inline
    complex<float>::complex(const complex<long double>& __z)
            : _M_value(_ComplexT(__z._M_value)) {}

    inline complex<float>&
    complex<float>::operator= (float __f)
    {
        __real__ _M_value = __f;
        __imag__ _M_value = 0.0f;
        return *this;
    }

    inline complex<float>&
    complex<float>::operator+= (float __f)
    {
        __real__ _M_value += __f;
        return *this;
    }

    inline complex<float>&
    complex<float>::operator-= (float __f)
    {
        __real__ _M_value -= __f;
        return *this;
    }

    inline complex<float>&
    complex<float>::operator*= (float __f)
    {
        _M_value *= __f;
        return *this;
    }

    inline complex<float>&
    complex<float>::operator/= (float __f)
    {
        _M_value /= __f;
        return *this;
    }

    template<typename _Tp>
    inline complex<float>&
    complex<float>::operator= (const complex<_Tp>& __z)
    {
        __real__ _M_value = __z.real();
        __imag__ _M_value = __z.imag();
        return *this;
    }

    template<typename _Tp>
    inline complex<float>&
    complex<float>::operator+= (const complex<_Tp>& __z)
    {
        __real__ _M_value += __z.real();
        __imag__ _M_value += __z.imag();
        return *this;
    }
    
    template<typename _Tp>
    inline complex<float>&
    complex<float>::operator-= (const complex<_Tp>& __z)
    {
        __real__ _M_value -= __z.real();
        __imag__ _M_value -= __z.real();
        return *this;
    }

    template<typename _Tp>
    inline complex<float>&
    complex<float>::operator*= (const complex<_Tp>& __z)
    {
        _ComplexT __t;
        __real__ __t = __z.real();
        __imag__ __t = __z.imag();
        _M_value *= __t;
        return *this;
    }

    template<typename _Tp>
    inline complex<float>&
    complex<float>::operator/= (const complex<_Tp>& __z)
    {
        _ComplexT __t;
        __real__ __t = __z.real();
        __imag__ __t = __z.imag();
        _M_value /= __t;
        return *this;
    }


    //
    // complex<double> continued.
    //
    inline
    complex<double>::complex(double __r, double __i)
    {
        __real__ _M_value = __r;
        __imag__ _M_value = __i;
    }

    inline
    complex<double>::complex(const complex<float>& __z)
            : _M_value(_ComplexT(__z._M_value)) {}

    inline
    complex<double>::complex(const complex<long double>& __z)
    {
        __real__ _M_value = __z.real();
        __imag__ _M_value = __z.imag();
    }

    inline complex<double>&
    complex<double>::operator= (double __d)
    {
        __real__ _M_value = __d;
        __imag__ _M_value = 0.0;
        return *this;
    }

    inline complex<double>&
    complex<double>::operator+= (double __d)
    {
        __real__ _M_value += __d;
        return *this;
    }

    inline complex<double>&
    complex<double>::operator-= (double __d)
    {
        __real__ _M_value -= __d;
        return *this;
    }

    inline complex<double>&
    complex<double>::operator*= (double __d)
    {
        _M_value *= __d;
        return *this;
    }

    inline complex<double>&
    complex<double>::operator/= (double __d)
    {
        _M_value /= __d;
        return *this;
    }

    template<typename _Tp>
    inline complex<double>&
    complex<double>::operator= (const complex<_Tp>& __z)
    {
        __real__ _M_value = __z.real();
        __imag__ _M_value = __z.imag();
        return *this;
    }
    
    template<typename _Tp>
    inline complex<double>&
    complex<double>::operator+= (const complex<_Tp>& __z)
    {
        __real__ _M_value += __z.real();
        __imag__ _M_value += __z.imag();
        return *this;
    }

    template<typename _Tp>
    inline complex<double>&
    complex<double>::operator-= (const complex<_Tp>& __z)
    {
        __real__ _M_value -= __z.real();
        __imag__ _M_value -= __z.imag();
        return *this;
    }

    template<typename _Tp>
    inline complex<double>&
    complex<double>::operator*= (const complex<_Tp>& __z)
    {
        _ComplexT __t;
        __real__ __t = __z.real();
        __imag__ __t = __z.imag();
        _M_value *= __t;
        return *this;
    }

    template<typename _Tp>
    inline complex<double>&
    complex<double>::operator/= (const complex<_Tp>& __z)
    {
        _ComplexT __t;
        __real__ __t = __z.real();
        __imag__ __t = __z.imag();
        _M_value /= __t;
        return *this;
    }

    //
    // Primary template class complex continued.
    //
    // 26.2.4
    template<typename _Tp>
    inline 
    complex<_Tp>::complex(const _Tp& __r, const _Tp& __i)
            : _M_real(__r), _M_imag(__i) {}

    template<typename _Tp>
       template<typename _Up>
    inline 
    complex<_Tp>::complex(const complex<_Up>& __z)
            : _M_real(__z.real()), _M_imag(__z.imag()) {}

    // 26.2.7/6
    template<typename _Tp>
    inline complex<_Tp>
    conj(const complex<_Tp>& __z)
    { return complex<_Tp>(__z.real(), -__z.imag()); }

    // 26.2.7/4
    template<typename _Tp>
    inline _Tp
    norm(const complex<_Tp>& __z)
    {
        // XXX: Grammar school computation
        return __z.real() * __z.real() + __z.imag() * __z.imag();
    }
        
    template<typename _Tp>
    complex<_Tp>&
    complex<_Tp>::operator= (const _Tp& __t)
    {
        _M_real = __t;
        _M_imag = _Tp();
        return *this;
    }

    // 26.2.5/1
    template<typename _Tp>
    inline complex<_Tp>&
    complex<_Tp>::operator+= (const _Tp& __t)
    {
        _M_real += __t;
        return *this;
    }

    // 26.2.5/3
    template<typename _Tp>
    inline complex<_Tp>&
    complex<_Tp>::operator-= (const _Tp& __t)
    {
        _M_real -= __t;
        return *this;
    }

    // 26.2.5/5
    template<typename _Tp>
    complex<_Tp>&
    complex<_Tp>::operator*= (const _Tp& __t)
    {
        _M_real *= __t;
        _M_imag *= __t;
        return *this;
    }

    // 26.2.5/7
    template<typename _Tp>
    complex<_Tp>&
    complex<_Tp>::operator/= (const _Tp& __t)
    {
        _M_real /= __t;
        _M_imag /= __t;
        return *this;
    }

    template<typename _Tp>
       template<typename _Up>
    complex<_Tp>&
    complex<_Tp>::operator= (const complex<_Up>& __z)
    {
        _M_real = __z.real();
        _M_imag = __z.imag();
        return *this;
    }

    // 26.2.5/9
    template<typename _Tp>
       template<typename _Up>
    complex<_Tp>&
    complex<_Tp>::operator+= (const complex<_Up>& __z)
    {
        _M_real += __z.real();
        _M_imag += __z.imag();
        return *this;
    }

    // 26.2.5/11
    template<typename _Tp>
       template<typename _Up>
    complex<_Tp>&
    complex<_Tp>::operator-= (const complex<_Up>& __z)
    {
        _M_real -= __z.real();
        _M_imag -= __z.imag();
        return *this;
    }

    // 26.2.5/13
    // XXX: this is a grammar school implementation.
    template<typename _Tp>
       template<typename _Up>
    complex<_Tp>&
    complex<_Tp>::operator*= (const complex<_Up>& __z)
    {
        _Tp __r = _M_real * __z.real() - _M_imag * __z.imag();
        _M_imag = _M_real * __z.imag() + _M_imag * __z.real();
        _M_real = __r;
        return *this;
    }

    // 26.2.5/15
    // XXX: this is a grammar school implementation.
    template<typename _Tp>
       template<typename _Up>
    complex<_Tp>&
    complex<_Tp>::operator/= (const complex<_Up>& __z)
    {
        _Tp __r =  _M_real * __z.real() + _M_imag * __z.imag();
        _Tp __n = norm(__z);
        _M_imag = (_M_real * __z.imag() - _M_imag * __z.real()) / __n;
        _M_real = __r / __n;
        return *this;
    }
    

    // Operators:
    template<typename _Tp>
    inline complex<_Tp>
    operator+(const complex<_Tp>& __x, const complex<_Tp>& __y)
    { return complex<_Tp> (__x) += __y; }

    template<typename _Tp>
    inline complex<_Tp>
    operator+(const complex<_Tp>& __x, const _Tp& __y)
    { return complex<_Tp> (__x) += __y; }

    template<typename _Tp>
    inline complex<_Tp>
    operator+(const _Tp& __x, const complex<_Tp>& __y)
    { return complex<_Tp> (__y) += __x; }

    template<typename _Tp>
    inline complex<_Tp>
    operator-(const complex<_Tp>& __x, const complex<_Tp>& __y)
    { return complex<_Tp> (__x) -= __y; }
    
    template<typename _Tp>
    inline complex<_Tp>
    operator-(const complex<_Tp>& __x, const _Tp& __y)
    { return complex<_Tp> (__x) -= __y; }

    template<typename _Tp>
    inline complex<_Tp>
    operator-(const _Tp& __x, const complex<_Tp>& __y)
    { return complex<_Tp> (__x) -= __y; }

    template<typename _Tp>
    inline complex<_Tp>
    operator*(const complex<_Tp>& __x, const complex<_Tp>& __y)
    { return complex<_Tp> (__x) *= __y; }

    template<typename _Tp>
    inline complex<_Tp>
    operator*(const complex<_Tp>& __x, const _Tp& __y)
    { return complex<_Tp> (__x) *= __y; }

    template<typename _Tp>
    inline complex<_Tp>
    operator*(const _Tp& __x, const complex<_Tp>& __y)
    { return complex<_Tp> (__y) *= __x; }

    template<typename _Tp>
    inline complex<_Tp>
    operator/(const complex<_Tp>& __x, const complex<_Tp>& __y)
    { return complex<_Tp> (__x) /= __y; }
    
    template<typename _Tp>
    inline complex<_Tp>
    operator/(const complex<_Tp>& __x, const _Tp& __y)
    { return complex<_Tp> (__x) /= __y; }

    template<typename _Tp>
    inline complex<_Tp>
    operator/(const _Tp& __x, const complex<_Tp>& __y)
    { return complex<_Tp> (__x) /= __y; }

    template<typename _Tp>
    inline complex<_Tp>
    operator+(const complex<_Tp>& __x)
    { return __x; }

    template<typename _Tp>
    inline complex<_Tp>
    operator-(const complex<_Tp>& __x)
    {  return complex<_Tp>(-__x.real(), -__x.imag()); }

    template<typename _Tp>
    inline bool
    operator==(const complex<_Tp>& __x, const complex<_Tp>& __y)
    { return __x.real() == __y.real() && __x.imag == __y.imag(); }

    template<typename _Tp>
    inline bool
    operator==(const complex<_Tp>& __x, const _Tp& __y)
    { return __x.real() == __y && __x.imag() == 0; }

    template<typename _Tp>
    inline bool
    operator==(const _Tp& __x, const complex<_Tp>& __y)
    { return __x == __y.real() && 0 == __y.imag(); }

    template<typename _Tp>
    inline bool
    operator!=(const complex<_Tp>& __x, const complex<_Tp>& __y)
    { return __x.real() != __y.real() || __x.imag() != __y.imag(); }

    template<typename _Tp>
    inline bool
    operator!=(const complex<_Tp>& __x, const _Tp& __y)
    {  return __x.real() != __y || __x.imag() != 0; }

    template<typename _Tp>
    inline bool
    operator!=(const _Tp& __x, const complex<_Tp>& __y)
    { return __x != __y.real() || 0 != __y.imag(); }

    template<typename _Tp, typename _CharT, class _Traits>
    basic_istream<_CharT, _Traits>&
    operator>>(basic_istream<_CharT, _Traits>&, complex<_Tp>&);

    template<typename _Tp, typename _CharT, class _Traits>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>&, const complex<_Tp>&);


    // Values:
    template <typename _Tp>
    inline _Tp
    real (const complex<_Tp>& __z)
    { return __z.real(); }
    
    template <typename _Tp>
    inline _Tp
    imag (const complex<_Tp>& __z)
    { return __z.imag(); }
    

    // We use here a few more specializations.
    template<>
    inline complex<float>
    conj(const complex<float> &__x)
#ifdef _GLIBCPP_BUGGY_FLOAT_COMPLEX
    {
      complex<float> __tmpf(~__x._M_value);
      return __tmpf;
    }
#else
    { return complex<float>(~__x._M_value); }
#endif

    template<>
    inline complex<double>
    conj(const complex<double> &__x)
    {  return complex<double> (~__x._M_value); }

    template<>
    inline complex<long double>
    conj(const complex<long double> &__x)
    {
        return complex<long double> (~__x._M_value);
    }

} // namespace std

#endif	/* _CPP_COMPLEX */
