// The template and inlines for the -*- C++ -*- complex number classes.
// Copyright (C) 1994 Free Software Foundation

// This file is part of the GNU ANSI C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms of
// the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

// As a special exception, if you link this library with files compiled
// with a GNU compiler to produce an executable, this does not cause the
// resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why the
// executable file might be covered by the GNU General Public License.

// Written by Jason Merrill based upon the specification in the 27 May 1994
// C++ working paper, ANSI document X3J16/94-0098.

#ifndef __COMPLEXT__
#define __COMPLEXT__

#ifdef __GNUG__
#pragma interface
#endif

#include <cmath>

#if ! defined (__GNUG__) && ! defined (__attribute__)
#define __attribute__(foo) /* Ignore.  */
#endif

class istream;
class ostream;

extern "C++" {
template <class _FLT> class complex;
template <class _FLT> complex<_FLT>&
  __doapl (complex<_FLT>* ths, const complex<_FLT>& r);
template <class _FLT> complex<_FLT>&
  __doami (complex<_FLT>* ths, const complex<_FLT>& r);
template <class _FLT> complex<_FLT>&
  __doaml (complex<_FLT>* ths, const complex<_FLT>& r);
template <class _FLT> complex<_FLT>&
  __doadv (complex<_FLT>* ths, const complex<_FLT>& r);

template <class _FLT>
class complex
{
public:
  complex (_FLT r = 0, _FLT i = 0): re (r), im (i) { }
  complex& operator += (const complex&);
  complex& operator -= (const complex&);
  complex& operator *= (const complex&);
  complex& operator /= (const complex&);
  _FLT real () const { return re; }
  _FLT imag () const { return im; }
private:
  _FLT re, im;

  friend complex& __doapl<> (complex *, const complex&);
  friend complex& __doami<> (complex *, const complex&);
  friend complex& __doaml<> (complex *, const complex&);
  friend complex& __doadv<> (complex *, const complex&);
};

// Declare specializations.
class complex<float>;
class complex<double>;
class complex<long double>;

template <class _FLT>
inline complex<_FLT>&
__doapl (complex<_FLT>* ths, const complex<_FLT>& r)
{
  ths->re += r.re;
  ths->im += r.im;
  return *ths;
}
template <class _FLT>
inline complex<_FLT>&
complex<_FLT>::operator += (const complex<_FLT>& r)
{
  return __doapl (this, r);
}

template <class _FLT>
inline complex<_FLT>&
__doami (complex<_FLT>* ths, const complex<_FLT>& r)
{
  ths->re -= r.re;
  ths->im -= r.im;
  return *ths;
}
template <class _FLT>
inline complex<_FLT>&
complex<_FLT>::operator -= (const complex<_FLT>& r)
{
  return __doami (this, r);
}

template <class _FLT>
inline complex<_FLT>&
__doaml (complex<_FLT>* ths, const complex<_FLT>& r)
{
  _FLT f = ths->re * r.re - ths->im * r.im;
  ths->im = ths->re * r.im + ths->im * r.re;
  ths->re = f;
  return *ths;
}
template <class _FLT>
inline complex<_FLT>&
complex<_FLT>::operator *= (const complex<_FLT>& r)
{
  return __doaml (this, r);
}

template <class _FLT>
inline complex<_FLT>&
complex<_FLT>::operator /= (const complex<_FLT>& r)
{
  return __doadv (this, r);
}

template <class _FLT> inline _FLT
imag (const complex<_FLT>& x) __attribute__ ((const));

template <class _FLT> inline _FLT
imag (const complex<_FLT>& x)
{
  return x.imag ();
}

template <class _FLT> inline _FLT
real (const complex<_FLT>& x) __attribute__ ((const));

template <class _FLT> inline _FLT
real (const complex<_FLT>& x)
{
  return x.real ();
}

template <class _FLT> inline complex<_FLT>
operator + (const complex<_FLT>& x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator + (const complex<_FLT>& x, const complex<_FLT>& y)
{
  return complex<_FLT> (real (x) + real (y), imag (x) + imag (y));
}

template <class _FLT> inline complex<_FLT>
operator + (const complex<_FLT>& x, _FLT y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator + (const complex<_FLT>& x, _FLT y)
{
  return complex<_FLT> (real (x) + y, imag (x));
}

template <class _FLT> inline complex<_FLT>
operator + (_FLT x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator + (_FLT x, const complex<_FLT>& y)
{
  return complex<_FLT> (x + real (y), imag (y));
}

template <class _FLT> inline complex<_FLT>
operator - (const complex<_FLT>& x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator - (const complex<_FLT>& x, const complex<_FLT>& y)
{
  return complex<_FLT> (real (x) - real (y), imag (x) - imag (y));
}

template <class _FLT> inline complex<_FLT>
operator - (const complex<_FLT>& x, _FLT y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator - (const complex<_FLT>& x, _FLT y)
{
  return complex<_FLT> (real (x) - y, imag (x));
}

template <class _FLT> inline complex<_FLT>
operator - (_FLT x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator - (_FLT x, const complex<_FLT>& y)
{
  return complex<_FLT> (x - real (y), - imag (y));
}

template <class _FLT> inline complex<_FLT>
operator * (const complex<_FLT>& x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator * (const complex<_FLT>& x, const complex<_FLT>& y)
{
  return complex<_FLT> (real (x) * real (y) - imag (x) * imag (y),
			   real (x) * imag (y) + imag (x) * real (y));
}

template <class _FLT> inline complex<_FLT>
operator * (const complex<_FLT>& x, _FLT y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator * (const complex<_FLT>& x, _FLT y)
{
  return complex<_FLT> (real (x) * y, imag (x) * y);
}

template <class _FLT> inline complex<_FLT>
operator * (_FLT x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator * (_FLT x, const complex<_FLT>& y)
{
  return complex<_FLT> (x * real (y), x * imag (y));
}

template <class _FLT> complex<_FLT>
operator / (const complex<_FLT>& x, _FLT y) __attribute__ ((const));

template <class _FLT> complex<_FLT>
operator / (const complex<_FLT>& x, _FLT y)
{
  return complex<_FLT> (real (x) / y, imag (x) / y);
}

template <class _FLT> inline complex<_FLT>
operator + (const complex<_FLT>& x) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator + (const complex<_FLT>& x)
{
  return x;
}

template <class _FLT> inline complex<_FLT>
operator - (const complex<_FLT>& x) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
operator - (const complex<_FLT>& x)
{
  return complex<_FLT> (-real (x), -imag (x));
}

template <class _FLT> inline bool
operator == (const complex<_FLT>& x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline bool
operator == (const complex<_FLT>& x, const complex<_FLT>& y)
{
  return real (x) == real (y) && imag (x) == imag (y);
}

template <class _FLT> inline bool
operator == (const complex<_FLT>& x, _FLT y) __attribute__ ((const));

template <class _FLT> inline bool
operator == (const complex<_FLT>& x, _FLT y)
{
  return real (x) == y && imag (x) == 0;
}

template <class _FLT> inline bool
operator == (_FLT x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline bool
operator == (_FLT x, const complex<_FLT>& y)
{
  return x == real (y) && imag (y) == 0;
}

template <class _FLT> inline bool
operator != (const complex<_FLT>& x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline bool
operator != (const complex<_FLT>& x, const complex<_FLT>& y)
{
  return real (x) != real (y) || imag (x) != imag (y);
}

template <class _FLT> inline bool
operator != (const complex<_FLT>& x, _FLT y) __attribute__ ((const));

template <class _FLT> inline bool
operator != (const complex<_FLT>& x, _FLT y)
{
  return real (x) != y || imag (x) != 0;
}

template <class _FLT> inline bool
operator != (_FLT x, const complex<_FLT>& y) __attribute__ ((const));

template <class _FLT> inline bool
operator != (_FLT x, const complex<_FLT>& y)
{
  return x != real (y) || imag (y) != 0;
}

// Some targets don't provide a prototype for hypot when -ansi.
extern "C" double hypot (double, double) __attribute__ ((const));

template <class _FLT> inline _FLT
abs (const complex<_FLT>& x) __attribute__ ((const));

template <class _FLT> inline _FLT
abs (const complex<_FLT>& x)
{
  return hypot (real (x), imag (x));
}

template <class _FLT> inline _FLT
arg (const complex<_FLT>& x) __attribute__ ((const));

template <class _FLT> inline _FLT
arg (const complex<_FLT>& x)
{
  return atan2 (imag (x), real (x));
}

template <class _FLT> inline complex<_FLT>
polar (_FLT r, _FLT t) __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
polar (_FLT r, _FLT t)
{
  return complex<_FLT> (r * cos (t), r * sin (t));
}

template <class _FLT> inline complex<_FLT>
conj (const complex<_FLT>& x)  __attribute__ ((const));

template <class _FLT> inline complex<_FLT>
conj (const complex<_FLT>& x) 
{
  return complex<_FLT> (real (x), -imag (x));
}

template <class _FLT> inline _FLT
norm (const complex<_FLT>& x) __attribute__ ((const));

template <class _FLT> inline _FLT
norm (const complex<_FLT>& x)
{
  return real (x) * real (x) + imag (x) * imag (x);
}

// Declarations of templates in complext.ccI

template <class _FLT> complex<_FLT>
  operator / (const complex<_FLT>&, const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  operator / (_FLT, const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  cos (const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  cosh (const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  exp (const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  log (const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  pow (const complex<_FLT>&, const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  pow (const complex<_FLT>&, _FLT) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  pow (const complex<_FLT>&, int) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  pow (_FLT, const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  sin (const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  sinh (const complex<_FLT>&) __attribute__ ((const));
template <class _FLT> complex<_FLT>
  sqrt (const complex<_FLT>&) __attribute__ ((const));

template <class _FLT> istream& operator >> (istream&, complex<_FLT>&);
template <class _FLT> ostream& operator << (ostream&, const complex<_FLT>&);
} // extern "C++"

// Specializations and such

#include <std/fcomplex.h>
#include <std/dcomplex.h>
#include <std/ldcomplex.h>

#endif
