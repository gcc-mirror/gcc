// The -*- C++ -*- double_complex class.
// Copyright (C) 1994 Free Software Foundation

// This file is part of the GNU ANSI C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

// As a special exception, if you link this library with files
// compiled with a GNU compiler to produce an executable, this does not cause
// the resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.

// Written by Jason Merrill based upon the specification in the 27 May 1994
// C++ working paper, ANSI document X3J16/94-0098.

#ifndef __DCOMPLEX__
#define __DCOMPLEX__

#ifdef __GNUG__
#pragma interface "dcomplex"
#endif

extern "C++" {
class complex<double>
{
public:
  complex (double r = 0, double i = 0): re (r), im (i) { }
  complex (const complex<float>& r): re (r.real ()), im (r.imag ()) { }
  explicit complex (const complex<long double>& r);

  complex& operator+= (const complex& r) { return __doapl (this, r); }
  complex& operator-= (const complex& r) { return __doami (this, r); }
  complex& operator*= (const complex& r) { return __doaml (this, r); }
  complex& operator/= (const complex& r) { return __doadv (this, r); }

  double real () const { return re; }
  double imag () const { return im; }
private:
  double re, im;

  friend complex& __doapl<> (complex *, const complex&);
  friend complex& __doami<> (complex *, const complex&);
  friend complex& __doaml<> (complex *, const complex&);
  friend complex& __doadv<> (complex *, const complex&);

#ifndef __STRICT_ANSI__
  friend inline complex operator + (const complex& x, double y)
    { return operator+<> (x, y); }
  friend inline complex operator + (double x, const complex& y)
    { return operator+<> (x, y); }
  friend inline complex operator - (const complex& x, double y)
    { return operator-<> (x, y); }
  friend inline complex operator - (double x, const complex& y)
    { return operator-<> (x, y); }
  friend inline complex operator * (const complex& x, double y)
    { return operator*<> (x, y); }
  friend inline complex operator * (double x, const complex& y)
    { return operator*<> (x, y); }
  friend inline complex operator / (const complex& x, double y)
    { return operator/<> (x, y); }
  friend inline complex operator / (double x, const complex& y)
    { return operator/<> (x, y); }
  friend inline bool operator == (const complex& x, double y)
    { return operator==<> (x, y); }
  friend inline bool operator == (double x, const complex& y)
    { return operator==<> (x, y); }
  friend inline bool operator != (const complex& x, double y)
    { return operator!=<> (x, y); }
  friend inline bool operator != (double x, const complex& y)
    { return operator!=<> (x, y); }
#endif /* __STRICT_ANSI__ */
};

inline complex<float>::complex (const complex<double>& r)
: re (r.real ()), im (r.imag ())
{ }
} // extern "C++"

#endif
