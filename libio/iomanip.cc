/* This is part of libio/iostream, providing -*- C++ -*- input/output.
Copyright (C) 1993 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

#ifdef __GNUG__
#pragma implementation
#endif

#include "iomanip.h"


// Those functions are called through a pointer, 
// thus it does not make sense, to inline them.

ios & __iomanip_setbase (ios& i, int n)
{
    ios::fmtflags b;
    switch (n)
      {
	case  8: 
	  b = ios::oct; break;
	case 10: 
	  b = ios::dec; break;
	case 16: 
	  b = ios::hex; break;
	default:
	  b = 0;
      }
    i.setf(b, ios::basefield);
    return i;
}

ios & __iomanip_setfill (ios& i, int n)
{
    //FIXME if ( i.flags() & ios::widechar )
      i.fill( (char) n);
    //FIXME else
    //FIXME   i.fill( (wchar) n);
    return i;
}   

ios &  __iomanip_setprecision (ios& i, int n)
{
    i.precision(n);
    return i;
}
ios &  __iomanip_setw (ios& i, int n)
{
    i.width(n);
    return i;
}

ios & __iomanip_setiosflags (ios& i, ios::fmtflags n)
{
    i.setf(n,n);
    return i;
}

ios & __iomanip_resetiosflags (ios& i, ios::fmtflags n)
{
    i.setf(0,n);
    return i;
}

template class smanip<int>;
template class smanip<ios::fmtflags>;
template istream& operator>>(istream&, const smanip<int>&);
template istream& operator>>(istream&, const smanip<ios::fmtflags>&);
template ostream& operator<<(ostream&, const smanip<int>&);
template ostream& operator<<(ostream&, const smanip<ios::fmtflags>&);
