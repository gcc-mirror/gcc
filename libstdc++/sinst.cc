// Instantiation file for the -*- C++ -*- string classes.
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

// Written by Jason Merrill based upon the specification by Takanori Adachi
// in ANSI X3J16/94-0013R2.

#ifdef __GNUG__
#ifdef TRAITS
#ifdef C
#pragma implementation "std/straits.h"
#endif
#endif
#endif

#include <string>

#ifdef C
typedef char c;
#endif
#ifdef W
typedef wchar_t c;
#endif

#if defined(TRAITS) && !defined(C)
template class string_char_traits <c>;
#else
/* string_char_traits<char> is already explicitly specialized in
   std/straits.h.  */
#endif

typedef basic_string <c> s;

#ifdef MAIN
template class basic_string <c>;
#endif

#ifdef ADDSS
template s operator+ (const s&, const s&);
#endif
#ifdef ADDPS
template s operator+ (const c*, const s&);
#endif
#ifdef ADDCS
template s operator+ (c, const s&);
#endif
#ifdef ADDSP
template s operator+ (const s&, const c*);
#endif
#ifdef ADDSC
template s operator+ (const s&, c);
#endif
#ifdef EQSS
template bool operator== (const s&, const s&);
#endif
#ifdef EQPS
template bool operator== (const c*, const s&);
#endif
#ifdef EQSP
template bool operator== (const s&, const c*);
#endif
#ifdef NESS
template bool operator!= (const s&, const s&);
#endif
#ifdef NEPS
template bool operator!= (const c*, const s&);
#endif
#ifdef NESP
template bool operator!= (const s&, const c*);
#endif
#ifdef LTSS
template bool operator< (const s&, const s&);
#endif
#ifdef LTPS
template bool operator< (const c*, const s&);
#endif
#ifdef LTSP
template bool operator< (const s&, const c*);
#endif
#ifdef GTSS
template bool operator> (const s&, const s&);
#endif
#ifdef GTPS
template bool operator> (const c*, const s&);
#endif
#ifdef GTSP
template bool operator> (const s&, const c*);
#endif
#ifdef LESS
template bool operator<= (const s&, const s&);
#endif
#ifdef LEPS
template bool operator<= (const c*, const s&);
#endif
#ifdef LESP
template bool operator<= (const s&, const c*);
#endif
#ifdef GESS
template bool operator>= (const s&, const s&);
#endif
#ifdef GEPS
template bool operator>= (const c*, const s&);
#endif
#ifdef GESP
template bool operator>= (const s&, const c*);
#endif
#ifdef EXTRACT
template istream& operator>> (istream&, s&);
#endif // EXTRACT
#ifdef INSERT
template ostream& operator<< (ostream&, const s&);
#endif // INSERT
#ifdef GETLINE
template istream& getline (istream&, s&, c);
#endif
