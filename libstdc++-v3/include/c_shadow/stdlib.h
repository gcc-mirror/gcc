// -*- C++ -*- header wrapper.

// Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.
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


#ifndef  _INCLUDED_CPP_STDLIB_H_
# define _INCLUDED_CPP_STDLIB_H_ 1

# include <bits/c++config.h>

# ifdef _IN_C_LEGACY_  /* sub-included by a C header */
      // get out of the "legacy"
    } // close extern "C"
  }   // close namespace _C_legacy::
#  undef _IN_C_LEGACY_
#  define _STDLIB_NEED_C_LEGACY_
# endif

# include <cstdlib>

  // Expose global C names, including non-standard ones, but shadow
  // some names and types with the std:: C++ version.
  using std::div_t;
  using std::ldiv_t;
#ifdef _GLIBCPP_USE_C99
  using std::lldiv_t;
#endif

//  using std::abort;
  using std::abs;
  using std::atexit;
  using std::atof;
  using std::atoi;
  using std::atol;
  using std::bsearch;
  using std::calloc;
  using std::div;
//  using std::exit;
  using std::free;
  using std::getenv;
  using std::labs;
  using std::ldiv;
  using std::malloc;
  using std::mblen;
  using std::mbstowcs;
  using std::mbtowc;
  using std::qsort;
  using std::rand;
  using std::realloc;
  using std::srand;
  using std::strtod;
  using std::strtol;
  using std::strtoul;
  using std::system;
  using std::wcstombs;
  using std::wctomb;

  using std::strtof;

#ifdef _GLIBCPP_USE_LONG_LONG
  using std::strtoll;
  using std::strtoull;
#endif

#ifdef _GLIBCPP_HAVE_STRTOLD
  using std::strtold;
#endif

# ifdef _STDLIB_NEED_C_LEGACY_
  // dive back into the "swamp"
  namespace _C_legacy {
    extern "C" {
#  define _IN_C_LEGACY_
#  undef _STDLIB_NEED_C_LEGACY_
# endif /* _STDLIB_NEED_C__LEGACY_ */
#endif /* _INCLUDED_CPP_STDLIB_H_ */
