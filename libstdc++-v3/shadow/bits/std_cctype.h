// -*- C++ -*- header wrapper.

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

// ISO C++ 14882: 22
//

#ifndef _CPP_CCTYPE
#define _CPP_CCTYPE 1

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <ctype.h>
    }

    inline int _CPP_isalnum_capture(int c) { return isalnum (c); }
    inline int _CPP_isalpha_capture(int c) { return isalpha (c); }
    inline int _CPP_iscntrl_capture(int c) { return iscntrl (c); }
    inline int _CPP_isdigit_capture(int c) { return isdigit (c); }
    inline int _CPP_isgraph_capture(int c) { return isgraph (c); }
    inline int _CPP_islower_capture(int c) { return islower (c); }
    inline int _CPP_isprint_capture(int c) { return isprint (c); }
    inline int _CPP_ispunct_capture(int c) { return ispunct (c); }
    inline int _CPP_isspace_capture(int c) { return isspace (c); }
    inline int _CPP_isupper_capture(int c) { return isupper (c); }
    inline int _CPP_isxdigit_capture(int c) { return isxdigit (c); }

    inline int _CPP_tolower_capture(int c) { return tolower (c); }
    inline int _CPP_toupper_capture(int c) { return toupper (c); }

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

# undef isalnum
# undef isalpha
# undef iscntrl
# undef isdigit
# undef isgraph
# undef islower
# undef isprint
# undef ispunct
# undef isspace
# undef isupper
# undef isxdigit

# undef tolower
# undef toupper

#if 0  /* these might not be necessary any more. 98-09-29 */
# undef _U
# undef _L
# undef _N
# undef _S
# undef _P
# undef _C
# undef _B
# undef _X
#endif

  namespace _C_Swamp {
    namespace _C_Shadow {
    }
  }
  namespace std {

    inline int isalnum(int __c)
      { return ::_C_Swamp::_CPP_isalnum_capture (__c); }
    inline int isalpha (int __c)
      { return ::_C_Swamp::_CPP_isalpha_capture (__c); }
    inline int iscntrl (int __c)
      { return ::_C_Swamp::_CPP_iscntrl_capture (__c); }
    inline int isdigit (int __c)
      { return ::_C_Swamp::_CPP_isdigit_capture (__c); }
    inline int isgraph (int __c)
      { return ::_C_Swamp::_CPP_isgraph_capture (__c); }
    inline int islower (int __c)
      { return ::_C_Swamp::_CPP_islower_capture (__c); }
    inline int isprint (int __c)
      { return ::_C_Swamp::_CPP_isprint_capture (__c); }
    inline int ispunct (int __c)
      { return ::_C_Swamp::_CPP_ispunct_capture (__c); }
    inline int isspace (int __c)
      { return ::_C_Swamp::_CPP_isspace_capture (__c); }
    inline int isupper (int __c)
      { return ::_C_Swamp::_CPP_isupper_capture (__c); }
    inline int isxdigit (int __c)
      { return ::_C_Swamp::_CPP_isxdigit_capture (__c); }

    inline int tolower (int __c)
      { return ::_C_Swamp::_CPP_tolower_capture (__c); }
    inline int toupper (int __c)
      { return ::_C_Swamp::_CPP_toupper_capture (__c); }

  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
      using ::std::isalnum;
      using ::std::isalpha;
      using ::std::iscntrl;
      using ::std::isdigit;
      using ::std::isgraph;
      using ::std::islower;
      using ::std::isprint;
      using ::std::ispunct;
      using ::std::isspace;
      using ::std::isupper;
      using ::std::isxdigit;
      using ::std::tolower;
      using ::std::toupper;
    }
  }

# undef _IN_C_SWAMP_

#endif
