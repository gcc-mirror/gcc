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

//
// ISO C++ 14882: 18.2.2  Implementation properties: C library
//

#ifndef _CPP_CLOCALE
#define _CPP_CLOCALE     1

# include <bits/std_cstddef.h> /* pick up NULL */

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <locale.h>
    }

    typedef struct lconv _CPP_lconv_capture;
    const int _CPP_LC_ALL_capture = LC_ALL;
    const int _CPP_LC_COLLATE_capture = LC_COLLATE;
    const int _CPP_LC_CTYPE_capture = LC_CTYPE;
    const int _CPP_LC_MONETARY_capture = LC_MONETARY;
    const int _CPP_LC_NUMERIC_capture = LC_NUMERIC;
    const int _CPP_LC_TIME_capture = LC_TIME;
#if 0 /* XXX need proper macro guard for this common extension. */
    const int _CPP_LC_MESSAGES_capture = LC_MESSAGES;
#endif

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

// #  undef  NULL
// #  define NULL 0  /* handled in <cstddef> */
#  undef LC_ALL
#  define LC_ALL	::_C_Swamp::_CPP_LC_ALL_capture
#  undef LC_COLLATE
#  define LC_COLLATE	::_C_Swamp::_CPP_LC_COLLATE_capture
#  undef LC_CTYPE
#  define LC_CTYPE	::_C_Swamp::_CPP_LC_CTYPE_capture
#  undef LC_MONETARY
#  define LC_MONETARY	::_C_Swamp::_CPP_LC_MONETARY_capture
#  undef LC_NUMERIC
#  define LC_NUMERIC	::_C_Swamp::_CPP_LC_NUMERIC_capture
#  undef LC_TIME
#  define LC_TIME	::_C_Swamp::_CPP_LC_TIME_capture
#if 0 /* XXX need proper macro guard for this common extension. */
#  undef LC_MESSAGES
#  define LC_MESSAGES	::_C_Swamp::_CPP_LC_MESSAGES_capture
#endif

#  undef lconv
#  undef setlocale
#  undef localeconv

  namespace _C_Swamp {
    namespace _C_Shadow {
    }
  }
  namespace std {

    // Adopt C names into std::
    using ::_C_Swamp::setlocale;  

    // note: still a POD type:
    struct lconv  : ::_C_Swamp::_CPP_lconv_capture  { };

    inline lconv* localeconv() 
      { return reinterpret_cast<lconv*>(::_C_Swamp::localeconv()); }

  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
      using ::std::lconv;
      using ::std::localeconv;
    }
  }

# undef _IN_C_SWAMP_

#endif /* _CPP_CLOCALE */
