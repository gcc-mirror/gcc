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

#ifndef _CPP_CFLOAT
#define _CPP_CFLOAT 1

#if 0  /* 1998-09-29 */
# ifdef __GLIBC__
// For GNU libc we must also include this one:
#  include <fenv.h>
# endif
#endif

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <float.h>
    }

    inline int _CPP_FLT_ROUNDS_capture() { return FLT_ROUNDS; }

    // FLT_RADIX is OK as-is.
    // const int     _CPP_FLT_RADIX_capture() { return FLT_RADIX; }
 
    inline int _CPP_FLT_MANT_DIG_capture() { return FLT_MANT_DIG; }
    inline int _CPP_DBL_MANT_DIG_capture() { return DBL_MANT_DIG; }
    inline int _CPP_LDBL_MANT_DIG_capture() { return LDBL_MANT_DIG; }

    inline int _CPP_FLT_DIG_capture() { return FLT_DIG; }
    inline int _CPP_DBL_DIG_capture() { return DBL_DIG; }
    inline int _CPP_LDBL_DIG_capture() { return LDBL_DIG; }

    inline int _CPP_FLT_MIN_EXP_capture() { return FLT_MIN_EXP; }
    inline int _CPP_DBL_MIN_EXP_capture() { return DBL_MIN_EXP; }
    inline int _CPP_LDBL_MIN_EXP_capture() { return LDBL_MIN_EXP; }

    inline int _CPP_FLT_MIN_10_EXP_capture() { return FLT_MIN_10_EXP; }
    inline int _CPP_DBL_MIN_10_EXP_capture() { return DBL_MIN_10_EXP; }
    inline int _CPP_LDBL_MIN_10_EXP_capture() { return LDBL_MIN_10_EXP; }

    inline int _CPP_FLT_MAX_EXP_capture() { return FLT_MAX_EXP; }
    inline int _CPP_DBL_MAX_EXP_capture() { return DBL_MAX_EXP; }
    inline int _CPP_LDBL_MAX_EXP_capture() { return LDBL_MAX_EXP; }

    inline int _CPP_FLT_MAX_10_EXP_capture() { return FLT_MAX_10_EXP; }
    inline int _CPP_DBL_MAX_10_EXP_capture() { return DBL_MAX_10_EXP; }
    inline int _CPP_LDBL_MAX_10_EXP_capture() { return LDBL_MAX_10_EXP; }

    inline float _CPP_FLT_MAX_capture() { return FLT_MAX; }
    inline double _CPP_DBL_MAX_capture() { return DBL_MAX; }
    inline long double _CPP_LDBL_MAX_capture() { return LDBL_MAX; }

    inline float _CPP_FLT_EPSILON_capture() { return FLT_EPSILON; }
    inline double _CPP_DBL_EPSILON_capture() { return DBL_EPSILON; }
    inline long double _CPP_LDBL_EPSILON_capture() { return LDBL_EPSILON; }

    inline float _CPP_FLT_MIN_capture() { return FLT_MIN; }
    inline double _CPP_DBL_MIN_capture() { return DBL_MIN; }
    inline long double _CPP_LDBL_MIN_capture() { return LDBL_MIN; }

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

# undef FLT_ROUNDS
# define FLT_ROUNDS ::_C_Swamp::_CPP_FLT_ROUNDS_capture() 

// # undef FLT_RADIX  // OK as-is.

# undef FLT_MANT_DIG
# define FLT_MANT_DIG ::_C_Swamp::_CPP_FLT_MANT_DIG_capture()
# undef DBL_MANT_DIG
# define DBL_MANT_DIG ::_C_Swamp::_CPP_DBL_MANT_DIG_capture()
# undef LDBL_MANT_DIG
# define LDBL_MANT_DIG ::_C_Swamp::_CPP_LDBL_MANT_DIG_capture()

# undef FLT_DIG
# define FLT_DIG ::_C_Swamp::_CPP_FLT_DIG_capture()
# undef DBL_DIG
# define DBL_DIG ::_C_Swamp::_CPP_DBL_DIG_capture()
# undef LDBL_DIG
# define LDBL_DIG ::_C_Swamp::_CPP_LDBL_DIG_capture()

# undef FLT_MIN_MIN_DIG
# define FLT_MIN_MIN_DIG ::_C_Swamp::_CPP_FLT_MIN_MIN_DIG_capture()
# undef DBL_MIN_MIN_DIG
# define DBL_MIN_MIN_DIG ::_C_Swamp::_CPP_DBL_MIN_MIN_DIG_capture()
# undef LDBL_MIN_MIN_DIG
# define LDBL_MIN_MIN_DIG ::_C_Swamp::_CPP_LDBL_MIN_MIN_DIG_capture()

# undef FLT_MIN_EXP
# define FLT_MIN_EXP ::_C_Swamp::_CPP_FLT_MIN_EXP_capture()
# undef DBL_MIN_EXP
# define DBL_MIN_EXP ::_C_Swamp::_CPP_DBL_MIN_EXP_capture()
# undef LDBL_MIN_EXP
# define LDBL_MIN_EXP ::_C_Swamp::_CPP_LDBL_MIN_EXP_capture()

# undef FLT_MIN_10_EXP
# define FLT_MIN_10_EXP ::_C_Swamp::_CPP_FLT_MIN_10_EXP_capture()
# undef DBL_MIN_10_EXP
# define DBL_MIN_10_EXP _::_C_Swamp::CPP_DBL_MIN_10_EXP_capture()
# undef LDBL_MIN_10_EXP
# define LDBL_MIN_10_EXP ::_C_Swamp::_CPP_LDBL_MIN_10_EXP_capture()

# undef FLT_MAX_EXP
# define FLT_MAX_EXP ::_C_Swamp::_CPP_FLT_MAX_EXP_capture()
# undef DBL_MAX_EXP
# define DBL_MAX_EXP ::_C_Swamp::_CPP_DBL_MAX_EXP_capture()
# undef LDBL_MAX_EXP
# define LDBL_MAX_EXP ::_C_Swamp::_CPP_LDBL_MAX_EXP_capture()

# undef FLT_MAX_10_EXP
# define FLT_MAX_10_EXP ::_C_Swamp::_CPP_FLT_MAX_10_EXP_capture()
# undef DBL_MAX_10_EXP
# define DBL_MAX_10_EXP ::_C_Swamp::_CPP_DBL_MAX_10_EXP_capture()
# undef LDBL_MAX_10_EXP
# define LDBL_MAX_10_EXP ::_C_Swamp::_CPP_LDBL_MAX_10_EXP_capture()

# undef FLT_MAX
# define FLT_MAX ::_C_Swamp::_CPP_FLT_MAX_capture()
# undef DBL_MAX
# define DBL_MAX ::_C_Swamp::_CPP_DBL_MAX_capture()
# undef LDBL_MAX
# define LDBL_MAX ::_C_Swamp::_CPP_LDBL_MAX_capture()

# undef FLT_EPSILON
# define FLT_EPSILON ::_C_Swamp::_CPP_FLT_EPSILON_capture()
# undef DBL_EPSILON
# define DBL_EPSILON ::_C_Swamp::_CPP_DBL_EPSILON_capture()
# undef LDBL_EPSILON
# define LDBL_EPSILON ::_C_Swamp::_CPP_LDBL_EPSILON_capture()

# undef FLT_MIN
# define FLT_MIN ::_C_Swamp::_CPP_FLT_MIN_capture()
# undef DBL_MIN
# define DBL_MIN ::_C_Swamp::_CPP_DBL_MIN_capture()
# undef LDBL_MIN
# define LDBL_MIN ::_C_Swamp::_CPP_LDBL_MIN_capture()

  namespace _C_Swamp {
    namespace _C_Shadow {
    }
  }
  namespace std {
  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
    }
  }

# undef _IN_C_SWAMP_

#endif

