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
// ISO C++ 14882: 20.4.6  C library
//

#ifndef _CPP_CSTDARG
#define _CPP_CSTDARG 1

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <stdarg.h>
    }
    typedef va_list   _CPP_va_list_capture;
#   ifdef __GNUC__

#   elif
      template <class T>
        inline void _CPP_va_start_capture(va_list& __val, T& __v) 
          { va_start(__val, __v); }
      template <class T>
        inline void _CPP_va_arg_capture(va_list& __val, T& __arg) 
          { va_start(__val, __arg); }
      template <class T>
        inline T& _CPP_va_arg_capture(va_list& __val)
          { return va_arg(__val, T); }
      template <class T>
        inline void _CPP_va_end(va_list& __val)
          { va_end(__val); }
#   endif

    // typedef size_t    _CPP_size_t_capture;  // handled in <cstddef>

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

#  ifdef __GNUC__

#    undef va_list
     using _C_Swamp::__gnuc_va_list;

#  elif  /* probably must be tailored for each compiler, as above. */

#    undef va_list
#    undef va_start
#    define va_start(a,b) ::_C_Swamp::_CPP_va_start_capture(a,b)
#    undef va_arg
#    define va_arg(a,b)   ::_C_Swamp::_CPP_va_arg_capture<b>(a)
#    undef va_end
#    define va_end(a)     ::_C_Swamp::_CPP_va_end_capture(a)

#  endif

  namespace _C_Swamp {
    namespace _C_Shadow {
      typedef ::_C_Swamp::_CPP_va_list_capture va_list;
    }
  }
  namespace std {
    using ::_C_Swamp::_C_Shadow::va_list;  
  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
    }
  }

# undef _IN_C_SWAMP_

#endif

