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
// ISO C++ 14882: 20.5  Date and time
//

#ifndef _CPP_CTIME
#define _CPP_CTIME 1

# include <bits/std_cstddef.h>  /* pick up size_t, NULL */

  namespace _C_Swamp {
    extern "C" {
#     define _IN_C_SWAMP_
#     include_next <time.h>
    }
    inline clock_t _CPP_CLOCKS_PER_SEC_capture() 
      { return CLOCKS_PER_SEC; }
    // typedef size_t    _CPP_size_t_capture;  // handled in <cstddef>
    typedef clock_t   _CPP_clock_t_capture;
    typedef time_t    _CPP_time_t_capture;
    typedef struct tm _CPP_tm_capture;

    namespace _C_Shadow { }
  } // close namespace ::_C_Swamp::

// #  undef  NULL
// #  define NULL 0  /* handled in <cstddef> */
#  undef  CLOCKS_PER_SEC
#  define CLOCKS_PER_SEC (::_C_Swamp::_CPP_CLOCKS_PER_SEC_capture())

#  undef size_t  /* handled in <cstddef> */
#  undef clock_t
#  undef time_t
#  undef tm
#  undef clock
#  undef difftime
#  undef mktime
#  undef time
#  undef asctime
#  undef ctime
#  undef gmtime
#  undef localtime
#  undef strftime

  namespace _C_Swamp {
    namespace _C_Shadow {
      // typedef ::_C_Swamp::_CPP_size_t_capture  size_t;
      typedef ::_C_Swamp::_CPP_clock_t_capture  clock_t;
      typedef ::_C_Swamp::_CPP_time_t_capture   time_t;
    }
  }
  namespace std {

    // Adopt C names into std::
    // using ::_C_Swamp::_C_Shadow::size_t;  
    using ::_C_Swamp::_C_Shadow::clock_t;  
    using ::_C_Swamp::_C_Shadow::time_t;

    // note: still a POD type:
    struct tm  : ::_C_Swamp::_CPP_tm_capture  { };

    using ::_C_Swamp::clock;
    using ::_C_Swamp::difftime;
    using ::_C_Swamp::mktime;
    using ::_C_Swamp::time;

    inline char* asctime(const tm* __tp) 
      { return ::_C_Swamp::asctime(
          static_cast< ::_C_Swamp::_CPP_tm_capture const*>(__tp)); }

    using ::_C_Swamp::ctime;

    inline tm* gmtime(time_t const* __tp) 
      { return reinterpret_cast<tm*>(::_C_Swamp::gmtime(__tp)); }

    inline tm* localtime(const time_t* __tp) 
      { return reinterpret_cast<tm*>(::_C_Swamp::localtime(__tp)); } 

    inline size_t strftime(char* __buf, size_t __maxsz, 
                           char const* __fmt, tm const* __tp) 
      { return ::_C_Swamp::strftime(__buf, __maxsz, __fmt,
                 static_cast< ::_C_Swamp::_CPP_tm_capture const*>(__tp)); }

  } // close namespace std::
  
  namespace _C_Swamp {
    namespace _C_Shadow {
      using ::std::tm;
      using ::std::asctime;
      using ::std::gmtime;
      using ::std::localtime;
      using ::std::strftime;
    }
  }

# undef _IN_C_SWAMP_

#endif

