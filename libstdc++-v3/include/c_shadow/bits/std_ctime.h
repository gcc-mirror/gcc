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

//
// ISO C++ 14882: 20.5  Date and time
//

#ifndef _CPP_CTIME
#define _CPP_CTIME 1

# include <bits/std_cstddef.h>  

namespace _C_legacy {
  extern "C" {
#     define _IN_C_LEGACY_
#     pragma GCC system_header

      // XXX
      // glibc 2.1.x time.h is on crack
#     undef __need_time_t
#     undef __need_clock_t
#     undef __need_timespec

#     include_next <time.h>
  }

  typedef clock_t	_CPP_clock_t_capture;
  typedef time_t    	_CPP_time_t_capture;
  typedef tm 		_CPP_tm_capture;

} // namespace _C_legacy

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

namespace std {

  // Adopt C names into std::
  typedef _C_legacy::_CPP_clock_t_capture  clock_t;
  typedef _C_legacy::_CPP_time_t_capture   time_t;
  struct tm : _C_legacy::_CPP_tm_capture  { };

  using _C_legacy::clock;
  using _C_legacy::difftime;
  using _C_legacy::mktime;
  using _C_legacy::time;
  using _C_legacy::ctime;

  inline char* 
  asctime(const tm* __t) 
  { return _C_legacy::asctime(static_cast<_C_legacy::_CPP_tm_capture const*>(__t)); }

  inline tm* 
  gmtime(time_t const* __tp) 
  { return reinterpret_cast<tm*>(_C_legacy::gmtime(__tp)); }

  inline tm* 
  localtime(const time_t* __tp) 
  { return reinterpret_cast<tm*>(_C_legacy::localtime(__tp)); } 
    
  inline size_t 
  strftime(char* __buf, size_t __maxsz, char const* __fmt, tm const* __tp) 
  { return _C_legacy::strftime(__buf, __maxsz, __fmt,
	       	       static_cast<_C_legacy::_CPP_tm_capture const*>(__tp)); }

} // namespace std
  
# undef _IN_C_LEGACY_

#endif

