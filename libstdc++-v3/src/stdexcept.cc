// Methods for Exception Support for -*- C++ -*-

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

// Written by Mike Stump based upon the specification in the 20 September 1994
// C++ working paper, ANSI document X3J16/94-0158.

//
// ISO C++ 14882: 19.1  Exception classes
//

#include <bits/std_string.h>
#include <bits/std_stdexcept.h>
#include <bits/stl_range_errors.h>

// To break the circularity with the <stdexcept> and <string> header we
// define two functions which throw exceptions as a direct call would do.

namespace std {

  __Named_exception::__Named_exception(const string& __str)
  {
    strncpy(_M_name, __str.c_str(), _S_bufsize);
    _M_name[_S_bufsize - 1] = '\0';
  }

  void
  __out_of_range(const char *str)
  { throw out_of_range(str); }

  void 
  __length_error(const char *str)
  { throw length_error(str); }

  // XXX: From stl_range_errors.h, eventually these approaches need to
  // be merged.
  void 
  __stl_throw_range_error(const char* __msg) 
  { throw range_error(__msg); }

  void 
  __stl_throw_length_error(const char* __msg)
  { throw length_error(__msg); }

} //namespace std




