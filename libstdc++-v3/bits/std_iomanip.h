// Standard stream manipulators -*- C++ -*-

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
// ISO C++ 14882: 27.6.3  Standard manipulators
//

#ifndef _CPP_IOMANIP
#define _CPP_IOMANIP 1

#include <bits/c++config.h>
#include <bits/std_istream.h>
#include <bits/std_functional.h>

namespace std {

  struct _Resetiosflags { ios_base::fmtflags _M_mask; };

  inline _Resetiosflags 
  resetiosflags(ios_base::fmtflags __mask)
  { 
    _Resetiosflags __x; 
    __x._M_mask = __mask; 
    return __x; 
  }

  template <class _CharT, class _Traits>
    basic_istream<_CharT,_Traits>& 
    operator>>(basic_istream<_CharT,_Traits>& __is, _Resetiosflags __f)
    { 
      __is.setf(ios_base::fmtflags(0), __f._M_mask); 
      return __is; 
    }

  template <class _CharT, class _Traits>
    basic_ostream<_CharT,_Traits>& 
    operator<<(basic_ostream<_CharT,_Traits>& __os, _Resetiosflags __f)
    { 
      __os.setf(ios_base::fmtflags(0), __f._M_mask); 
      return __os; 
    }


  struct _Setiosflags { ios_base::fmtflags _M_mask; };

  inline _Setiosflags 
  setiosflags (ios_base::fmtflags __mask)
  { 
    _Setiosflags __x; 
    __x._M_mask = __mask; 
    return __x; 
  }

  template <class _CharT, class _Traits>
    basic_istream<_CharT,_Traits>& 
    operator>>(basic_istream<_CharT,_Traits>& __is, _Setiosflags __f)
    { 
      __is.setf(__f._M_mask); 
      return __is; 
    }

  template <class _CharT, class _Traits>
    basic_ostream<_CharT,_Traits>& 
    operator<<(basic_ostream<_CharT,_Traits>& __os, _Setiosflags __f)
    { 
      __os.setf(__f._M_mask); 
      return __os; 
    }


  struct _Setbase { int _M_base; };

  inline _Setbase 
  setbase (int __base)
  { 
    _Setbase __x; 
    __x._M_base = __base; 
    return __x; 
  }

  template <class _CharT, class _Traits>
    basic_istream<_CharT,_Traits>& 
    operator>>(basic_istream<_CharT,_Traits>& __is, _Setbase __f)
    {
      __is.setf(__f._M_base ==  8 ? ios_base::oct : 
	      __f._M_base == 10 ? ios_base::dec : 
	      __f._M_base == 16 ? ios_base::hex : 
	      ios_base::fmtflags(0), ios_base::basefield);
      return __is; 
    }
  
  template <class _CharT, class _Traits>
    basic_ostream<_CharT,_Traits>& 
    operator<<(basic_ostream<_CharT,_Traits>& __os, _Setbase __f)
    {
      __os.setf(__f._M_base ==  8 ? ios_base::oct : 
		__f._M_base == 10 ? ios_base::dec : 
		__f._M_base == 16 ? ios_base::hex : 
		ios_base::fmtflags(0), ios_base::basefield);
      return __os; 
    }
  

  template<class _CharT> 
    struct _Setfill { _CharT _M_c; };

  template<class _CharT> 
    _Setfill<_CharT> 
    setfill(_CharT __c)
    { 
      _Setfill<_CharT> __x; 
      __x._M_c = __c; 
      return __x; 
    }

  template <class _CharT, class _Traits>
    basic_istream<_CharT,_Traits>& 
    operator>>(basic_istream<_CharT,_Traits>& __is, _Setfill<_CharT> __f)
    { 
      __is.fill(__f._M_c); 
      return __is; 
    }

  template <class _CharT, class _Traits>
    basic_ostream<_CharT,_Traits>& 
    operator<<(basic_ostream<_CharT,_Traits>& __os, _Setfill<_CharT> __f)
    { 
      __os.fill(__f._M_c); 
      return __os; 
    }


  struct _Setprecision { int _M_n; };

  inline _Setprecision 
  setprecision(int __n)
  { 
    _Setprecision __x; 
    __x._M_n = __n; 
    return __x; 
  }

  template <class _CharT, class _Traits>
    basic_istream<_CharT,_Traits>& 
    operator>>(basic_istream<_CharT,_Traits>& __is, _Setprecision __f)
    { 
      __is.precision(__f._M_n); 
      return __is; 
    }

  template <class _CharT, class _Traits>
    basic_ostream<_CharT,_Traits>& 
    operator<<(basic_ostream<_CharT,_Traits>& __os, _Setprecision __f)
    { 
      __os.precision(__f._M_n); 
      return __os; 
    }


  struct _Setw { int _M_n; };

  inline _Setw 
  setw(int __n)
  { 
    _Setw __x; 
    __x._M_n = __n; 
    return __x; 
  }

  template <class _CharT, class _Traits>
    basic_istream<_CharT,_Traits>& 
    operator>>(basic_istream<_CharT,_Traits>& __is, _Setw __f)
    { 
      __is.width(__f._M_n); 
      return __is; 
    }

  template <class _CharT, class _Traits>
    basic_ostream<_CharT,_Traits>& 
    operator<<(basic_ostream<_CharT,_Traits>& __os, _Setw __f)
    { 
      __os.width(__f._M_n); 
      return __os; 
    }

} // namespace std

#endif	/* __IOMANIP */





