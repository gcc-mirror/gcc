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
// ISO C++ 14882: 
//

#ifndef _CPP_CWCTYPE
#define _CPP_CWCTYPE 1

# include <bits/std_cwchar.h>  

namespace _C_legacy {
  extern "C" {
#     define _IN_C_LEGACY_
#     pragma GCC system_header
#     include_next <wctype.h>
  }
} // namespace _C_legacy


# undef wctype_t
# undef wctrans_t
# undef iswalpha
# undef iswupper
# undef iswlower
# undef iswdigit
# undef iswxdigit
# undef iswalnum
# undef iswspace
# undef iswpunct
# undef iswprint
# undef iswgraph
# undef iswcntrl
# undef iswctype
# undef towctrans
# undef towlower
# undef towupper
# undef wctrans
# undef wctype

namespace std {
  using _C_legacy::wctype_t;
  using _C_legacy::wctrans_t;

  inline int 
  iswalpha(wint_t __wc) { return _C_legacy::iswalpha(__wc); }

  inline int 
  iswupper(wint_t __wc) { return _C_legacy::iswupper(__wc); }

  inline int 
  iswlower(wint_t __wc) { return _C_legacy::iswlower(__wc); }

  inline int 
  iswdigit(wint_t __wc) { return _C_legacy::iswdigit(__wc); }

  inline int 
  iswxdigit(wint_t __wc) { return _C_legacy::iswxdigit(__wc); }

  inline int 
  iswalnum(wint_t __wc) { return _C_legacy::iswalnum(__wc); }

  inline int 
  iswspace(wint_t __wc) { return _C_legacy::iswspace(__wc); }

  inline int 
  iswpunct(wint_t __wc) { return _C_legacy::iswpunct(__wc); }

  inline int 
  iswprint(wint_t __wc) { return _C_legacy::iswprint(__wc); }

  inline int 
  iswgraph(wint_t __wc) { return _C_legacy::iswgraph(__wc); }

  inline int 
  iswcntrl(wint_t __wc) { return _C_legacy::iswcntrl(__wc); }

  inline int 
  towlower(wint_t __wc) { return _C_legacy::towlower(__wc); }

  inline int 
  towupper(wint_t __wc) { return _C_legacy::towupper(__wc); }

  inline int 
  iswctype(wint_t __wc, wctype_t __desc) 
  { return _C_legacy::iswctype(__wc, __desc); }

  inline wint_t 
  towctrans(wint_t __wc, wctrans_t __desc)
  { return _C_legacy::towctrans (__wc, __desc); }
  
  inline wctrans_t 
  wctrans(const char *__property) { return _C_legacy::wctrans(__property); }

  inline wctype_t 
  wctype(char const* __property) { return _C_legacy::wctype(__property); }
} // namespace std

# undef _IN_C_LEGACY_

#endif

