// -*- C++ -*- forwarding header.

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
// ISO C++ 14882: <cwctype>
//

#ifndef _CPP_CWCTYPE
#define _CPP_CWCTYPE 1

// This keeps isanum, et al from being propagated as macros.
#if __linux__
#define __NO_WCTYPE 1
#endif

# include_next <wctype.h>

// Sequester the C non-inline implementations in the _C_Swamp::
// namespace, and provide C++ inlines for them in the std:: namespace
// where they belong.

namespace std 
{

#ifdef towupper
  inline wint_t 
  _S_towupper_helper(wint_t __wc) { return towupper(__wc); }
# undef towupper
  inline wint_t 
  towupper(wint_t __wc) { return _S_towupper_helper(__wc); }
#endif

#ifdef towlower
  inline wint_t 
  _S_towlower_helper(wint_t __wc) { return towlower(__wc); }
# undef towlower
  inline wint_t 
  towlower(wint_t __wc) { return _S_towlower_helper(__wc); }
#endif

#ifdef iswspace
  inline int 
  _S_iswspace_helper(wint_t __wc) { return iswspace(__wc); }
# undef iswspace
  inline int 
  iswspace(wint_t __wc) { return _S_iswspace_helper(__wc); }
#endif

#ifdef iswprint
  inline int 
  _S_iswprint_helper(wint_t __wc) { return iswprint(__wc); }
# undef iswprint
  inline int 
  iswprint(wint_t __wc) { return _S_iswprint_helper(__wc); }
#endif

#ifdef iswcntrl
  inline int 
  _S_iswcntrl_helper(wint_t __wc) { return iswcntrl(__wc); }
# undef iswcntrl
  inline int 
  iswcntrl(wint_t __wc) { return _S_iswcntrl_helper(__wc); }
#endif

#ifdef iswupper
  inline int 
  _S_iswupper_helper(wint_t __wc) { return iswupper(__wc); }
# undef iswupper
  inline int 
  iswupper(wint_t __wc) { return _S_iswupper_helper(__wc); }
#endif

#ifdef iswlower
  inline int 
  _S_iswlower_helper(wint_t __wc) { return iswlower(__wc); }
# undef iswlower
  inline int 
  iswlower(wint_t __wc) { return _S_iswlower_helper(__wc); }
#endif

#ifdef iswalpha
  inline int 
  _S_iswalpha_helper(wint_t __wc) { return iswalpha(__wc); }
# undef iswalpha
  inline int 
  iswalpha(wint_t __wc) { return _S_iswalpha_helper(__wc); }
#endif

#ifdef iswdigit
  inline int 
  _S_iswdigit_helper(wint_t __wc) { return iswdigit(__wc); }
# undef iswdigit
  inline int 
  iswdigit(wint_t __wc) { return _S_iswdigit_helper(__wc); }
#endif

#ifdef iswpunct
  inline int 
  _S_iswpunct_helper(wint_t __wc) { return iswpunct(__wc); }
# undef iswpunct
  inline int 
  iswpunct(wint_t __wc) { return _S_iswpunct_helper(__wc); }
#endif

#ifdef iswxdigit
  inline int 
  _S_iswxdigit_helper (wint_t __wc) { return iswxdigit(__wc); }
# undef iswxdigit
  inline int 
  iswxdigit(wint_t __wc) { return _S_iswxdigit_helper(__wc); }
#endif

#ifdef iswalnum
  inline int 
  _S_iswalnum_helper(wint_t __wc) { return iswalnum(__wc); }
# undef iswalnum
  inline int 
  iswalnum(wint_t __wc) { return _S_iswalnum_helper(__wc); }
#endif

#ifdef iswgraph
  inline int 
  _S_iswgraph_helper(wint_t __wc) { return iswgraph(__wc); }
# undef iswgraph
  inline int 
  iswgraph(wint_t __wc) { return _S_iswgraph_helper(__wc); }
#endif

} // namespace std

#endif // _CPP_CWCTYPE







