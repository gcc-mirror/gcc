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
// ISO C++ 14882: <ccytpe>
//

#ifndef _CPP_CCTYPE
#define _CPP_CCTYPE 1

// This keeps isanum, et al from being propagated as macros.
#if __linux__
#define __NO_CTYPE 1
#endif

# include_next <ctype.h>

// Sequester the C non-inline implementations in the _C_Swamp::
// namespace, and provide C++ inlines for them in the std:: namespace
// where they belong.

namespace std 
{
  // NB: If not using namespaces, can't have any of these definitions,
  // as they will duplicate what's in the global namespace. 

#ifdef toupper
  inline int 
  _S_toupper_helper(int __c) { return toupper(__c); }
# undef toupper
  inline int 
  toupper(int __c) { return _S_toupper_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  toupper(int __c) { return ::toupper(__c); }
#endif

#ifdef tolower
  inline int 
  _S_tolower_helper(int __c) { return tolower(__c); }
# undef tolower
  inline int 
  tolower(int __c) { return _S_tolower_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  tolower(int __c) { return ::tolower(__c); }
#endif

#ifdef isspace
  inline int 
  _S_isspace_helper(int __c) { return isspace(__c); }
# undef isspace
  inline int 
  isspace(int __c) { return _S_isspace_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  isspace(int __c) { return ::isspace(__c); }
#endif

#ifdef isprint
  inline int 
  _S_isprint_helper(int __c) { return isprint(__c); }
# undef isprint
  inline int 
  isprint(int __c) { return _S_isprint_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  isprint(int __c) { return ::isprint(__c); }
#endif

#ifdef iscntrl
  inline int 
  _S_iscntrl_helper(int __c) { return iscntrl(__c); }
# undef iscntrl
  inline int 
  iscntrl(int __c) { return _S_iscntrl_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  iscntrl(int __c) { return ::iscntrl(__c); }
#endif

#ifdef isupper
  inline int 
  _S_isupper_helper(int __c) { return isupper(__c); }
# undef isupper
  inline int 
  isupper(int __c) { return _S_isupper_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  isupper(int __c) { return ::isupper(__c); }
#endif

#ifdef islower
  inline int 
  _S_islower_helper(int __c) { return islower(__c); }
# undef islower
  inline int 
  islower(int __c) { return _S_islower_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  islower(int __c) { return ::islower(__c); }
#endif

#ifdef isalpha
  inline int 
  _S_isalpha_helper(int __c) { return isalpha(__c); }
# undef isalpha
  inline int 
  isalpha(int __c) { return _S_isalpha_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  isalpha(int __c) { return ::isalpha(__c); }
#endif

#ifdef isdigit
  inline int 
  _S_isdigit_helper(int __c) { return isdigit(__c); }
# undef isdigit
  inline int 
  isdigit(int __c) { return _S_isdigit_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  isdigit(int __c) { return ::isdigit(__c); }
#endif

#ifdef ispunct
  inline int 
  _S_ispunct_helper(int __c) { return ispunct(__c); }
# undef ispunct
  inline int 
  ispunct(int __c) { return _S_ispunct_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  ispunct(int __c) { return ::ispunct(__c); }
#endif

#ifdef isxdigit
  inline int 
  _S_isxdigit_helper(int __c) { return isxdigit(__c); }
# undef isxdigit
  inline int 
  isxdigit(int __c) { return _S_isxdigit_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  isxdigit(int __c) { return ::isxdigit(__c); }
#endif

#ifdef isalnum
  inline int 
  _S_isalnum_helper(int __c) { return isalnum(__c); }
# undef isalnum
  inline int 
  isalnum(int __c) { return _S_isalnum_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  isalnum(int __c) { return ::isalnum(__c); }
#endif

#ifdef isgraph
  inline int 
  _S_isgraph_helper(int __c) { return isgraph(__c); }
# undef isgraph
  inline int 
  isgraph(int __c) { return _S_isgraph_helper(__c); }
#elif _GLIBCPP_USE_NAMESPACES 
  inline int 
  isgraph(int __c) { return ::isgraph(__c); }
#endif

} // namespace std

#endif // _CPP_CCTYPE












