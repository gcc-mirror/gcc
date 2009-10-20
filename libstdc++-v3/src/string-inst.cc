// Components for manipulating sequences of characters -*- C++ -*-

// Copyright (C) 1997, 1998, 2009, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
// Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

//
// ISO C++ 14882: 21  Strings library
//

// Written by Jason Merrill based upon the specification by Takanori Adachi
// in ANSI X3J16/94-0013R2.  Rewritten by Nathan Myers.

#include <string>

// Instantiation configuration.
#ifndef C
# define C char
#endif

_GLIBCXX_BEGIN_NAMESPACE(std)

  typedef basic_string<C> S;

  template class basic_string<C>;
  template S operator+(const C*, const S&);
  template S operator+(C, const S&);
  template S operator+(const S&, const S&);

  // Only one template keyword allowed here. 
  // See core issue #46 (NAD)
  // http://anubis.dkuug.dk/jtc1/sc22/wg21/docs/cwg_closed.html#46
  template
    S::basic_string(C*, C*, const allocator<C>&);

  template
    S::basic_string(const C*, const C*, const allocator<C>&);

  template 
    S::basic_string(S::iterator, S::iterator, const allocator<C>&);

  template 
    C* 
    S::_S_construct(S::iterator, S::iterator, 
		    const allocator<C>&, forward_iterator_tag);

  template
    C*
    S::_S_construct(C*, C*, const allocator<C>&, forward_iterator_tag);

  template
    C*
    S::_S_construct(const C*, const C*, const allocator<C>&,
		    forward_iterator_tag);

_GLIBCXX_END_NAMESPACE

_GLIBCXX_BEGIN_NAMESPACE(__gnu_cxx)

  using std::S;
  template bool operator==(const S::iterator&, const S::iterator&);
  template bool operator==(const S::const_iterator&, const S::const_iterator&);

_GLIBCXX_END_NAMESPACE
