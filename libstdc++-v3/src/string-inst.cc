// Components for manipulating sequences of characters -*- C++ -*-

// Copyright (C) 2000, 1999, 1998, 1997 Free Software Foundation, Inc.
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
// ISO C++ 14882: 21  Strings library
//

// Written by Jason Merrill based upon the specification by Takanori Adachi
// in ANSI X3J16/94-0013R2.  Rewritten by Nathan Myers.

#include <bits/std_string.h>
#include <bits/std_algorithm.h>

// Instantiation configuration.
#ifndef C
# define C char
# define _GLIBCPP_INSTANTIATING_CHAR 1
#endif

namespace std 
{
  typedef basic_string<C> S;

  template C S::_Rep::_S_terminal;
  template const S::size_type S::npos;
  template S::size_type S::_Rep::_S_max_size;
  template S::size_type S::_S_empty_rep_storage[];
  template S::_Rep* S::_Rep::_S_create(size_t, S::allocator_type const&);
  template void S::_Rep::_M_destroy(const S::allocator_type&) throw();
  template void __destroy_aux(S*, S*, __false_type);

  template S::basic_string(S const&);

  template 
    S::basic_string(S::size_type, C, const S::allocator_type&);

  template 
    S::basic_string(const S::allocator_type&);

  template 
    S::basic_string(const S&, S::size_type, S::size_type);

  template 
    S::basic_string(const S&, S::size_type, S::size_type, 
		    const S::allocator_type&);

  template 
    S::basic_string(C const*, S::size_type, const S::allocator_type&);

  template 
    S::basic_string(C const*, S::allocator_type const&);

  template 
    S::basic_string(C*, C*, const allocator<C>&);

  template 
    S::basic_string(S::iterator, S::iterator, const allocator<C>&);

  template
    void S::_M_leak_hard();

  template
    void S::_M_mutate(S::size_type, S::size_type, S::size_type);

  template
    C* S::_Rep::_M_clone(S::allocator_type const&, S::size_type);

  template
    void S::reserve(S::size_type);

  template
    void S::swap(S&);

# ifdef _GLIBCPP_ALLOC_CONTROL
    template
      bool (* S::_Rep::_S_excess_slop)(size_t, size_t); 

    template
      bool S::_Rep::_S_default_excess(size_t, size_t); 
# endif

  template
    void S::resize(S::size_type, C);

  template
    S& S::append(S const&);

  template
    S& S::append(S const&, S::size_type, S::size_type);

  template
    S& S::append(C const*, S::size_type);

  template
    S& S::append(S::size_type, C);

  template 
    S& 
    S::append<S::iterator>(S::iterator, S::iterator);

  template
    S& 
    S::assign(S const&);

  template 
    S& 
    S::assign<S::iterator>(S::iterator, S::iterator);

  template 
    void
    S::insert<S::iterator> //c*
    (S::iterator, S::iterator, S::iterator); //it, c+, c+ and temptype = char*

  template
    S& S::replace(S::size_type, S::size_type, S const&, 
		  S::size_type, S::size_type);

  template 
    S& S::replace(S::iterator, S::iterator, S::size_type, C);

  template 
    S&
    S::replace<S::iterator> // c*
    (S::iterator, S::iterator, S::iterator, S::iterator); //it, it, c+, c+ 

  template 
    S& 
    S::_M_replace<S::iterator>
    (S::iterator, S::iterator, S::iterator, S::iterator, forward_iterator_tag);

  // Only one template keyword allowed here. 
  // See core issue #46 (NAD)
  // http://anubis.dkuug.dk/jtc1/sc22/wg21/docs/cwg_closed.html#46
  template 
    S& 
    S::_M_replace<S::const_iterator>
    (S::iterator, S::iterator, 
     S::const_iterator, S::const_iterator, forward_iterator_tag);

  template 
    S& 
    S::_M_replace<C*>
    (S::iterator, S::iterator, C*, C*, forward_iterator_tag);

  template 
    S& 
    S::_M_replace<const C*>
    (S::iterator, S::iterator, const C*, const C*, forward_iterator_tag);

  template
    S::size_type  S::copy(C*, S::size_type, S::size_type) const;

  template 
    C* 
    S::_S_construct<S::iterator>
    (S::iterator, S::iterator, const allocator<C>&);

  template 
    C* 
    S::_S_construct<S::iterator>
    (S::iterator, S::iterator, const allocator<C>&, forward_iterator_tag);

  template 
    C* 
    S::_S_construct<C*>
    (C*, C*, const allocator<C>&, forward_iterator_tag);

  template 
    C* 
    S::_S_construct<const C*>
    (const C*, const C*, const allocator<C>&, forward_iterator_tag);

  template 
    C* 
    S::_S_construct(S::size_type, C, S::allocator_type const&);

  // These members are explicitly specialized, and can only be in one
  // translation unit or else we get multiple copies. . . 
#if _GLIBCPP_INSTANTIATING_CHAR
  template<>
    const char* 
    string::_S_find(const char* __beg, const char* __end, char __c)
    { 
      const char* __ret = strchr(__beg, __c); 
      return (__ret ? __ret : __end);
    }
#elif defined(_GLIBCPP_USE_WCHAR_T)
  template<>
    const wchar_t* 
    wstring::_S_find(const wchar_t* __beg, const wchar_t* __end, wchar_t __c)
    {
      return find_if(__beg, __end, 
		     _Char_traits_match<wchar_t, traits_type>(__c));
    }
#endif

  template
    S::size_type S::find(C, S::size_type) const;

  template
    S::size_type S::rfind(C const*, S::size_type, S::size_type) const;

  template
    S::size_type S::rfind(C, S::size_type) const;

  template
    S::size_type S::find_first_of(C const*, S::size_type, S::size_type) const;

  template
    S::size_type S::find_last_of(C const*, S::size_type, S::size_type) const;

  template
    S::size_type S::find_first_not_of(
      C const*, S::size_type, S::size_type) const;

  template
    S::size_type S::find_last_not_of(
      C const*, S::size_type, S::size_type) const;

  template
    S::size_type S::find_last_not_of(C, S::size_type) const;

  template
    int S::compare(S::size_type, S::size_type, S const&) const;

  template
    int S::compare(S::size_type, S::size_type, S const&, S::size_type, 
		   S::size_type) const;

  template
    int S::compare(C const*) const;

  template
    int S::compare(
      S::size_type, S::size_type, C const*, S::size_type) const;

  template S operator+(const C*, const S&);

  template S operator+(C, const S&);

  template bool operator==(const S::iterator&, const S::iterator&);
  template bool operator==(const S::const_iterator&, const S::const_iterator&);

  template void _S_string_copy(const S&, C*, allocator<C>::size_type);

} // std




