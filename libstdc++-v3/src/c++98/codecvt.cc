// Copyright (C) 2000, 2002, 2004, 2005, 2009 Free Software Foundation, Inc.
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

// Written by Benjamin Kosnik <bkoz@redhat.com>

#include <locale>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Definitions for locale::id of standard facets that are specialized.
 locale::id codecvt<char, char, mbstate_t>::id;

#ifdef _GLIBCXX_USE_WCHAR_T  
  locale::id codecvt<wchar_t, char, mbstate_t>::id;
#endif

  codecvt<char, char, mbstate_t>::
  codecvt(size_t __refs)
  : __codecvt_abstract_base<char, char, mbstate_t>(__refs),
  _M_c_locale_codecvt(_S_get_c_locale())
  { }

  codecvt<char, char, mbstate_t>::
  codecvt(__c_locale __cloc, size_t __refs)
  : __codecvt_abstract_base<char, char, mbstate_t>(__refs),
  _M_c_locale_codecvt(_S_clone_c_locale(__cloc))
  { }

  codecvt<char, char, mbstate_t>::
  ~codecvt()
  { _S_destroy_c_locale(_M_c_locale_codecvt); }
  
  codecvt_base::result
  codecvt<char, char, mbstate_t>::
  do_out(state_type&, const intern_type* __from, 
	 const intern_type*, const intern_type*& __from_next,
	 extern_type* __to, extern_type*, 
	 extern_type*& __to_next) const
  { 
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // According to the resolution of DR19, "If returns noconv [...]
    // there are no changes to the values in [to, to_limit)."
    __from_next = __from; 
    __to_next = __to;
    return noconv;  
  }
  
  codecvt_base::result
  codecvt<char, char, mbstate_t>::
  do_unshift(state_type&, extern_type* __to,
             extern_type*, extern_type*& __to_next) const
  { 
    __to_next = __to; 
    return noconv; 
  }
  
  codecvt_base::result
  codecvt<char, char, mbstate_t>::
  do_in(state_type&, const extern_type* __from, 
	const extern_type*, const extern_type*& __from_next,
	intern_type* __to, intern_type*, intern_type*& __to_next) const
  {
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // According to the resolution of DR19, "If returns noconv [...]
    // there are no changes to the values in [to, to_limit)."
    __from_next = __from; 
    __to_next = __to;
    return noconv;  
  }

  int 
  codecvt<char, char, mbstate_t>::
  do_encoding() const throw() 
  { return 1; }
  
  bool 
  codecvt<char, char, mbstate_t>::
  do_always_noconv() const throw() 
  { return true; }
  
  int 
  codecvt<char, char, mbstate_t>::
  do_length (state_type&, const extern_type* __from,
	     const extern_type* __end, size_t __max) const
  { 
    size_t __d = static_cast<size_t>(__end - __from);
    return std::min(__max, __d); 
  }
  
  int 
  codecvt<char, char, mbstate_t>::
  do_max_length() const throw() 
  { return 1; }
  
#ifdef _GLIBCXX_USE_WCHAR_T
  // codecvt<wchar_t, char, mbstate_t> required specialization
  codecvt<wchar_t, char, mbstate_t>::
  codecvt(size_t __refs)
  : __codecvt_abstract_base<wchar_t, char, mbstate_t>(__refs),
  _M_c_locale_codecvt(_S_get_c_locale())
  { }

  codecvt<wchar_t, char, mbstate_t>::
  codecvt(__c_locale __cloc, size_t __refs)
  : __codecvt_abstract_base<wchar_t, char, mbstate_t>(__refs),
  _M_c_locale_codecvt(_S_clone_c_locale(__cloc))
  { }

  codecvt<wchar_t, char, mbstate_t>::
  ~codecvt()
  { _S_destroy_c_locale(_M_c_locale_codecvt); }
  
  codecvt_base::result
  codecvt<wchar_t, char, mbstate_t>::
  do_unshift(state_type&, extern_type* __to,
	     extern_type*, extern_type*& __to_next) const
  {
    // XXX Probably wrong for stateful encodings
    __to_next = __to;
    return noconv;
  }
  
  bool 
  codecvt<wchar_t, char, mbstate_t>::
  do_always_noconv() const throw()
  { return false; }
#endif //  _GLIBCXX_USE_WCHAR_T

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
