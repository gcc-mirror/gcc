/*
 * Copyright (c) 1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */ 

#ifndef __SGI_STL_STRING_FWD_H
#define __SGI_STL_STRING_FWD_H

#include <stddef.h>

__STL_BEGIN_NAMESPACE

#ifdef __STL_USE_STD_ALLOCATORS

template <class _Tp> class allocator;

#else /* __STL_USE_STD_ALLOCATORS */

template <bool __threads, int __inst> class _Default_alloc_template;
typedef _Default_alloc_template<true, 0> _Alloc;

#endif /* __STL_USE_STD_ALLOCATORS */

template <class _CharT> struct char_traits;
template <class _CharT, 
          class _Traits = char_traits<_CharT>, 
          class _Alloc = __STL_DEFAULT_ALLOCATOR(_CharT) >
class basic_string;

typedef basic_string<char> string;
typedef basic_string<wchar_t> wstring;

template <class _CharT, class _Traits, class _Alloc>
void _S_string_copy(const basic_string<_CharT,_Traits,_Alloc>&, _CharT*, 
                    size_t);

__STL_END_NAMESPACE

#endif /* __SGI_STL_STRING_FWD_H */

// Local Variables:
// mode:C++
// End:
