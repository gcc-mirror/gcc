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

#include <bits/stl_config.h>
//#include <bits/std_cstddef.h>
#include <bits/stl_alloc.h>

__STL_BEGIN_NAMESPACE

template <class _CharT> struct char_traits;
template <class _CharT, 
          class _Traits = char_traits<_CharT>, 
          class _Alloc = allocator<_CharT> >
class basic_string;

typedef basic_string<char>    string;
#ifdef _GLIBCPP_USE_WCHAR_T
typedef basic_string<wchar_t> wstring;
#endif

__STL_END_NAMESPACE

#endif /* __SGI_STL_STRING_FWD_H */

// Local Variables:
// mode:C++
// End:







