// { dg-do preprocess { target c++11 } }

// Copyright (C) 2008-2019 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <atomic>

#ifndef ATOMIC_BOOL_LOCK_FREE 
# error "ATOMIC_BOOL_LOCK_FREE must be a macro"
#elif ATOMIC_BOOL_LOCK_FREE != 1 && ATOMIC_BOOL_LOCK_FREE != 2
# error "ATOMIC_BOOL_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_CHAR_LOCK_FREE 
# error "ATOMIC_CHAR_LOCK_FREE must be a macro"
#elif ATOMIC_CHAR_LOCK_FREE != 1 && ATOMIC_CHAR_LOCK_FREE != 2
# error "ATOMIC_CHAR_LOCK_FREE must be 1 or 2"
#endif

#ifdef _GLIBCXX_USE_CHAR8_T
# ifndef ATOMIC_CHAR8_T_LOCK_FREE
#  error "ATOMIC_CHAR8_T_LOCK_FREE must be a macro"
# elif ATOMIC_CHAR8_T_LOCK_FREE != 1 && ATOMIC_CHAR8_T_LOCK_FREE != 2
#  error "ATOMIC_CHAR8_T_LOCK_FREE must be 1 or 2"
# endif
#endif

#ifndef ATOMIC_CHAR16_T_LOCK_FREE 
# error "ATOMIC_CHAR16_T_LOCK_FREE must be a macro"
#elif ATOMIC_CHAR16_T_LOCK_FREE != 1 && ATOMIC_CHAR16_T_LOCK_FREE != 2
# error "ATOMIC_CHAR16_T_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_CHAR32_T_LOCK_FREE 
# error "ATOMIC_CHAR32_T_LOCK_FREE must be a macro"
#elif ATOMIC_CHAR32_T_LOCK_FREE != 1 && ATOMIC_CHAR32_T_LOCK_FREE != 2
# error "ATOMIC_CHAR32_T_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_WCHAR_T_LOCK_FREE 
# error "ATOMIC_WCHAR_T_LOCK_FREE must be a macro"
#elif ATOMIC_WCHAR_T_LOCK_FREE != 1 && ATOMIC_WCHAR_T_LOCK_FREE != 2
# error "ATOMIC_WCHAR_T_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_SHORT_LOCK_FREE 
# error "ATOMIC_SHORT_LOCK_FREE must be a macro"
#elif ATOMIC_SHORT_LOCK_FREE != 1 && ATOMIC_SHORT_LOCK_FREE != 2
# error "ATOMIC_SHORT_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_INT_LOCK_FREE 
# error "ATOMIC_INT_LOCK_FREE must be a macro"
#elif ATOMIC_INT_LOCK_FREE != 1 && ATOMIC_INT_LOCK_FREE != 2
# error "ATOMIC_INT_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_LONG_LOCK_FREE 
# error "ATOMIC_LONG_LOCK_FREE must be a macro"
#elif ATOMIC_LONG_LOCK_FREE != 1 && ATOMIC_LONG_LOCK_FREE != 2
# error "ATOMIC_LONG_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_LLONG_LOCK_FREE 
# error "ATOMIC_LLONG_LOCK_FREE must be a macro"
#elif ATOMIC_LLONG_LOCK_FREE != 1 && ATOMIC_LLONG_LOCK_FREE != 2
# error "ATOMIC_LLONG_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_POINTER_LOCK_FREE 
# error "ATOMIC_POINTER_LOCK_FREE must be a macro"
#elif ATOMIC_POINTER_LOCK_FREE != 1 && ATOMIC_POINTER_LOCK_FREE != 2
# error "ATOMIC_POINTER_LOCK_FREE must be 1 or 2"
#endif

#ifndef ATOMIC_FLAG_INIT
    #error "ATOMIC_FLAG_INIT_must_be_a_macro"
#endif

#ifndef ATOMIC_VAR_INIT
    #error "ATOMIC_VAR_INIT_must_be_a_macro"
#endif
