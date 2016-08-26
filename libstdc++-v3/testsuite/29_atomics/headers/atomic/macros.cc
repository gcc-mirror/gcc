// { dg-do compile { target c++11 } }

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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
#endif

#ifndef ATOMIC_CHAR_LOCK_FREE 
# error "ATOMIC_CHAR_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_CHAR16_T_LOCK_FREE 
# error "ATOMIC_CHAR16_T_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_CHAR32_T_LOCK_FREE 
# error "ATOMIC_CHAR32_T_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_WCHAR_T_LOCK_FREE 
# error "ATOMIC_WCHAR_T_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_SHORT_LOCK_FREE 
# error "ATOMIC_SHORT_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_INT_LOCK_FREE 
# error "ATOMIC_INT_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_LONG_LOCK_FREE 
# error "ATOMIC_LONG_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_LLONG_LOCK_FREE 
# error "ATOMIC_LLONG_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_POINTER_LOCK_FREE 
# error "ATOMIC_POINTER_LOCK_FREE must be a macro"
#endif

#ifndef ATOMIC_FLAG_INIT
    #error "ATOMIC_FLAG_INIT_must_be_a_macro"
#endif

#ifndef ATOMIC_VAR_INIT
    #error "ATOMIC_VAR_INIT_must_be_a_macro"
#endif


extern void abort(void);

int main ()
{
#if (ATOMIC_BOOL_LOCK_FREE != 1 && ATOMIC_BOOL_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_CHAR_LOCK_FREE != 1 && ATOMIC_CHAR_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_CHAR16_T_LOCK_FREE != 1 && ATOMIC_CHAR16_T_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_CHAR32_T_LOCK_FREE != 1 && ATOMIC_CHAR32_T_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_WCHAR_T_LOCK_FREE != 1 && ATOMIC_WCHAR_T_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_SHORT_LOCK_FREE != 1 && ATOMIC_SHORT_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_INT_LOCK_FREE != 1 && ATOMIC_INT_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_LONG_LOCK_FREE != 1 && ATOMIC_LONG_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_LLONG_LOCK_FREE != 1 && ATOMIC_LLONG_LOCK_FREE != 2)
   abort ();
#endif

#if (ATOMIC_POINTER_LOCK_FREE != 1 && ATOMIC_POINTER_LOCK_FREE != 2)
   abort ();
#endif
}
