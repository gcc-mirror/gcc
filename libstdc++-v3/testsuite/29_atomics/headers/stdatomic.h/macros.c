// { dg-options "-x c" }
// { dg-do compile }

// Copyright (C) 2008 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

#include <stdatomic.h>

int main()
{
#ifndef ATOMIC_INTEGRAL_LOCK_FREE
    #error "ATOMIC_INTEGRAL_LOCK_FREE_must_be_a_macro"
#endif

#ifndef ATOMIC_ADDRESS_LOCK_FREE
    #error "ATOMIC_ADDRESS_LOCK_FREE_must_be_a_macro"
#endif

#ifndef ATOMIC_FLAG_INIT
    #error "ATOMIC_FLAG_INIT_must_be_a_macro"
#endif

#ifndef atomic_is_lock_free
    #error "atomic_is_lock_free_must_be_a_macro"
#endif

#ifndef atomic_load
    #error "atomic_load_must_be_a_macro"
#endif

#ifndef atomic_load_explicit
    #error "atomic_load_explicit_must_be_a_macro"
#endif

#ifndef atomic_store_explicit
    #error "atomic_store_explicit_must_be_a_macro"
#endif

#ifndef atomic_store
    #error "atomic_store_must_be_a_macro"
#endif

#ifndef atomic_swap_explicit
    #error "atomic_swap_explicit_must_be_a_macro"
#endif

#ifndef atomic_swap
    #error "atomic_swap_must_be_a_macro"
#endif

#ifndef atomic_compare_swap
    #error "atomic_compare_swap_must_be_a_macro"
#endif

#ifndef atomic_compare_swap_explicit
    #error "atomic_compare_swap_explicit_must_be_a_macro"
#endif

#ifndef atomic_fence
    #error "atomic_fence_must_be_a_macro"
#endif

#ifndef atomic_fetch_add_explicit
    #error "atomic_fetch_add_explicit_must_be_a_macro"
#endif

#ifndef atomic_fetch_add
    #error "atomic_fetch_add_must_be_a_macro"
#endif

#ifndef atomic_fetch_sub_explicit
    #error "atomic_fetch_sub_explicit_must_be_a_macro"
#endif

#ifndef atomic_fetch_sub
    #error "atomic_fetch_sub_must_be_a_macro"
#endif

#ifndef atomic_fetch_and_explicit
    #error "atomic_fetch_and_explicit_must_be_a_macro"
#endif
 
#ifndef atomic_fetch_and
    #error "atomic_fetch_and_must_be_a_macro"
#endif

#ifndef atomic_fetch_or_explicit
    #error "atomic_fetch_or_explicit_must_be_a_macro"
#endif

#ifndef atomic_fetch_or
    #error "atomic_fetch_or_must_be_a_macro"
#endif

#ifndef atomic_fetch_xor_explicit
    #error "atomic_fetch_xor_explicit_must_be_a_macro"
#endif

#ifndef atomic_fetch_xor
    #error "atomic_fetch_xor_must_be_a_macro"
#endif

  return 0;
}
