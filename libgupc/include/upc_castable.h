/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Derived from public domain reference implementation
   written by Dan Bonachea <danbonachea@gmail.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#ifndef _UPC_CASTABLE_H_
#define _UPC_CASTABLE_H_

#if __UPC_CASTABLE__ != 1
#error Bad feature macro predefinition
#endif

#include <stddef.h>		/* size_t */

#define UPC_CASTABLE_ALL_ALLOC      (1<<0)
#define UPC_CASTABLE_GLOBAL_ALLOC   (1<<1)
#define UPC_CASTABLE_ALLOC          (1<<2)
#define UPC_CASTABLE_STATIC         (1<<3)

#define UPC_CASTABLE_ALL  (            \
           UPC_CASTABLE_ALL_ALLOC    | \
           UPC_CASTABLE_GLOBAL_ALLOC | \
           UPC_CASTABLE_ALLOC        | \
           UPC_CASTABLE_STATIC         \
         )

typedef struct _S_upc_thread_info
{
  int guaranteedCastable;
  int probablyCastable;
} upc_thread_info_t;


void *upc_cast (const shared void *);

upc_thread_info_t upc_thread_info (size_t);

#endif /* _UPC_CASTABLE_H_ */
