/* Copyright (C) 2009-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

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

#ifndef _UPC_TYPES_H_
#define _UPC_TYPES_H_


typedef int upc_type_t;

#define UPC_CHAR        1
#define UPC_UCHAR       2
#define UPC_SHORT       3
#define UPC_USHORT      4
#define UPC_INT         5
#define UPC_UINT        6
#define UPC_LONG        7
#define UPC_ULONG       8
#define UPC_LLONG       9
#define UPC_ULLONG      10
#define UPC_INT8        11
#define UPC_UINT8       12
#define UPC_INT16       13
#define UPC_UINT16      14
#define UPC_INT32       15
#define UPC_UINT32      16
#define UPC_INT64       17
#define UPC_UINT64      18
#define UPC_FLOAT       19
#define UPC_DOUBLE      20
#define UPC_LDOUBLE     21
#define UPC_PTS         22

/* Flag type for synchronization semantics
   (and potentially other uses).  */

typedef int upc_flag_t;

/* Synchronization flags.  */

#define UPC_IN_ALLSYNC       (1<<0)
#define UPC_IN_MYSYNC        (1<<1)
#define UPC_IN_NOSYNC        (1<<2)
#define UPC_OUT_ALLSYNC      (1<<3)
#define UPC_OUT_MYSYNC       (1<<4)
#define UPC_OUT_NOSYNC       (1<<5)

/* Operation type for upc_all_reduceT() and upc_all_prefix_reduceT().  */

typedef unsigned long upc_op_t;

#define UPC_ADD         (1UL<<0)
#define UPC_MULT        (1UL<<1)
#define UPC_AND         (1UL<<2)
#define UPC_OR          (1UL<<3)
#define UPC_XOR         (1UL<<4)
#define UPC_LOGAND      (1UL<<5)
#define UPC_LOGOR       (1UL<<6)
#define UPC_MIN         (1UL<<7)
#define UPC_MAX         (1UL<<8)

#endif /* _UPC_TYPES_H_ */
