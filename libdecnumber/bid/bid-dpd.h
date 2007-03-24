/* Copyright (C) 2007
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

typedef unsigned int UINT32;
typedef unsigned long long UINT64;
typedef struct { UINT64 w[2]; } UINT128;

#ifndef IN_LIBGCC2
#define _Decimal32 UINT32
#define _Decimal64 UINT64
#define _Decimal128 UINT128
#endif

void _bid_to_dpd32 (_Decimal32 *, _Decimal32 *);
void _dpd_to_bid32 (_Decimal32 *, _Decimal32 *);
void _bid_to_dpd64 (_Decimal64 *, _Decimal64 *);
void _dpd_to_bid64 (_Decimal64 *, _Decimal64 *);
void _bid_to_dpd128 (_Decimal128 *, _Decimal128 *);
void _dpd_to_bid128 (_Decimal128 *, _Decimal128 *);
