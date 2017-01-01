/* Copyright (C) 2007-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

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
