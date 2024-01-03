/* Copyright (C) 2007-2024 Free Software Foundation, Inc.

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

#include "bid_internal.h"

#define SIZE_MASK      0xffff8000
#define INVALID_RESULT 0x8000

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_rnint, UINT64, x,
			       bid64_to_int32_rnint, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_xrnint, UINT64, x,
			       bid64_to_int32_xrnint, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_rninta, UINT64, x,
			       bid64_to_int32_rninta, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_xrninta, UINT64, x,
			       bid64_to_int32_xrninta, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_int, UINT64, x,
			       bid64_to_int32_int, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_xint, UINT64, x,
			       bid64_to_int32_xint, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_floor, UINT64, x,
			       bid64_to_int32_floor, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_ceil, UINT64, x,
			       bid64_to_int32_ceil, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_xfloor, UINT64, x,
			       bid64_to_int32_xfloor, int, SIZE_MASK,
			       INVALID_RESULT)

BID_TO_SMALL_INT_CVT_FUNCTION (short, bid64_to_int16_xceil, UINT64, x,
			       bid64_to_int32_xceil, int, SIZE_MASK,
			       INVALID_RESULT)
