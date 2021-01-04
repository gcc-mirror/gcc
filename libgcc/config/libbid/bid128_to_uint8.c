/* Copyright (C) 2007-2021 Free Software Foundation, Inc.

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

#define SIZE_MASK      0xffffff00
#define INVALID_RESULT 0x80

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_rnint,
				UINT128, x, bid128_to_uint32_rnint,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_xrnint,
				UINT128, x, bid128_to_uint32_xrnint,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_rninta,
				UINT128, x, bid128_to_uint32_rninta,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_xrninta,
				UINT128, x, bid128_to_uint32_xrninta,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_int,
				UINT128, x, bid128_to_uint32_int,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_xint,
				UINT128, x, bid128_to_uint32_xint,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_floor,
				UINT128, x, bid128_to_uint32_floor,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_ceil,
				UINT128, x, bid128_to_uint32_ceil,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_xfloor,
				UINT128, x, bid128_to_uint32_xfloor,
				unsigned int, SIZE_MASK, INVALID_RESULT)

BID_TO_SMALL_UINT_CVT_FUNCTION (unsigned char, bid128_to_uint8_xceil,
				UINT128, x, bid128_to_uint32_xceil,
				unsigned int, SIZE_MASK, INVALID_RESULT)
