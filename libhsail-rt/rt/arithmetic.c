/* arithmetic.c -- Builtins for HSAIL arithmetic instructions for which
   there is no feasible direct gcc GENERIC expression.

   Copyright (C) 2015-2019 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#include <math.h>
#include <float.h>

/* HSAIL defines INT_MIN % -1 to be 0 while with C it's undefined,
   and causes an overflow exception at least with gcc and C on IA-32.  */

int32_t
__hsail_rem_s32 (int32_t dividend, int32_t divisor)
{
  if (dividend == INT_MIN && divisor == -1)
    return 0;
  else
    return dividend % divisor;
}

int64_t
__hsail_rem_s64 (int64_t dividend, int64_t divisor)
{
  if (dividend == INT64_MIN && divisor == -1)
    return 0;
  else
    return dividend % divisor;
}

/* HSAIL has defined behavior for min and max when one of the operands is
   NaN: in that case the other operand is returned.  In C and with gcc's
   MIN_EXPR/MAX_EXPR, the returned operand is undefined.  */

float
__hsail_min_f32 (float a, float b)
{
  if (isnan (a))
    return b;
  else if (isnan (b))
    return a;
  else if (a == 0.0f && b == 0.0f)
    return signbit (a) ? a : b;
  else if (a > b)
    return b;
  else
    return a;
}

double
__hsail_min_f64 (double a, double b)
{
  if (isnan (a))
    return b;
  else if (isnan (b))
    return a;
  else if (a > b)
    return b;
  else
    return a;
}

float
__hsail_max_f32 (float a, float b)
{
  if (isnan (a))
    return b;
  else if (isnan (b))
    return a;
  else if (a == 0.0f && b == 0.0f && signbit (a))
    return b;
  else if (a < b)
    return b;
  else
    return a;
}

double
__hsail_max_f64 (double a, double b)
{
  if (isnan (a))
    return b;
  else if (isnan (b))
    return a;
  else if (a == 0.0 && b == 0.0 && signbit (a))
    return b;
  else if (a < b)
    return b;
  else
    return a;
}

uint8_t
__hsail_cvt_zeroi_sat_u8_f32 (float a)
{
  if (isnan (a))
    return 0;
  if (a >= (float) UINT8_MAX)
    return UINT8_MAX;
  else if (a <= 0.0f)
    return 0;
  return (uint8_t) a;
}

int8_t
__hsail_cvt_zeroi_sat_s8_f32 (float a)
{
  if (isnan (a))
    return 0;
  if (a >= (float) INT8_MAX)
    return INT8_MAX;
  if (a <= (float) INT8_MIN)
    return INT8_MIN;
  return (int8_t) a;
}

uint16_t
__hsail_cvt_zeroi_sat_u16_f32 (float a)
{
  if (isnan (a))
    return 0;
  if (a >= (float) UINT16_MAX)
    return UINT16_MAX;
  else if (a <= 0.0f)
    return 0;
  return (uint16_t) a;
}

int16_t
__hsail_cvt_zeroi_sat_s16_f32 (float a)
{
  if (isnan (a))
    return 0;
  if (a >= (float) INT16_MAX)
    return INT16_MAX;
  if (a <= (float) INT16_MIN)
    return INT16_MIN;
  return (int16_t) a;
}

uint32_t
__hsail_cvt_zeroi_sat_u32_f32 (float a)
{
  if (isnan (a))
    return 0;
  if (a >= (float) UINT32_MAX)
    return UINT32_MAX;
  else if (a <= 0.0f)
    return 0;
  return (uint32_t) a;
}

int32_t
__hsail_cvt_zeroi_sat_s32_f32 (float a)
{
  if (isnan (a))
    return 0;
  if (a >= (float) INT32_MAX)
    return INT32_MAX;
  if (a <= (float) INT32_MIN)
    return INT32_MIN;
  return (int32_t) a;
}

uint64_t
__hsail_cvt_zeroi_sat_u64_f32 (float a)
{
  if (isnan (a))
    return 0;
  if (a >= (float) UINT64_MAX)
    return UINT64_MAX;
  else if (a <= 0.0f)
    return 0;
  return (uint64_t) a;
}

int64_t
__hsail_cvt_zeroi_sat_s64_f32 (float a)
{
  if (isnan (a))
    return 0;
  if (a >= (float) INT64_MAX)
    return INT64_MAX;
  if (a <= (float) INT64_MIN)
    return INT64_MIN;
  return (int64_t) a;
}

uint8_t
__hsail_cvt_zeroi_sat_u8_f64 (double a)
{
  if (isnan (a))
    return 0;
  if (a >= (double) UINT8_MAX)
    return UINT8_MAX;
  else if (a <= 0.0f)
    return 0;
  return (uint8_t) a;
}

int8_t
__hsail_cvt_zeroi_sat_s8_f64 (double a)
{
  if (isnan (a))
    return 0;
  if (a >= (double) INT8_MAX)
    return INT8_MAX;
  if (a <= (double) INT8_MIN)
    return INT8_MIN;
  return (int8_t) a;
}

uint16_t
__hsail_cvt_zeroi_sat_u16_f64 (double a)
{
  if (isnan (a))
    return 0;
  if (a >= (double) UINT16_MAX)
    return UINT16_MAX;
  else if (a <= 0.0f)
    return 0;
  return (uint16_t) a;
}

int16_t
__hsail_cvt_zeroi_sat_s16_f64 (double a)
{
  if (isnan (a))
    return 0;
  if (a >= (double) INT16_MAX)
    return INT16_MAX;
  if (a <= (double) INT16_MIN)
    return INT16_MIN;
  return (int16_t) a;
}

uint32_t
__hsail_cvt_zeroi_sat_u32_f64 (double a)
{
  if (isnan (a))
    return 0;
  if (a >= (double) UINT32_MAX)
    return UINT32_MAX;
  else if (a <= 0.0f)
    return 0;
  return (uint32_t) a;
}

int32_t
__hsail_cvt_zeroi_sat_s32_f64 (double a)
{
  if (isnan (a))
    return 0;
  if (a >= (double) INT32_MAX)
    return INT32_MAX;
  if (a <= (double) INT32_MIN)
    return INT32_MIN;
  return (int32_t) a;
}

uint64_t
__hsail_cvt_zeroi_sat_u64_f64 (double a)
{
  if (isnan (a))
    return 0;
  if (a >= (double) UINT64_MAX)
    return UINT64_MAX;
  else if (a <= 0.0f)
    return 0;
  return (uint64_t) a;
}

int64_t
__hsail_cvt_zeroi_sat_s64_f64 (double a)
{
  if (isnan (a))
    return 0;
  if (a >= (double) INT64_MAX)
    return INT64_MAX;
  if (a <= (double) INT64_MIN)
    return INT64_MIN;
  return (int64_t) a;
}


/* Flush the operand to zero in case it's a denormalized number.
   Do not cause any exceptions in case of NaNs.  */

float
__hsail_ftz_f32 (float a)
{
  if (isnan (a) || isinf (a) || a == 0.0f)
    return a;

  if (a < 0.0f)
    {
      if (-a < FLT_MIN)
	return -0.0f;
    }
  else
    {
      if (a < FLT_MIN)
	return 0.0f;
    }
  return a;
}

#define F16_MIN (6.10e-5)

/* Flush the single precision operand to zero in case it's considered
   a denormalized number in case it was a f16.  Do not cause any exceptions
   in case of NaNs.  */

float
__hsail_ftz_f32_f16 (float a)
{
  if (isnan (a) || isinf (a) || a == 0.0f)
    return a;

  if (a < 0.0f)
    {
      if (-a < F16_MIN)
	return -0.0f;
    }
  else
    {
      if (a < F16_MIN)
	return 0.0f;
    }
  return a;
}

double
__hsail_ftz_f64 (double a)
{
  if (isnan (a) || isinf (a) || a == 0.0d)
    return a;

  if (a < 0.0d)
    {
      if (-a < DBL_MIN)
	return -0.0d;
    }
  else
    {
      if (a < DBL_MIN)
	return 0.0d;
    }
  return a;
}

uint32_t
__hsail_borrow_u32 (uint32_t a, uint32_t b)
{
  return __builtin_sub_overflow_p (a, b, a);
}

uint64_t
__hsail_borrow_u64 (uint64_t a, uint64_t b)
{
  return __builtin_sub_overflow_p (a, b, a);
}

uint32_t
__hsail_carry_u32 (uint32_t a, uint32_t b)
{
  return __builtin_add_overflow_p (a, b, a);
}

uint64_t
__hsail_carry_u64 (uint64_t a, uint64_t b)
{
  return __builtin_add_overflow_p (a, b, a);
}

float
__hsail_fract_f32 (float a)
{
  int exp;
  if (isinf (a))
    return signbit (a) == 0 ? 0.0f : -0.0f;
  if (isnan (a) || a == 0.0f)
    return a;
  else
    return fminf (a - floorf (a), 0x1.fffffep-1f);
}

double
__hsail_fract_f64 (double a)
{
  int exp;
  if (isinf (a))
    return 0.0f * isinf (a);
  if (isnan (a) || a == 0.0f)
    return a;
  else
    return fmin (a - floor (a), 0x1.fffffffffffffp-1d);
}

uint32_t
__hsail_class_f32 (float a, uint32_t flags)
{
  return (flags & 0x0001 && isnan (a) && !(*(uint32_t *) &a & (1ul << 22)))
    || (flags & 0x0002 && isnan (a) && (*(uint32_t *) &a & (1ul << 22)))
    || (flags & 0x0004 && isinf (a) && a < 0.0f)
    || (flags & 0x0008 && isnormal (a) && signbit (a))
    || (flags & 0x0010 && a < 0.0f && a > -FLT_MIN)
    || (flags & 0x0020 && a == 0.0f && signbit (a))
    || (flags & 0x0040 && a == 0.0f && !signbit (a))
    || (flags & 0x0080 && a > 0.0f && a < FLT_MIN)
    || (flags & 0x0100 && isnormal (a) && !signbit (a))
    || (flags & 0x0200 && isinf (a) && a >= 0.0f);
}

uint32_t
__hsail_class_f64 (double a, uint32_t flags)
{
  return (flags & 0x0001 && isnan (a) && !(*(uint64_t *) &a & (1ul << 51)))
    || (flags & 0x0002 && isnan (a) && (*(uint64_t *) &a & (1ul << 51)))
    || (flags & 0x0004 && isinf (a) && a < 0.0f)
    || (flags & 0x0008 && isnormal (a) && signbit (a))
    || (flags & 0x0010 && a < 0.0f && a > -FLT_MIN)
    || (flags & 0x0020 && a == 0.0f && signbit (a))
    || (flags & 0x0040 && a == 0.0f && !signbit (a))
    || (flags & 0x0080 && a > 0.0f && a < FLT_MIN)
    || (flags & 0x0100 && isnormal (a) && !signbit (a))
    || (flags & 0x0200 && isinf (a) && a >= 0.0f);
}


/* 'class' for a f32-converted f16 which should otherwise be treated like f32
 except for its limits.  */

uint32_t
__hsail_class_f32_f16 (float a, uint32_t flags)
{
  return (flags & 0x0001 && isnan (a) && !(*(uint32_t *) &a & 0x40000000))
	 || (flags & 0x0002 && isnan (a) && (*(uint32_t *) &a & 0x40000000))
	 || (flags & 0x0004 && isinf (a) && a < 0.0f)
	 || (flags & 0x0008 && a != 0.0f && !isinf (a) && !isnan (a)
	     && a <= -F16_MIN)
	 || (flags & 0x0010 && a != 0.0f && !isinf (a) && !isnan (a) && a < 0.0f
	     && a > -F16_MIN)
	 || (flags & 0x0020 && a == 0.0f && signbit (a))
	 || (flags & 0x0040 && a == 0.0f && !signbit (a))
	 || (flags & 0x0080 && a != 0.0f && !isinf (a) && !isnan (a) && a > 0.0f
	     && a < F16_MIN)
	 || (flags & 0x0100 && a != 0.0f && !isinf (a) && !isnan (a)
	     && a >= F16_MIN)
	 || (flags & 0x0200 && isinf (a) && a >= 0.0f);
}
