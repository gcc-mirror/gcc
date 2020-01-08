/* sat_arithmetic.c -- Builtins for HSAIL saturating arithmetic instructions.

   Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

#include <stdint.h>

uint8_t
__hsail_sat_add_u8 (uint8_t a, uint8_t b)
{
  uint16_t c = (uint16_t) a + (uint16_t) b;
  if (c > UINT8_MAX)
    return UINT8_MAX;
  else
    return c;
}

uint16_t
__hsail_sat_add_u16 (uint16_t a, uint16_t b)
{
  uint32_t c = (uint32_t) a + (uint32_t) b;
  if (c > UINT16_MAX)
    return UINT16_MAX;
  else
    return c;
}

uint32_t
__hsail_sat_add_u32 (uint32_t a, uint32_t b)
{
  uint32_t c;
  if (__builtin_add_overflow (a, b, &c))
    return UINT32_MAX;
  return c;
}

uint64_t
__hsail_sat_add_u64 (uint64_t a, uint64_t b)
{
  uint64_t c;
  if (__builtin_add_overflow (a, b, &c))
    return UINT64_MAX;
  return c;
}

int8_t
__hsail_sat_add_s8 (int8_t a, int8_t b)
{
  int16_t c = (int16_t) a + (int16_t) b;
  if (c > INT8_MAX)
    return INT8_MAX;
  else if (c < INT8_MIN)
    return INT8_MIN;
  else
    return c;
}

int16_t
__hsail_sat_add_s16 (int16_t a, int16_t b)
{
  int32_t c = (int32_t) a + (int32_t) b;
  if (c > INT16_MAX)
    return INT16_MAX;
  else if (c < INT16_MIN)
    return INT16_MIN;
  else
    return c;
}

int32_t
__hsail_sat_add_s32 (int32_t a, int32_t b)
{
  int32_t c;
  if (__builtin_add_overflow (a, b, &c))
    return b < 0 ? INT32_MIN : INT32_MAX;
  return c;
}

int64_t
__hsail_sat_add_s64 (int64_t a, int64_t b)
{
  int64_t c;
  if (__builtin_add_overflow (a, b, &c))
    return b < 0 ? INT64_MIN : INT64_MAX;
  return c;
}

uint8_t
__hsail_sat_sub_u8 (uint8_t a, uint8_t b)
{
  int16_t c = (uint16_t) a - (uint16_t) b;
  if (c < 0)
    return 0;
  else
    return c;
}

uint16_t
__hsail_sat_sub_u16 (uint16_t a, uint16_t b)
{
  int32_t c = (uint32_t) a - (uint32_t) b;
  if (c < 0)
    return 0;
  else
    return c;
}

uint32_t
__hsail_sat_sub_u32 (uint32_t a, uint32_t b)
{
  uint32_t c;
  if (__builtin_sub_overflow (a, b, &c))
    return 0;
  return c;
}

uint64_t
__hsail_sat_sub_u64 (uint64_t a, uint64_t b)
{
  uint64_t c;
  if (__builtin_sub_overflow (a, b, &c))
    return 0;
  return c;
}

int8_t
__hsail_sat_sub_s8 (int8_t a, int8_t b)
{
  int16_t c = (int16_t) a - (int16_t) b;
  if (c > INT8_MAX)
    return INT8_MAX;
  else if (c < INT8_MIN)
    return INT8_MIN;
  else
    return c;
}

int16_t
__hsail_sat_sub_s16 (int16_t a, int16_t b)
{
  int32_t c = (int32_t) a - (int32_t) b;
  if (c > INT16_MAX)
    return INT16_MAX;
  else if (c < INT16_MIN)
    return INT16_MIN;
  else
    return c;
}

int32_t
__hsail_sat_sub_s32 (int32_t a, int32_t b)
{
  int32_t c;
  if (__builtin_sub_overflow (a, b, &c))
    return b < 0 ? INT32_MAX : INT32_MIN;
  return c;
}

int64_t
__hsail_sat_sub_s64 (int64_t a, int64_t b)
{
  int64_t c;
  if (__builtin_sub_overflow (a, b, &c))
    return b < 0 ? INT64_MAX : INT64_MIN;
  return c;
}

uint8_t
__hsail_sat_mul_u8 (uint8_t a, uint8_t b)
{
  uint16_t c = (uint16_t) a * (uint16_t) b;
  if (c > UINT8_MAX)
    return UINT8_MAX;
  else
    return c;
}

uint16_t
__hsail_sat_mul_u16 (uint16_t a, uint16_t b)
{
  uint32_t c = (uint32_t) a * (uint32_t) b;
  if (c > UINT16_MAX)
    return UINT16_MAX;
  else
    return c;
}

uint32_t
__hsail_sat_mul_u32 (uint32_t a, uint32_t b)
{
  uint32_t c;
  if (__builtin_mul_overflow (a, b, &c))
    return UINT32_MAX;
  return c;
}

uint64_t
__hsail_sat_mul_u64 (uint64_t a, uint64_t b)
{
  uint64_t c;
  if (__builtin_mul_overflow (a, b, &c))
    return UINT64_MAX;
  return c;
}

int8_t
__hsail_sat_mul_s8 (int8_t a, int8_t b)
{
  int16_t c = (int16_t) a * (int16_t) b;
  if (c > INT8_MAX)
    return INT8_MAX;
  else if (c < INT8_MIN)
    return INT8_MIN;
  else
    return c;
}

int16_t
__hsail_sat_mul_s16 (int16_t a, int16_t b)
{
  int32_t c = (int32_t) a * (int32_t) b;
  if (c > INT16_MAX)
    return INT16_MAX;
  else if (c < INT16_MIN)
    return INT16_MIN;
  else
    return c;
}

int32_t
__hsail_sat_mul_s32 (int32_t a, int32_t b)
{
  int32_t c;
  if (__builtin_mul_overflow (a, b, &c))
    return ((a > 0) ^ (b > 0)) ? INT32_MIN : INT32_MAX;
  return c;
}

int64_t
__hsail_sat_mul_s64 (int64_t a, int64_t b)
{
  int64_t c;
  if (__builtin_mul_overflow (a, b, &c))
    return ((a > 0) ^ (b > 0)) ? INT64_MIN : INT64_MAX;
  return c;
}
