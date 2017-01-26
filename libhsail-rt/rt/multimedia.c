/* multimedia.c -- Builtins for HSAIL multimedia instructions.

   Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

#include <math.h>
#include <stdint.h>

uint32_t
__hsail_bitalign (uint64_t lower, uint64_t upper, uint32_t shift_amount)
{
  shift_amount = shift_amount & 31;
  uint64_t packed_value = (upper << 32) | lower;
  return (packed_value >> shift_amount) & 0xFFFFFFFF;
}

uint32_t
__hsail_bytealign (uint64_t lower, uint64_t upper, uint32_t shift_amount)
{
  shift_amount = (shift_amount & 3) * 8;
  uint64_t packed_value = (upper << 32) | lower;
  return (packed_value >> shift_amount) & 0xFFFFFFFF;
}

uint32_t
__hsail_lerp (uint32_t a, uint32_t b, uint32_t c)
{
  uint32_t e3
    = (((((a >> 24) & 0xff) + ((b >> 24) & 0xff) + ((c >> 24) & 0x1)) / 2)
       & 0xff)
      << 24;
  uint32_t e2
    = (((((a >> 16) & 0xff) + ((b >> 16) & 0xff) + ((c >> 16) & 0x1)) / 2)
       & 0xff)
      << 16;
  uint32_t e1
    = (((((a >> 8) & 0xff) + ((b >> 8) & 0xff) + ((c >> 8) & 0x1)) / 2) & 0xff)
      << 8;
  uint32_t e0 = (((a & 0xff) + (b & 0xff) + (c & 0x1)) / 2) & 0xff;

  return e3 | e2 | e1 | e0;
}

static uint8_t
cvt_neari_sat_u8_f32 (float a)
{
  if (isinf (a))
    {
      if (signbit (a)) return 0;
      else return 255;
    }
  else if (isnan (a)) return 0;
  else if (a < 0.0)
    return 0;
  else if (a > 255.0)
    return 255;
  else
    return (uint8_t) a;
}

uint32_t
__hsail_packcvt (float a, float b, float c, float d)
{
  return (uint32_t) cvt_neari_sat_u8_f32 (a)
	 | (uint32_t) cvt_neari_sat_u8_f32 (b) << 8
	 | (uint32_t) cvt_neari_sat_u8_f32 (c) << 16
	 | (uint32_t) cvt_neari_sat_u8_f32 (d) << 24;
}

float
__hsail_unpackcvt (uint32_t val, uint32_t index)
{
  return (float) ((val >> (index * 8)) & 0xff);
}

static uint32_t
abs_diff (uint32_t a, uint32_t b)
{
  if (a < b)
    return b - a;
  else
    return a - b;
}

uint32_t
__hsail_sad_u8x4 (uint32_t a, uint32_t b, uint32_t add)
{
  return abs_diff ((a >> 24) & 0xff, (b >> 24) & 0xff)
	 + abs_diff ((a >> 16) & 0xff, (b >> 16) & 0xff)
	 + abs_diff ((a >> 8) & 0xff, (b >> 8) & 0xff)
	 + abs_diff ((a >> 0) & 0xff, (b >> 0) & 0xff) + add;
}

uint32_t
__hsail_sad_u16x2 (uint32_t a, uint32_t b, uint32_t add)
{
  return abs_diff ((a >> 16) & 0xffff, (b >> 16) & 0xffff)
	 + abs_diff ((a >> 0) & 0xffff, (b >> 0) & 0xffff) + add;
}

uint32_t
__hsail_sad_u32 (uint32_t a, uint32_t b, uint32_t add)
{
  return abs_diff (a, b) + add;
}

uint32_t
__hsail_sadhi_u16x2_u8x4 (uint32_t a, uint32_t b, uint32_t add)
{
  return (abs_diff ((a >> 24) & 0xff, (b >> 24) & 0xff) << 16)
	 + (abs_diff ((a >> 16) & 0xff, (b >> 16) & 0xff) << 16)
	 + (abs_diff ((a >> 8) & 0xff, (b >> 8) & 0xff) << 16)
	 + (abs_diff ((a >> 0) & 0xff, (b >> 0) & 0xff) << 16) + add;
}
