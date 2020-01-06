/* bitstring.c -- Builtins for HSAIL bitstring instructions.

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
#include <limits.h>

#define BITEXTRACT(DEST_TYPE, SRC0, SRC1, SRC2)				\
  uint32_t offset = SRC1 & (sizeof (DEST_TYPE) * 8 - 1);		\
  uint32_t width = SRC2 & (sizeof (DEST_TYPE) * 8 - 1);			\
  if (width == 0)							\
    return 0;								\
  else									\
    return (SRC0 << (sizeof (DEST_TYPE) * 8 - width - offset))		\
      >> (sizeof (DEST_TYPE) * 8 - width)

uint32_t
__hsail_bitextract_u32 (uint32_t src0, uint32_t src1, uint32_t src2)
{
  BITEXTRACT (uint32_t, src0, src1, src2);
}

int32_t
__hsail_bitextract_s32 (int32_t src0, uint32_t src1, uint32_t src2)
{
  BITEXTRACT (int32_t, src0, src1, src2);
}

uint64_t
__hsail_bitextract_u64 (uint64_t src0, uint32_t src1, uint32_t src2)
{
  BITEXTRACT (uint64_t, src0, src1, src2);
}

int64_t
__hsail_bitextract_s64 (int64_t src0, uint32_t src1, uint32_t src2)
{
  BITEXTRACT (int64_t, src0, src1, src2);
}

#define BITINSERT(DEST_TYPE, SRC0, SRC1, SRC2, SRC3)			\
  uint32_t offset = SRC2 & (sizeof (DEST_TYPE) * 8 - 1);		\
  uint32_t width = SRC3 & (sizeof (DEST_TYPE) * 8 - 1);			\
  DEST_TYPE mask = ((DEST_TYPE) 1 << width) - 1;			\
  return (SRC0 & ~(mask << offset)) | ((SRC1 & mask) << offset)

uint32_t
__hsail_bitinsert_u32 (uint32_t src0, uint32_t src1, uint32_t src2,
			      uint32_t src3)
{
  BITINSERT (uint32_t, src0, src1, src2, src3);
}

int64_t
__hsail_bitinsert_u64 (uint64_t src0, uint64_t src1, uint32_t src2,
			      uint32_t src3)
{
  BITINSERT (uint64_t, src0, src1, src2, src3);
}

#define BITMASK(DEST_TYPE, SRC0, SRC1)					\
  uint32_t offset = SRC0 & (sizeof (DEST_TYPE) * 8 - 1);		\
  uint32_t width = SRC1 & (sizeof (DEST_TYPE) * 8 - 1);			\
  DEST_TYPE mask = ((DEST_TYPE) 1 << width) - 1;			\
  return mask << offset

uint32_t
__hsail_bitmask_u32 (uint32_t src0, uint32_t src1)
{
  BITMASK (uint32_t, src0, src1);
}

uint64_t
__hsail_bitmask_u64 (uint32_t src0, uint32_t src1)
{
  BITMASK (uint64_t, src0, src1);
}

/* The dummy, but readable version from
   http://graphics.stanford.edu/~seander/bithacks.html#BitReverseObvious
   This (also) often maps to a single instruction in DSPs.  */

#define BITREV(DEST_TYPE, SRC)						\
  DEST_TYPE v = SRC;							\
  DEST_TYPE r = v;							\
  int s = sizeof (SRC) * CHAR_BIT - 1;					\
  									\
  for (v >>= 1; v; v >>= 1)						\
    {									\
      r <<= 1;								\
      r |= v & 1;							\
      s--;								\
    }									\
  return r << s

uint32_t
__hsail_bitrev_u32 (uint32_t src0)
{
  BITREV (uint32_t, src0);
}

uint64_t
__hsail_bitrev_u64 (uint64_t src0)
{
  BITREV (uint64_t, src0);
}

uint32_t
__hsail_bitselect_u32 (uint32_t src0, uint32_t src1, uint32_t src2)
{
  return (src1 & src0) | (src2 & ~src0);
}

uint64_t
__hsail_bitselect_u64 (uint64_t src0, uint64_t src1, uint64_t src2)
{
  return (src1 & src0) | (src2 & ~src0);
}

/* Due to the defined behavior with 0, we cannot use the gcc builtin
   __builtin_clz* () directly. __builtin_ffs () has defined behavior, but
   returns 0 while HSAIL requires to return -1.  */

uint32_t
__hsail_firstbit_u32 (uint32_t src0)
{
  if (src0 == 0)
    return -1;
  return __builtin_clz (src0);
}

uint32_t
__hsail_firstbit_s32 (int32_t src0)
{
  uint32_t converted = src0 >= 0 ? src0 : ~src0;
  return __hsail_firstbit_u32 (converted);
}

uint32_t
__hsail_firstbit_u64 (uint64_t src0)
{
  if (src0 == 0)
    return -1;
  return __builtin_clzl (src0);
}

uint32_t
__hsail_firstbit_s64 (int64_t src0)
{
  uint64_t converted = src0 >= 0 ? src0 : ~src0;
  return __hsail_firstbit_u64 (converted);
}

uint32_t
__hsail_lastbit_u32 (uint32_t src0)
{
  if (src0 == 0)
    return -1;
  return __builtin_ctz (src0);
}

uint32_t
__hsail_lastbit_u64 (uint64_t src0)
{
  if (src0 == 0)
    return -1;
  return __builtin_ctzl (src0);
}
