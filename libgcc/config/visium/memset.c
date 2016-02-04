/* Copyright (C) 2012-2016 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file must be kept in sync with newlib/libc/machine/visium/memset.c  */

#include <stddef.h>
#include "memset.h"

#define SET_32_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out [6] = m0;			\
  out [7] = m0;			\
  out [8] = m0;			\
  out [9] = m0;			\
  out [10] = m0;		\
  out [11] = m0;		\
  out [12] = m0;		\
  out [13] = m0;		\
  out [14] = m0;		\
  out [15] = m0;		\
  out [16] = m0;		\
  out [17] = m0;		\
  out [18] = m0;		\
  out [19] = m0;		\
  out [20] = m0;		\
  out [21] = m0;		\
  out [22] = m0;		\
  out [23] = m0;		\
  out [24] = m0;		\
  out [25] = m0;		\
  out [26] = m0;		\
  out [27] = m0;		\
  out [28] = m0;		\
  out [29] = m0;		\
  out [30] = m0;		\
  out [31] = m0;		\
  out += 32;			\
} while(0)

#define SET_16_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out [6] = m0;			\
  out [7] = m0;			\
  out [8] = m0;			\
  out [9] = m0;			\
  out [10] = m0;		\
  out [11] = m0;		\
  out [12] = m0;		\
  out [13] = m0;		\
  out [14] = m0;		\
  out [15] = m0;		\
  out += 16;			\
} while(0)

#define SET_12_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out [6] = m0;			\
  out [7] = m0;			\
  out [8] = m0;			\
  out [9] = m0;			\
  out [10] = m0;		\
  out [11] = m0;		\
  out += 12;			\
} while(0)

#define SET_11_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out [6] = m0;			\
  out [7] = m0;			\
  out [8] = m0;			\
  out [9] = m0;			\
  out [10] = m0;		\
  out += 11;			\
} while(0)

#define SET_10_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out [6] = m0;			\
  out [7] = m0;			\
  out [8] = m0;			\
  out [9] = m0;			\
  out += 10;			\
} while(0)

#define SET_9_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out [6] = m0;			\
  out [7] = m0;			\
  out [8] = m0;			\
  out += 9;			\
} while(0)

#define SET_8_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out [6] = m0;			\
  out [7] = m0;			\
  out += 8;			\
} while(0)

#define SET_7_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out [6] = m0;			\
  out += 7;			\
} while(0)

#define SET_6_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out [5] = m0;			\
  out += 6;			\
} while(0)

#define SET_5_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out [4] = m0;			\
  out += 5;			\
} while(0)

#define SET_4_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out [3] = m0;			\
  out += 4;			\
} while(0)

#define SET_3_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out [2] = m0;			\
  out += 3;			\
} while(0)

#define SET_2_OBJECTS(out)	\
do {				\
  out [0] = m0;			\
  out [1] = m0;			\
  out += 2;			\
} while(0)

#define SET_1_OBJECT(out)	\
do {				\
  out [0] = m0;			\
  out += 1;			\
} while(0)


static inline void
__int_memset (void *__restrict s1, int val, size_t n)
{
  int value = n;
  int loop_var;
  int *out = s1;
  int count;
  int m0 = val;

  /* This code currently give a stall for any value with a 1->2 in the low 5
     bits, i.e.  1,2, 33,34 ? not acceptable!  */
  switch (value & 0x1f)
    {
    case 0:
      break;
    case 1:
      SET_1_OBJECT (out);
      break;
    case 2:
      SET_2_OBJECTS (out);
      break;
    case 3:
      SET_3_OBJECTS (out);
      break;
    case 4:
      SET_4_OBJECTS (out);
      break;
    case 5:
      SET_5_OBJECTS (out);
      break;
    case 6:
      SET_6_OBJECTS (out);
      break;
    case 7:
      SET_7_OBJECTS (out);
      break;
    case 8:
      SET_8_OBJECTS (out);
      break;
    case 9:
      SET_9_OBJECTS (out);
      break;
    case 10:
      SET_10_OBJECTS (out);
      break;
    case 11:
      SET_11_OBJECTS (out);
      break;
    case 12:
      SET_12_OBJECTS (out);
      break;
    case 13:
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 14:
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case 15:
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 16:
      SET_16_OBJECTS (out);
      break;
    case 17:
      SET_11_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case 18:
      SET_9_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case 19:
      SET_16_OBJECTS (out);
      SET_3_OBJECTS (out);
      break;
    case 20:
      SET_16_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 21:
      SET_16_OBJECTS (out);
      SET_5_OBJECTS (out);
      break;
    case 22:
      SET_16_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case 23:
      SET_16_OBJECTS (out);
      SET_7_OBJECTS (out);
      break;
    case 24:
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      break;
    case 25:
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case 26:
      SET_16_OBJECTS (out);
      SET_10_OBJECTS (out);
      break;
    case 27:
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      break;
    case 28:
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 29:
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 30:
      SET_16_OBJECTS (out);
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case 31:
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    }

  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word copies.  */
  count = value >> 5;
  for (loop_var = 0; loop_var < count; loop_var++)
    SET_32_OBJECTS (out);
}

static inline void
__short_int_memset (void *__restrict s1, int val, size_t n)
{
  int value = n;
  int loop_var;
  int short *out = s1;
  int count;
  int m0 = val;

  /* This code currently give a stall for any value with a 1->2 in the low 5
     bits, i.e.  1,2, 33,34 ? not acceptable!  */
  switch (value & 0x1f)
    {
    case 0:
      break;
    case 1:
      SET_1_OBJECT (out);
      break;
    case 2:
      SET_2_OBJECTS (out);
      break;
    case 3:
      SET_3_OBJECTS (out);
      break;
    case 4:
      SET_4_OBJECTS (out);
      break;
    case 5:
      SET_5_OBJECTS (out);
      break;
    case 6:
      SET_6_OBJECTS (out);
      break;
    case 7:
      SET_7_OBJECTS (out);
      break;
    case 8:
      SET_8_OBJECTS (out);
      break;
    case 9:
      SET_9_OBJECTS (out);
      break;
    case 10:
      SET_10_OBJECTS (out);
      break;
    case 11:
      SET_11_OBJECTS (out);
      break;
    case 12:
      SET_12_OBJECTS (out);
      break;
    case 13:
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 14:
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case 15:
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 16:
      SET_16_OBJECTS (out);
      break;
    case 17:
      SET_11_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case 18:
      SET_9_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case 19:
      SET_16_OBJECTS (out);
      SET_3_OBJECTS (out);
      break;
    case 20:
      SET_16_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 21:
      SET_16_OBJECTS (out);
      SET_5_OBJECTS (out);
      break;
    case 22:
      SET_16_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case 23:
      SET_16_OBJECTS (out);
      SET_7_OBJECTS (out);
      break;
    case 24:
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      break;
    case 25:
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case 26:
      SET_16_OBJECTS (out);
      SET_10_OBJECTS (out);
      break;
    case 27:
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      break;
    case 28:
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 29:
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 30:
      SET_16_OBJECTS (out);
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case 31:
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    }

  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word copies.  */
  count = value >> 5;
  for (loop_var = 0; loop_var < count; loop_var++)
    SET_32_OBJECTS (out);
}

static inline void
__byte_memset (void *__restrict s1, int val, size_t n)
{
  int value = n;
  int loop_var;
  char *out = s1;
  int count;
  int m0 = val;

  /* This code currently give a stall for any value with a 1->2 in the low 5
     bits, i.e.  1,2, 33,34 ? not acceptable!  */
  switch (value & 0x1f)
    {
    case 0:
      break;
    case 1:
      SET_1_OBJECT (out);
      break;
    case 2:
      SET_2_OBJECTS (out);
      break;
    case 3:
      SET_3_OBJECTS (out);
      break;
    case 4:
      SET_4_OBJECTS (out);
      break;
    case 5:
      SET_5_OBJECTS (out);
      break;
    case 6:
      SET_6_OBJECTS (out);
      break;
    case 7:
      SET_7_OBJECTS (out);
      break;
    case 8:
      SET_8_OBJECTS (out);
      break;
    case 9:
      SET_9_OBJECTS (out);
      break;
    case 10:
      SET_10_OBJECTS (out);
      break;
    case 11:
      SET_11_OBJECTS (out);
      break;
    case 12:
      SET_12_OBJECTS (out);
      break;
    case 13:
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 14:
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case 15:
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 16:
      SET_16_OBJECTS (out);
      break;
    case 17:
      SET_11_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case 18:
      SET_9_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case 19:
      SET_16_OBJECTS (out);
      SET_3_OBJECTS (out);
      break;
    case 20:
      SET_16_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 21:
      SET_16_OBJECTS (out);
      SET_5_OBJECTS (out);
      break;
    case 22:
      SET_16_OBJECTS (out);
      SET_6_OBJECTS (out);
      break;
    case 23:
      SET_16_OBJECTS (out);
      SET_7_OBJECTS (out);
      break;
    case 24:
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      break;
    case 25:
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      break;
    case 26:
      SET_16_OBJECTS (out);
      SET_10_OBJECTS (out);
      break;
    case 27:
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      break;
    case 28:
      SET_16_OBJECTS (out);
      SET_8_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 29:
      SET_16_OBJECTS (out);
      SET_9_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    case 30:
      SET_16_OBJECTS (out);
      SET_12_OBJECTS (out);
      SET_2_OBJECTS (out);
      break;
    case 31:
      SET_16_OBJECTS (out);
      SET_11_OBJECTS (out);
      SET_4_OBJECTS (out);
      break;
    }

  /* This loop governs the asmptoptic behaviour of this algorithm, for long
     word copies.  */
  count = value >> 5;
  for (loop_var = 0; loop_var < count; loop_var++)
    SET_32_OBJECTS (out);
}


/* Exposed interface.  */

void
__long_int_memset (void *__restrict s, int c, size_t n)
{
  int ic = (c << 24) + ((char) c << 16) + ((char) c << 8) + (char) c;
  __int_memset (s, ic, n);
}

void
__wrd_memset (void *__restrict s, int c, size_t n)
{
  int sc = ((c << 8) + (char) c);
  __short_int_memset (s, sc, n);
}

void
__byt_memset (void *__restrict s, int c, size_t n)
{
  __byte_memset (s, c, n);
}
