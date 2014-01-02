/*

picoChip GCC support for 32-bit shift left.

Copyright (C) 2003-2014 Free Software Foundation, Inc.
Contributed by Picochip Ltd.
Maintained by Daniel Towner (daniel.towner@picochip.com)

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef PICOCHIP
#error "Intended for compilation for PICOCHIP only."
#endif

typedef int HItype __attribute__ ((mode (HI)));
typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));

typedef struct USIstruct {
  UHItype low, high;
} USIstruct;

typedef union USIunion {
  USItype l;
  USIstruct s;
} USIunion;

USItype __ashlsi3(USIunion value, HItype count) {
  USIunion result;
  int temp;

  /* Ignore a zero count until we get into the (count < 16)
     clause. This is slightly slower when shifting by zero, but faster
     and smaller in all other cases (due to the better scheduling
     opportunities available by putting the test near computational
     instructions. */
  /* if (count == 0) return value.l; */

  if (count < 16) {
    /* Shift low and high words by the count. */
    result.s.low = value.s.low << count;
    result.s.high = value.s.high << count;
     
    /* There is now a hole in the lower `count' bits of the high
       word. Shift the upper `count' bits of the low word into the
       high word. This is only required when the count is non-zero. */
    if (count != 0) {
      temp = 16 - count;
      temp = value.s.low >> temp;
      result.s.high |= temp;
    }
  
  } else {
    /* Shift the lower word of the source into the upper word of the
       result, and zero the result's lower word. */
    count -= 16;
    result.s.high = value.s.low << count;
    result.s.low = 0;

  }

  return result.l;

}

