/* Printing operations with very long integers.
   Copyright (C) 2012-2016 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "wide-int-print.h"

/*
 * public printing routines.
 */

#define BLOCKS_NEEDED(PREC) \
  (((PREC) + HOST_BITS_PER_WIDE_INT - 1) / HOST_BITS_PER_WIDE_INT)

void
print_dec (const wide_int_ref &wi, char *buf, signop sgn)
{
  if (sgn == SIGNED)
    print_decs (wi, buf);
  else
    print_decu (wi, buf);
}

void
print_dec (const wide_int_ref &wi, FILE *file, signop sgn)
{
  if (sgn == SIGNED)
    print_decs (wi, file);
  else
    print_decu (wi, file);
}


/* Try to print the signed self in decimal to BUF if the number fits
   in a HWI.  Other print in hex.  */

void
print_decs (const wide_int_ref &wi, char *buf)
{
  if ((wi.get_precision () <= HOST_BITS_PER_WIDE_INT)
      || (wi.get_len () == 1))
    {
      if (wi::neg_p (wi))
      	sprintf (buf, "-" HOST_WIDE_INT_PRINT_UNSIGNED,
		 -(unsigned HOST_WIDE_INT) wi.to_shwi ());
      else
	sprintf (buf, HOST_WIDE_INT_PRINT_DEC, wi.to_shwi ());
    }
  else
    print_hex (wi, buf);
}

/* Try to print the signed self in decimal to FILE if the number fits
   in a HWI.  Other print in hex.  */

void
print_decs (const wide_int_ref &wi, FILE *file)
{
  char buf[WIDE_INT_PRINT_BUFFER_SIZE];
  print_decs (wi, buf);
  fputs (buf, file);
}

/* Try to print the unsigned self in decimal to BUF if the number fits
   in a HWI.  Other print in hex.  */

void
print_decu (const wide_int_ref &wi, char *buf)
{
  if ((wi.get_precision () <= HOST_BITS_PER_WIDE_INT)
      || (wi.get_len () == 1 && !wi::neg_p (wi)))
    sprintf (buf, HOST_WIDE_INT_PRINT_UNSIGNED, wi.to_uhwi ());
  else
    print_hex (wi, buf);
}

/* Try to print the signed self in decimal to FILE if the number fits
   in a HWI.  Other print in hex.  */

void
print_decu (const wide_int_ref &wi, FILE *file)
{
  char buf[WIDE_INT_PRINT_BUFFER_SIZE];
  print_decu (wi, buf);
  fputs (buf, file);
}

void
print_hex (const wide_int_ref &wi, char *buf)
{
  int i = wi.get_len ();

  if (wi == 0)
    buf += sprintf (buf, "0x0");
  else
    {
      if (wi::neg_p (wi))
	{
	  int j;
	  /* If the number is negative, we may need to pad value with
	     0xFFF...  because the leading elements may be missing and
	     we do not print a '-' with hex.  */
	  buf += sprintf (buf, "0x");
	  for (j = BLOCKS_NEEDED (wi.get_precision ()); j > i; j--)
	    buf += sprintf (buf, HOST_WIDE_INT_PRINT_PADDED_HEX, HOST_WIDE_INT_M1);

	}
      else
	buf += sprintf (buf, "0x" HOST_WIDE_INT_PRINT_HEX_PURE, wi.elt (--i));

      while (--i >= 0)
	buf += sprintf (buf, HOST_WIDE_INT_PRINT_PADDED_HEX, wi.elt (i));
    }
}

/* Print one big hex number to FILE.  Note that some assemblers may not
   accept this for large modes.  */
void
print_hex (const wide_int_ref &wi, FILE *file)
{
  char buf[WIDE_INT_PRINT_BUFFER_SIZE];
  print_hex (wi, buf);
  fputs (buf, file);
}

