/* Printing operations with very long integers.
   Copyright (C) 2012-2024 Free Software Foundation, Inc.
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
#include "pretty-print.h"

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


/* Try to print the signed self in decimal to BUF.  */

void
print_decs (const wide_int_ref &wi, char *buf)
{
  if (wi.get_precision () <= HOST_BITS_PER_WIDE_INT || wi.get_len () == 1)
    {
      if (wi::neg_p (wi))
      	sprintf (buf, "-" HOST_WIDE_INT_PRINT_UNSIGNED,
		 -(unsigned HOST_WIDE_INT) wi.to_shwi ());
      else
	sprintf (buf, HOST_WIDE_INT_PRINT_DEC, wi.to_shwi ());
    }
  else if (wi::neg_p (wi))
    {
      widest2_int w = widest2_int::from (wi, SIGNED);
      *buf = '-';
      print_decu (-w, buf + 1);
    }
  else
    print_decu (wi, buf);
}

/* Try to print the signed self in decimal to FILE.  */

void
print_decs (const wide_int_ref &wi, FILE *file)
{
  char buf[WIDE_INT_PRINT_BUFFER_SIZE], *p = buf;
  unsigned len;
  if (print_decs_buf_size (wi, &len))
    p = XALLOCAVEC (char, len);
  print_decs (wi, p);
  fputs (p, file);
}

/* Try to print the unsigned self in decimal to BUF.  */

void
print_decu (const wide_int_ref &wi, char *buf)
{
  if ((wi.get_precision () <= HOST_BITS_PER_WIDE_INT)
      || (wi.get_len () == 1 && !wi::neg_p (wi)))
    sprintf (buf, HOST_WIDE_INT_PRINT_UNSIGNED, wi.to_uhwi ());
  else
    {
      widest2_int w = widest2_int::from (wi, UNSIGNED), r;
      widest2_int ten19 = HOST_WIDE_INT_UC (10000000000000000000);
      char buf2[20], next1[19], next2[19];
      size_t l, c = 0, i;
      /* In order to avoid dividing this twice, print the 19 decimal
	 digit chunks in reverse order into buffer and then reorder
	 them in-place.  */
      while (wi::gtu_p (w, ten19))
	{
	  w = wi::divmod_trunc (w, ten19, UNSIGNED, &r);
	  sprintf (buf + c * 19, "%019" PRIu64, r.to_uhwi ());
	  ++c;
	}
      l = sprintf (buf2, HOST_WIDE_INT_PRINT_UNSIGNED, w.to_uhwi ());
      buf[c * 19 + l] = '\0';
      memcpy (next1, buf, 19);
      memcpy (buf, buf2, l);
      for (i = 0; i < c / 2; ++i)
	{
	  memcpy (next2, buf + (c - i - 1) * 19, 19);
	  memcpy (buf + l + (c - i - 1) * 19, next1, 19);
	  memcpy (next1, buf + (i + 1) * 19, 19);
	  memcpy (buf + l + i * 19, next2, 19);
	}
      if (c & 1)
	memcpy (buf + l + i * 19, next1, 19);
    }
}

/* Try to print the signed self in decimal to FILE.  */

void
print_decu (const wide_int_ref &wi, FILE *file)
{
  char buf[WIDE_INT_PRINT_BUFFER_SIZE], *p = buf;
  unsigned len;
  if (print_decu_buf_size (wi, &len))
    p = XALLOCAVEC (char, len);
  print_decu (wi, p);
  fputs (p, file);
}

void
print_hex (const wide_int_ref &val, char *buf)
{
  if (val == 0)
    buf += sprintf (buf, "0x0");
  else
    {
      buf += sprintf (buf, "0x");
      int start = ROUND_DOWN (val.get_precision (), HOST_BITS_PER_WIDE_INT);
      int width = val.get_precision () - start;
      bool first_p = true;
      for (int i = start; i >= 0; i -= HOST_BITS_PER_WIDE_INT)
	{
	  unsigned HOST_WIDE_INT uhwi = wi::extract_uhwi (val, i, width);
	  if (!first_p)
	    buf += sprintf (buf, HOST_WIDE_INT_PRINT_PADDED_HEX, uhwi);
	  else if (uhwi != 0)
	    {
	      buf += sprintf (buf, HOST_WIDE_INT_PRINT_HEX_PURE, uhwi);
	      first_p = false;
	    }
	  width = HOST_BITS_PER_WIDE_INT;
	}
    }
}

/* Print one big hex number to FILE.  Note that some assemblers may not
   accept this for large modes.  */
void
print_hex (const wide_int_ref &wi, FILE *file)
{
  char buf[WIDE_INT_PRINT_BUFFER_SIZE], *p = buf;
  unsigned len;
  if (print_hex_buf_size (wi, &len))
    p = XALLOCAVEC (char, len);
  print_hex (wi, p);
  fputs (p, file);
}

/* Print larger precision wide_int.  Not defined as inline in a header
   together with pp_wide_int because XALLOCAVEC will make it uninlinable.  */

void
pp_wide_int_large (pretty_printer *pp, const wide_int_ref &w, signop sgn)
{
  unsigned int len;
  print_dec_buf_size (w, sgn, &len);
  char *buf = XALLOCAVEC (char, len);
  print_dec (w, buf, sgn);
  pp_string (pp, buf);
}
