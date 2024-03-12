/* Print wide integers.
   Copyright (C) 2013-2023 Free Software Foundation, Inc.

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

#ifndef WIDE_INT_PRINT_H
#define WIDE_INT_PRINT_H

#include <stdio.h>

#define WIDE_INT_PRINT_BUFFER_SIZE (WIDE_INT_MAX_INL_PRECISION / 4 + 4)

/* Printing functions.  */

extern void print_dec (const wide_int_ref &wi, char *buf, signop sgn);
extern void print_dec (const wide_int_ref &wi, FILE *file, signop sgn);
extern void print_decs (const wide_int_ref &wi, char *buf);
extern void print_decs (const wide_int_ref &wi, FILE *file);
extern void print_decu (const wide_int_ref &wi, char *buf);
extern void print_decu (const wide_int_ref &wi, FILE *file);
extern void print_hex (const wide_int_ref &wi, char *buf);
extern void print_hex (const wide_int_ref &wi, FILE *file);
extern void pp_wide_int_large (pretty_printer *, const wide_int_ref &, signop);

inline bool
print_dec_buf_size (const wide_int_ref &wi, signop sgn, unsigned int *len)
{
  unsigned int l = wi.get_len ();
  if ((l != 1 || sgn == UNSIGNED) && wi::neg_p (wi))
    l = WIDE_INT_MAX_HWIS (wi.get_precision ());
  l = l * HOST_BITS_PER_WIDE_INT / 3 + 3;
  *len = l;
  return UNLIKELY (l > WIDE_INT_PRINT_BUFFER_SIZE);
}

inline bool
print_decs_buf_size (const wide_int_ref &wi, unsigned int *len)
{
  return print_dec_buf_size (wi, SIGNED, len);
}

inline bool
print_decu_buf_size (const wide_int_ref &wi, unsigned int *len)
{
  return print_dec_buf_size (wi, UNSIGNED, len);
}

inline bool
print_hex_buf_size (const wide_int_ref &wi, unsigned int *len)
{
  unsigned int l;
  if (wi::neg_p (wi))
    l = WIDE_INT_MAX_HWIS (wi.get_precision ());
  else
    l = wi.get_len ();
  l = l * HOST_BITS_PER_WIDE_INT / 4 + 4;
  *len = l;
  return UNLIKELY (l > WIDE_INT_PRINT_BUFFER_SIZE);
}

#endif /* WIDE_INT_PRINT_H */
