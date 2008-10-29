/* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Andy Vaught
   Namelist output contributed by Paul Thomas

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "io.h"
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#define star_fill(p, n) memset(p, '*', n)

#include "write_float.def"

void
write_a (st_parameter_dt *dtp, const fnode *f, const char *source, int len)
{
  int wlen;
  char *p;

  wlen = f->u.string.length < 0 ? len : f->u.string.length;

#ifdef HAVE_CRLF
  /* If this is formatted STREAM IO convert any embedded line feed characters
     to CR_LF on systems that use that sequence for newlines.  See F2003
     Standard sections 10.6.3 and 9.9 for further information.  */
  if (is_stream_io (dtp))
    {
      const char crlf[] = "\r\n";
      int i, q, bytes;
      q = bytes = 0;

      /* Write out any padding if needed.  */
      if (len < wlen)
	{
	  p = write_block (dtp, wlen - len);
	  if (p == NULL)
	    return;
	  memset (p, ' ', wlen - len);
	}

      /* Scan the source string looking for '\n' and convert it if found.  */
      for (i = 0; i < wlen; i++)
	{
	  if (source[i] == '\n')
	    {
	      /* Write out the previously scanned characters in the string.  */
	      if (bytes > 0)
		{
		  p = write_block (dtp, bytes);
		  if (p == NULL)
		    return;
		  memcpy (p, &source[q], bytes);
		  q += bytes;
		  bytes = 0;
		}

	      /* Write out the CR_LF sequence.  */ 
	      q++;
	      p = write_block (dtp, 2);
              if (p == NULL)
                return;
	      memcpy (p, crlf, 2);
	    }
	  else
	    bytes++;
	}

      /*  Write out any remaining bytes if no LF was found.  */
      if (bytes > 0)
	{
	  p = write_block (dtp, bytes);
	  if (p == NULL)
	    return;
	  memcpy (p, &source[q], bytes);
	}
    }
  else
    {
#endif
      p = write_block (dtp, wlen);
      if (p == NULL)
	return;

      if (wlen < len)
	memcpy (p, source, wlen);
      else
	{
	  memset (p, ' ', wlen - len);
	  memcpy (p + wlen - len, source, len);
	}
#ifdef HAVE_CRLF
    }
#endif
}

static GFC_INTEGER_LARGEST
extract_int (const void *p, int len)
{
  GFC_INTEGER_LARGEST i = 0;

  if (p == NULL)
    return i;

  switch (len)
    {
    case 1:
      {
	GFC_INTEGER_1 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
    case 2:
      {
	GFC_INTEGER_2 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
    case 4:
      {
	GFC_INTEGER_4 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
    case 8:
      {
	GFC_INTEGER_8 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
#ifdef HAVE_GFC_INTEGER_16
    case 16:
      {
	GFC_INTEGER_16 tmp;
	memcpy ((void *) &tmp, p, len);
	i = tmp;
      }
      break;
#endif
    default:
      internal_error (NULL, "bad integer kind");
    }

  return i;
}

static GFC_UINTEGER_LARGEST
extract_uint (const void *p, int len)
{
  GFC_UINTEGER_LARGEST i = 0;

  if (p == NULL)
    return i;

  switch (len)
    {
    case 1:
      {
	GFC_INTEGER_1 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_1) tmp;
      }
      break;
    case 2:
      {
	GFC_INTEGER_2 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_2) tmp;
      }
      break;
    case 4:
      {
	GFC_INTEGER_4 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_4) tmp;
      }
      break;
    case 8:
      {
	GFC_INTEGER_8 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_8) tmp;
      }
      break;
#ifdef HAVE_GFC_INTEGER_16
    case 16:
      {
	GFC_INTEGER_16 tmp;
	memcpy ((void *) &tmp, p, len);
	i = (GFC_UINTEGER_16) tmp;
      }
      break;
#endif
    default:
      internal_error (NULL, "bad integer kind");
    }

  return i;
}


void
write_l (st_parameter_dt *dtp, const fnode *f, char *source, int len)
{
  char *p;
  GFC_INTEGER_LARGEST n;

  p = write_block (dtp, f->u.w);
  if (p == NULL)
    return;

  memset (p, ' ', f->u.w - 1);
  n = extract_int (source, len);
  p[f->u.w - 1] = (n) ? 'T' : 'F';
}


static void
write_int (st_parameter_dt *dtp, const fnode *f, const char *source, int len,
           const char *(*conv) (GFC_UINTEGER_LARGEST, char *, size_t))
{
  GFC_UINTEGER_LARGEST n = 0;
  int w, m, digits, nzero, nblank;
  char *p;
  const char *q;
  char itoa_buf[GFC_BTOA_BUF_SIZE];

  w = f->u.integer.w;
  m = f->u.integer.m;

  n = extract_uint (source, len);

  /* Special case:  */

  if (m == 0 && n == 0)
    {
      if (w == 0)
        w = 1;

      p = write_block (dtp, w);
      if (p == NULL)
        return;

      memset (p, ' ', w);
      goto done;
    }

  q = conv (n, itoa_buf, sizeof (itoa_buf));
  digits = strlen (q);

  /* Select a width if none was specified.  The idea here is to always
     print something.  */

  if (w == 0)
    w = ((digits < m) ? m : digits);

  p = write_block (dtp, w);
  if (p == NULL)
    return;

  nzero = 0;
  if (digits < m)
    nzero = m - digits;

  /* See if things will work.  */

  nblank = w - (nzero + digits);

  if (nblank < 0)
    {
      star_fill (p, w);
      goto done;
    }


  if (!dtp->u.p.no_leading_blank)
    {
      memset (p, ' ', nblank);
      p += nblank;
      memset (p, '0', nzero);
      p += nzero;
      memcpy (p, q, digits);
    }
  else
    {
      memset (p, '0', nzero);
      p += nzero;
      memcpy (p, q, digits);
      p += digits;
      memset (p, ' ', nblank);
      dtp->u.p.no_leading_blank = 0;
    }

 done:
  return;
}

static void
write_decimal (st_parameter_dt *dtp, const fnode *f, const char *source,
	       int len,
               const char *(*conv) (GFC_INTEGER_LARGEST, char *, size_t))
{
  GFC_INTEGER_LARGEST n = 0;
  int w, m, digits, nsign, nzero, nblank;
  char *p;
  const char *q;
  sign_t sign;
  char itoa_buf[GFC_BTOA_BUF_SIZE];

  w = f->u.integer.w;
  m = f->u.integer.m;

  n = extract_int (source, len);

  /* Special case:  */

  if (m == 0 && n == 0)
    {
      if (w == 0)
        w = 1;

      p = write_block (dtp, w);
      if (p == NULL)
        return;

      memset (p, ' ', w);
      goto done;
    }

  sign = calculate_sign (dtp, n < 0);
  if (n < 0)
    n = -n;

  nsign = sign == SIGN_NONE ? 0 : 1;
  q = conv (n, itoa_buf, sizeof (itoa_buf));

  digits = strlen (q);

  /* Select a width if none was specified.  The idea here is to always
     print something.  */

  if (w == 0)
    w = ((digits < m) ? m : digits) + nsign;

  p = write_block (dtp, w);
  if (p == NULL)
    return;

  nzero = 0;
  if (digits < m)
    nzero = m - digits;

  /* See if things will work.  */

  nblank = w - (nsign + nzero + digits);

  if (nblank < 0)
    {
      star_fill (p, w);
      goto done;
    }

  memset (p, ' ', nblank);
  p += nblank;

  switch (sign)
    {
    case SIGN_PLUS:
      *p++ = '+';
      break;
    case SIGN_MINUS:
      *p++ = '-';
      break;
    case SIGN_NONE:
      break;
    }

  memset (p, '0', nzero);
  p += nzero;

  memcpy (p, q, digits);

 done:
  return;
}


/* Convert unsigned octal to ascii.  */

static const char *
otoa (GFC_UINTEGER_LARGEST n, char *buffer, size_t len)
{
  char *p;

  assert (len >= GFC_OTOA_BUF_SIZE);

  if (n == 0)
    return "0";

  p = buffer + GFC_OTOA_BUF_SIZE - 1;
  *p = '\0';

  while (n != 0)
    {
      *--p = '0' + (n & 7);
      n >>= 3;
    }

  return p;
}


/* Convert unsigned binary to ascii.  */

static const char *
btoa (GFC_UINTEGER_LARGEST n, char *buffer, size_t len)
{
  char *p;

  assert (len >= GFC_BTOA_BUF_SIZE);

  if (n == 0)
    return "0";

  p = buffer + GFC_BTOA_BUF_SIZE - 1;
  *p = '\0';

  while (n != 0)
    {
      *--p = '0' + (n & 1);
      n >>= 1;
    }

  return p;
}


void
write_i (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_decimal (dtp, f, p, len, (void *) gfc_itoa);
}


void
write_b (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_int (dtp, f, p, len, btoa);
}


void
write_o (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_int (dtp, f, p, len, otoa);
}

void
write_z (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_int (dtp, f, p, len, xtoa);
}


void
write_d (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


void
write_e (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


void
write_f (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


void
write_en (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


void
write_es (st_parameter_dt *dtp, const fnode *f, const char *p, int len)
{
  write_float (dtp, f, p, len);
}


/* Take care of the X/TR descriptor.  */

void
write_x (st_parameter_dt *dtp, int len, int nspaces)
{
  char *p;

  p = write_block (dtp, len);
  if (p == NULL)
    return;

  if (nspaces > 0)
    memset (&p[len - nspaces], ' ', nspaces);
}


/* List-directed writing.  */


/* Write a single character to the output.  Returns nonzero if
   something goes wrong.  */

static int
write_char (st_parameter_dt *dtp, char c)
{
  char *p;

  p = write_block (dtp, 1);
  if (p == NULL)
    return 1;

  *p = c;

  return 0;
}


/* Write a list-directed logical value.  */

static void
write_logical (st_parameter_dt *dtp, const char *source, int length)
{
  write_char (dtp, extract_int (source, length) ? 'T' : 'F');
}


/* Write a list-directed integer value.  */

static void
write_integer (st_parameter_dt *dtp, const char *source, int length)
{
  char *p;
  const char *q;
  int digits;
  int width;
  char itoa_buf[GFC_ITOA_BUF_SIZE];

  q = gfc_itoa (extract_int (source, length), itoa_buf, sizeof (itoa_buf));

  switch (length)
    {
    case 1:
      width = 4;
      break;

    case 2:
      width = 6;
      break;

    case 4:
      width = 11;
      break;

    case 8:
      width = 20;
      break;

    default:
      width = 0;
      break;
    }

  digits = strlen (q);

  if (width < digits)
    width = digits;
  p = write_block (dtp, width);
  if (p == NULL)
    return;
  if (dtp->u.p.no_leading_blank)
    {
      memcpy (p, q, digits);
      memset (p + digits, ' ', width - digits);
    }
  else
    {
      memset (p, ' ', width - digits);
      memcpy (p + width - digits, q, digits);
    }
}


/* Write a list-directed string.  We have to worry about delimiting
   the strings if the file has been opened in that mode.  */

static void
write_character (st_parameter_dt *dtp, const char *source, int length)
{
  int i, extra;
  char *p, d;

  switch (dtp->u.p.current_unit->flags.delim)
    {
    case DELIM_APOSTROPHE:
      d = '\'';
      break;
    case DELIM_QUOTE:
      d = '"';
      break;
    default:
      d = ' ';
      break;
    }

  if (d == ' ')
    extra = 0;
  else
    {
      extra = 2;

      for (i = 0; i < length; i++)
	if (source[i] == d)
	  extra++;
    }

  p = write_block (dtp, length + extra);
  if (p == NULL)
    return;

  if (d == ' ')
    memcpy (p, source, length);
  else
    {
      *p++ = d;

      for (i = 0; i < length; i++)
	{
	  *p++ = source[i];
	  if (source[i] == d)
	    *p++ = d;
	}

      *p = d;
    }
}


/* Output a real number with default format.
   This is 1PG14.7E2 for REAL(4), 1PG23.15E3 for REAL(8),
   1PG28.19E4 for REAL(10) and 1PG43.34E4 for REAL(16).  */

static void
write_real (st_parameter_dt *dtp, const char *source, int length)
{
  fnode f ;
  int org_scale = dtp->u.p.scale_factor;
  f.format = FMT_G;
  dtp->u.p.scale_factor = 1;
  switch (length)
    {
    case 4:
      f.u.real.w = 15;
      f.u.real.d = 8;
      f.u.real.e = 2;
      break;
    case 8:
      f.u.real.w = 25;
      f.u.real.d = 17;
      f.u.real.e = 3;
      break;
    case 10:
      f.u.real.w = 29;
      f.u.real.d = 20;
      f.u.real.e = 4;
      break;
    case 16:
      f.u.real.w = 44;
      f.u.real.d = 35;
      f.u.real.e = 4;
      break;
    default:
      internal_error (&dtp->common, "bad real kind");
      break;
    }
  write_float (dtp, &f, source , length);
  dtp->u.p.scale_factor = org_scale;
}


static void
write_complex (st_parameter_dt *dtp, const char *source, int kind, size_t size)
{
  if (write_char (dtp, '('))
    return;
  write_real (dtp, source, kind);

  if (write_char (dtp, ','))
    return;
  write_real (dtp, source + size / 2, kind);

  write_char (dtp, ')');
}


/* Write the separator between items.  */

static void
write_separator (st_parameter_dt *dtp)
{
  char *p;

  p = write_block (dtp, options.separator_len);
  if (p == NULL)
    return;

  memcpy (p, options.separator, options.separator_len);
}


/* Write an item with list formatting.
   TODO: handle skipping to the next record correctly, particularly
   with strings.  */

static void
list_formatted_write_scalar (st_parameter_dt *dtp, bt type, void *p, int kind,
			     size_t size)
{
  if (dtp->u.p.current_unit == NULL)
    return;

  if (dtp->u.p.first_item)
    {
      dtp->u.p.first_item = 0;
      write_char (dtp, ' ');
    }
  else
    {
      if (type != BT_CHARACTER || !dtp->u.p.char_flag ||
	  dtp->u.p.current_unit->flags.delim != DELIM_NONE)
	write_separator (dtp);
    }

  switch (type)
    {
    case BT_INTEGER:
      write_integer (dtp, p, kind);
      break;
    case BT_LOGICAL:
      write_logical (dtp, p, kind);
      break;
    case BT_CHARACTER:
      write_character (dtp, p, kind);
      break;
    case BT_REAL:
      write_real (dtp, p, kind);
      break;
    case BT_COMPLEX:
      write_complex (dtp, p, kind, size);
      break;
    default:
      internal_error (&dtp->common, "list_formatted_write(): Bad type");
    }

  dtp->u.p.char_flag = (type == BT_CHARACTER);
}


void
list_formatted_write (st_parameter_dt *dtp, bt type, void *p, int kind,
		      size_t size, size_t nelems)
{
  size_t elem;
  char *tmp;

  tmp = (char *) p;

  /* Big loop over all the elements.  */
  for (elem = 0; elem < nelems; elem++)
    {
      dtp->u.p.item_count++;
      list_formatted_write_scalar (dtp, type, tmp + size*elem, kind, size);
    }
}

/*			NAMELIST OUTPUT

   nml_write_obj writes a namelist object to the output stream.  It is called
   recursively for derived type components:
	obj    = is the namelist_info for the current object.
	offset = the offset relative to the address held by the object for
		 derived type arrays.
	base   = is the namelist_info of the derived type, when obj is a
		 component.
	base_name = the full name for a derived type, including qualifiers
		    if any.
   The returned value is a pointer to the object beyond the last one
   accessed, including nested derived types.  Notice that the namelist is
   a linear linked list of objects, including derived types and their
   components.  A tree, of sorts, is implied by the compound names of
   the derived type components and this is how this function recurses through
   the list.  */

/* A generous estimate of the number of characters needed to print
   repeat counts and indices, including commas, asterices and brackets.  */

#define NML_DIGITS 20

static namelist_info *
nml_write_obj (st_parameter_dt *dtp, namelist_info * obj, index_type offset,
	       namelist_info * base, char * base_name)
{
  int rep_ctr;
  int num;
  int nml_carry;
  index_type len;
  index_type obj_size;
  index_type nelem;
  index_type dim_i;
  index_type clen;
  index_type elem_ctr;
  index_type obj_name_len;
  void * p ;
  char cup;
  char * obj_name;
  char * ext_name;
  char rep_buff[NML_DIGITS];
  namelist_info * cmp;
  namelist_info * retval = obj->next;
  size_t base_name_len;
  size_t base_var_name_len;
  size_t tot_len;
  unit_delim tmp_delim;

  /* Write namelist variable names in upper case. If a derived type,
     nothing is output.  If a component, base and base_name are set.  */

  if (obj->type != GFC_DTYPE_DERIVED)
    {
#ifdef HAVE_CRLF
      write_character (dtp, "\r\n ", 3);
#else
      write_character (dtp, "\n ", 2);
#endif
      len = 0;
      if (base)
	{
	  len =strlen (base->var_name);
	  for (dim_i = 0; dim_i < (index_type) strlen (base_name); dim_i++)
            {
	      cup = toupper (base_name[dim_i]);
	      write_character (dtp, &cup, 1);
            }
	}
      for (dim_i =len; dim_i < (index_type) strlen (obj->var_name); dim_i++)
	{
	  cup = toupper (obj->var_name[dim_i]);
	  write_character (dtp, &cup, 1);
	}
      write_character (dtp, "=", 1);
    }

  /* Counts the number of data output on a line, including names.  */

  num = 1;

  len = obj->len;

  switch (obj->type)
    {

    case GFC_DTYPE_REAL:
      obj_size = size_from_real_kind (len);
      break;

    case GFC_DTYPE_COMPLEX:
      obj_size = size_from_complex_kind (len);
      break;

    case GFC_DTYPE_CHARACTER:
      obj_size = obj->string_length;
      break;

    default:
      obj_size = len;      
    }

  if (obj->var_rank)
    obj_size = obj->size;

  /* Set the index vector and count the number of elements.  */

  nelem = 1;
  for (dim_i=0; dim_i < obj->var_rank; dim_i++)
    {
      obj->ls[dim_i].idx = obj->dim[dim_i].lbound;
      nelem = nelem * (obj->dim[dim_i].ubound + 1 - obj->dim[dim_i].lbound);
    }

  /* Main loop to output the data held in the object.  */

  rep_ctr = 1;
  for (elem_ctr = 0; elem_ctr < nelem; elem_ctr++)
    {

      /* Build the pointer to the data value.  The offset is passed by
	 recursive calls to this function for arrays of derived types.
	 Is NULL otherwise.  */

      p = (void *)(obj->mem_pos + elem_ctr * obj_size);
      p += offset;

      /* Check for repeat counts of intrinsic types.  */

      if ((elem_ctr < (nelem - 1)) &&
	  (obj->type != GFC_DTYPE_DERIVED) &&
	  !memcmp (p, (void*)(p + obj_size ), obj_size ))
	{
	  rep_ctr++;
	}

      /* Execute a repeated output.  Note the flag no_leading_blank that
	 is used in the functions used to output the intrinsic types.  */

      else
	{
	  if (rep_ctr > 1)
	    {
	      sprintf(rep_buff, " %d*", rep_ctr);
	      write_character (dtp, rep_buff, strlen (rep_buff));
	      dtp->u.p.no_leading_blank = 1;
	    }
	  num++;

	  /* Output the data, if an intrinsic type, or recurse into this
	     routine to treat derived types.  */

	  switch (obj->type)
	    {

	    case GFC_DTYPE_INTEGER:
	      write_integer (dtp, p, len);
              break;

	    case GFC_DTYPE_LOGICAL:
	      write_logical (dtp, p, len);
              break;

	    case GFC_DTYPE_CHARACTER:
	      tmp_delim = dtp->u.p.current_unit->flags.delim;
	      if (dtp->u.p.nml_delim == '"')
		dtp->u.p.current_unit->flags.delim = DELIM_QUOTE;
	      if (dtp->u.p.nml_delim == '\'')
		dtp->u.p.current_unit->flags.delim = DELIM_APOSTROPHE;
	      write_character (dtp, p, obj->string_length);
	      dtp->u.p.current_unit->flags.delim = tmp_delim;
              break;

	    case GFC_DTYPE_REAL:
	      write_real (dtp, p, len);
              break;

	    case GFC_DTYPE_COMPLEX:
	      dtp->u.p.no_leading_blank = 0;
	      num++;
              write_complex (dtp, p, len, obj_size);
              break;

	    case GFC_DTYPE_DERIVED:

	      /* To treat a derived type, we need to build two strings:
		 ext_name = the name, including qualifiers that prepends
			    component names in the output - passed to
			    nml_write_obj.
		 obj_name = the derived type name with no qualifiers but %
			    appended.  This is used to identify the
			    components.  */

	      /* First ext_name => get length of all possible components  */

	      base_name_len = base_name ? strlen (base_name) : 0;
	      base_var_name_len = base ? strlen (base->var_name) : 0;
	      ext_name = (char*)get_mem ( base_name_len
					+ base_var_name_len
					+ strlen (obj->var_name)
					+ obj->var_rank * NML_DIGITS
					+ 1);

	      memcpy (ext_name, base_name, base_name_len);
	      clen = strlen (obj->var_name + base_var_name_len);
	      memcpy (ext_name + base_name_len, 
		      obj->var_name + base_var_name_len, clen);
	      
	      /* Append the qualifier.  */

	      tot_len = base_name_len + clen;
	      for (dim_i = 0; dim_i < obj->var_rank; dim_i++)
		{
		  if (!dim_i)
		    {
		      ext_name[tot_len] = '(';
		      tot_len++;
		    }
		  sprintf (ext_name + tot_len, "%d", (int) obj->ls[dim_i].idx);
		  tot_len += strlen (ext_name + tot_len);
		  ext_name[tot_len] = (dim_i == obj->var_rank - 1) ? ')' : ',';
		  tot_len++;
		}

	      ext_name[tot_len] = '\0';

	      /* Now obj_name.  */

	      obj_name_len = strlen (obj->var_name) + 1;
	      obj_name = get_mem (obj_name_len+1);
	      memcpy (obj_name, obj->var_name, obj_name_len-1);
	      memcpy (obj_name + obj_name_len-1, "%", 2);

	      /* Now loop over the components. Update the component pointer
		 with the return value from nml_write_obj => this loop jumps
		 past nested derived types.  */

	      for (cmp = obj->next;
		   cmp && !strncmp (cmp->var_name, obj_name, obj_name_len);
		   cmp = retval)
		{
		  retval = nml_write_obj (dtp, cmp,
					  (index_type)(p - obj->mem_pos),
					  obj, ext_name);
		}

	      free_mem (obj_name);
	      free_mem (ext_name);
	      goto obj_loop;

            default:
	      internal_error (&dtp->common, "Bad type for namelist write");
            }

	  /* Reset the leading blank suppression, write a comma and, if 5
	     values have been output, write a newline and advance to column
	     2. Reset the repeat counter.  */

	  dtp->u.p.no_leading_blank = 0;
	  write_character (dtp, ",", 1);
	  if (num > 5)
	    {
	      num = 0;
#ifdef HAVE_CRLF
	      write_character (dtp, "\r\n ", 3);
#else
	      write_character (dtp, "\n ", 2);
#endif
	    }
	  rep_ctr = 1;
	}

    /* Cycle through and increment the index vector.  */

obj_loop:

    nml_carry = 1;
    for (dim_i = 0; nml_carry && (dim_i < obj->var_rank); dim_i++)
      {
	obj->ls[dim_i].idx += nml_carry ;
	nml_carry = 0;
	if (obj->ls[dim_i].idx  > (ssize_t)obj->dim[dim_i].ubound)
	  {
	    obj->ls[dim_i].idx = obj->dim[dim_i].lbound;
	    nml_carry = 1;
	  }
       }
    }

  /* Return a pointer beyond the furthest object accessed.  */

  return retval;
}

/* This is the entry function for namelist writes.  It outputs the name
   of the namelist and iterates through the namelist by calls to
   nml_write_obj.  The call below has dummys in the arguments used in
   the treatment of derived types.  */

void
namelist_write (st_parameter_dt *dtp)
{
  namelist_info * t1, *t2, *dummy = NULL;
  index_type i;
  index_type dummy_offset = 0;
  char c;
  char * dummy_name = NULL;
  unit_delim tmp_delim;

  /* Set the delimiter for namelist output.  */

  tmp_delim = dtp->u.p.current_unit->flags.delim;

  dtp->u.p.nml_delim = tmp_delim == DELIM_APOSTROPHE ? '\'' : '"';

  /* Temporarily disable namelist delimters.  */
  dtp->u.p.current_unit->flags.delim = DELIM_NONE;

  write_character (dtp, "&", 1);

  /* Write namelist name in upper case - f95 std.  */
  for (i = 0 ;i < dtp->namelist_name_len ;i++ )
    {
      c = toupper (dtp->namelist_name[i]);
      write_character (dtp, &c ,1);
    }

  if (dtp->u.p.ionml != NULL)
    {
      t1 = dtp->u.p.ionml;
      while (t1 != NULL)
	{
	  t2 = t1;
	  t1 = nml_write_obj (dtp, t2, dummy_offset, dummy, dummy_name);
	}
    }

#ifdef HAVE_CRLF
  write_character (dtp, "  /\r\n", 5);
#else
  write_character (dtp, "  /\n", 4);
#endif

  /* Restore the original delimiter.  */
  dtp->u.p.current_unit->flags.delim = tmp_delim;
}

#undef NML_DIGITS
