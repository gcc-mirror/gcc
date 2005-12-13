/* Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught
   Namelist transfer functions contributed by Paul Thomas

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


/* transfer.c -- Top level handling of data transfer statements.  */

#include "config.h"
#include <string.h>
#include <assert.h>
#include "libgfortran.h"
#include "io.h"


/* Calling conventions:  Data transfer statements are unlike other
   library calls in that they extend over several calls.

   The first call is always a call to st_read() or st_write().  These
   subroutines return no status unless a namelist read or write is
   being done, in which case there is the usual status.  No further
   calls are necessary in this case.

   For other sorts of data transfer, there are zero or more data
   transfer statement that depend on the format of the data transfer
   statement.

      transfer_integer
      transfer_logical
      transfer_character
      transfer_real
      transfer_complex

    These subroutines do not return status.

    The last call is a call to st_[read|write]_done().  While
    something can easily go wrong with the initial st_read() or
    st_write(), an error inhibits any data from actually being
    transferred.  */

extern void transfer_integer (st_parameter_dt *, void *, int);
export_proto(transfer_integer);

extern void transfer_real (st_parameter_dt *, void *, int);
export_proto(transfer_real);

extern void transfer_logical (st_parameter_dt *, void *, int);
export_proto(transfer_logical);

extern void transfer_character (st_parameter_dt *, void *, int);
export_proto(transfer_character);

extern void transfer_complex (st_parameter_dt *, void *, int);
export_proto(transfer_complex);

extern void transfer_array (st_parameter_dt *, gfc_array_char *, int,
			    gfc_charlen_type);
export_proto(transfer_array);

static const st_option advance_opt[] = {
  {"yes", ADVANCE_YES},
  {"no", ADVANCE_NO},
  {NULL, 0}
};


typedef enum
{ FORMATTED_SEQUENTIAL, UNFORMATTED_SEQUENTIAL,
  FORMATTED_DIRECT, UNFORMATTED_DIRECT
}
file_mode;


static file_mode
current_mode (st_parameter_dt *dtp)
{
  file_mode m;

  if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT)
    {
      m = dtp->u.p.current_unit->flags.form == FORM_FORMATTED ?
	FORMATTED_DIRECT : UNFORMATTED_DIRECT;
    }
  else
    {
      m = dtp->u.p.current_unit->flags.form == FORM_FORMATTED ?
	FORMATTED_SEQUENTIAL : UNFORMATTED_SEQUENTIAL;
    }

  return m;
}


/* Mid level data transfer statements.  These subroutines do reading
   and writing in the style of salloc_r()/salloc_w() within the
   current record.  */

/* When reading sequential formatted records we have a problem.  We
   don't know how long the line is until we read the trailing newline,
   and we don't want to read too much.  If we read too much, we might
   have to do a physical seek backwards depending on how much data is
   present, and devices like terminals aren't seekable and would cause
   an I/O error.

   Given this, the solution is to read a byte at a time, stopping if
   we hit the newline.  For small locations, we use a static buffer.
   For larger allocations, we are forced to allocate memory on the
   heap.  Hopefully this won't happen very often.  */

static char *
read_sf (st_parameter_dt *dtp, int *length)
{
  char *base, *p, *q;
  int n, readlen, crlf;
  gfc_offset pos;

  if (*length > SCRATCH_SIZE)
    dtp->u.p.line_buffer = get_mem (*length);
  p = base = dtp->u.p.line_buffer;

  /* If we have seen an eor previously, return a length of 0.  The
     caller is responsible for correctly padding the input field.  */
  if (dtp->u.p.sf_seen_eor)
    {
      *length = 0;
      return base;
    }

  readlen = 1;
  n = 0;

  do
    {
      if (is_internal_unit (dtp))
	{
	  /* readlen may be modified inside salloc_r if
	     is_internal_unit (dtp) is true.  */
	  readlen = 1;
	}

      q = salloc_r (dtp->u.p.current_unit->s, &readlen);
      if (q == NULL)
	break;

      /* If we have a line without a terminating \n, drop through to
	 EOR below.  */
      if (readlen < 1 && n == 0)
	{
	  generate_error (&dtp->common, ERROR_END, NULL);
	  return NULL;
	}

      if (readlen < 1 || *q == '\n' || *q == '\r')
	{
	  /* Unexpected end of line.  */

	  /* If we see an EOR during non-advancing I/O, we need to skip
	     the rest of the I/O statement.  Set the corresponding flag.  */
	  if (dtp->u.p.advance_status == ADVANCE_NO || dtp->u.p.seen_dollar)
	    dtp->u.p.eor_condition = 1;

	  crlf = 0;
	  /* If we encounter a CR, it might be a CRLF.  */
	  if (*q == '\r') /* Probably a CRLF */
	    {
	      readlen = 1;
	      pos = stream_offset (dtp->u.p.current_unit->s);
	      q = salloc_r (dtp->u.p.current_unit->s, &readlen);
	      if (*q != '\n' && readlen == 1) /* Not a CRLF after all.  */
		sseek (dtp->u.p.current_unit->s, pos);
	      else
		crlf = 1;
	    }

	  /* Without padding, terminate the I/O statement without assigning
	     the value.  With padding, the value still needs to be assigned,
	     so we can just continue with a short read.  */
	  if (dtp->u.p.current_unit->flags.pad == PAD_NO)
	    {
	      generate_error (&dtp->common, ERROR_EOR, NULL);
	      return NULL;
	    }

	  *length = n;
	  dtp->u.p.sf_seen_eor = (crlf ? 2 : 1);
	  break;
	}

      n++;
      *p++ = *q;
      dtp->u.p.sf_seen_eor = 0;
    }
  while (n < *length);
  dtp->u.p.current_unit->bytes_left -= *length;

  if ((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0)
    *dtp->size += *length;

  return base;
}


/* Function for reading the next couple of bytes from the current
   file, advancing the current position.  We return a pointer to a
   buffer containing the bytes.  We return NULL on end of record or
   end of file.

   If the read is short, then it is because the current record does not
   have enough data to satisfy the read request and the file was
   opened with PAD=YES.  The caller must assume tailing spaces for
   short reads.  */

void *
read_block (st_parameter_dt *dtp, int *length)
{
  char *source;
  int nread;

  if (dtp->u.p.current_unit->bytes_left < *length)
    {
      if (dtp->u.p.current_unit->flags.pad == PAD_NO)
	{
	  generate_error (&dtp->common, ERROR_EOR, NULL);
	  /* Not enough data left.  */
	  return NULL;
	}

      *length = dtp->u.p.current_unit->bytes_left;
    }

  if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED &&
      dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
    return read_sf (dtp, length);	/* Special case.  */

  dtp->u.p.current_unit->bytes_left -= *length;

  nread = *length;
  source = salloc_r (dtp->u.p.current_unit->s, &nread);

  if ((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0)
    *dtp->size += nread;

  if (nread != *length)
    {				/* Short read, this shouldn't happen.  */
      if (dtp->u.p.current_unit->flags.pad == PAD_YES)
	*length = nread;
      else
	{
	  generate_error (&dtp->common, ERROR_EOR, NULL);
	  source = NULL;
	}
    }

  return source;
}


/* Reads a block directly into application data space.  */

static void
read_block_direct (st_parameter_dt *dtp, void *buf, size_t *nbytes)
{
  int *length;
  void *data;
  size_t nread;

  if (dtp->u.p.current_unit->bytes_left < *nbytes)
    {
      if (dtp->u.p.current_unit->flags.pad == PAD_NO)
	{
	  /* Not enough data left.  */
	  generate_error (&dtp->common, ERROR_EOR, NULL);
	  return;
	}

      *nbytes = dtp->u.p.current_unit->bytes_left;
    }

  if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED &&
      dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
    {
      length = (int *) nbytes;
      data = read_sf (dtp, length);	/* Special case.  */
      memcpy (buf, data, (size_t) *length);
      return;
    }

  dtp->u.p.current_unit->bytes_left -= *nbytes;

  nread = *nbytes;
  if (sread (dtp->u.p.current_unit->s, buf, &nread) != 0)
    {
      generate_error (&dtp->common, ERROR_OS, NULL);
      return;
    }

  if ((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0)
    *dtp->size += (GFC_INTEGER_4) nread;

  if (nread != *nbytes)
    {				/* Short read, e.g. if we hit EOF.  */
      if (dtp->u.p.current_unit->flags.pad == PAD_YES)
	{
	  memset (((char *) buf) + nread, ' ', *nbytes - nread);
	  *nbytes = nread;
	}
      else
	generate_error (&dtp->common, ERROR_EOR, NULL);
    }
}


/* Function for writing a block of bytes to the current file at the
   current position, advancing the file pointer. We are given a length
   and return a pointer to a buffer that the caller must (completely)
   fill in.  Returns NULL on error.  */

void *
write_block (st_parameter_dt *dtp, int length)
{
  char *dest;
  
  if (dtp->u.p.current_unit->bytes_left < length)
    {
      generate_error (&dtp->common, ERROR_EOR, NULL);
      return NULL;
    }

  dtp->u.p.current_unit->bytes_left -= (gfc_offset) length;
  dest = salloc_w (dtp->u.p.current_unit->s, &length);
  
  if (dest == NULL)
    {
      generate_error (&dtp->common, ERROR_END, NULL);
      return NULL;
    }

  if ((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0)
    *dtp->size += length;

  return dest;
}


/* Writes a block directly without necessarily allocating space in a
   buffer.  */

static void
write_block_direct (st_parameter_dt *dtp, void *buf, size_t *nbytes)
{
  if (dtp->u.p.current_unit->bytes_left < *nbytes)
    generate_error (&dtp->common, ERROR_EOR, NULL);

  dtp->u.p.current_unit->bytes_left -= (gfc_offset) *nbytes;

  if (swrite (dtp->u.p.current_unit->s, buf, nbytes) != 0)
    generate_error (&dtp->common, ERROR_OS, NULL);

  if ((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0)
    *dtp->size += (GFC_INTEGER_4) *nbytes;
}


/* Master function for unformatted reads.  */

static void
unformatted_read (st_parameter_dt *dtp, bt type,
		  void *dest, int kind,
		  size_t size, size_t nelems)
{
  /* Currently, character implies size=1.  */
  if (dtp->u.p.current_unit->flags.convert == CONVERT_NATIVE
      || size == 1 || type == BT_CHARACTER)
    {
      size *= nelems;
      read_block_direct (dtp, dest, &size);
    }
  else
    {
      char buffer[16];
      char *p;
      size_t i, sz;
      
      /* Break up complex into its constituent reals.  */
      if (type == BT_COMPLEX)
	{
	  nelems *= 2;
	  size /= 2;
	}
      p = dest;
      
      /* By now, all complex variables have been split into their
	 constituent reals.  For types with padding, we only need to
	 read kind bytes.  We don't care about the contents
	 of the padding.  */
      
      sz = kind;
      for (i=0; i<nelems; i++)
	{
 	  read_block_direct (dtp, buffer, &sz);
 	  reverse_memcpy (p, buffer, sz);
 	  p += size;
 	}
    }
}


/* Master function for unformatted writes.  */

static void
unformatted_write (st_parameter_dt *dtp, bt type,
		   void *source, int kind,
		   size_t size, size_t nelems)
{
  if (dtp->u.p.current_unit->flags.convert == CONVERT_NATIVE ||
      size == 1 || type == BT_CHARACTER)
    {
      size *= nelems;

      write_block_direct (dtp, source, &size);
    }
  else
    {
      char buffer[16];
      char *p;
      size_t i, sz;
  
      /* Break up complex into its constituent reals.  */
      if (type == BT_COMPLEX)
	{
	  nelems *= 2;
	  size /= 2;
	}      

      p = source;

      /* By now, all complex variables have been split into their
	 constituent reals.  For types with padding, we only need to
	 read kind bytes.  We don't care about the contents
	 of the padding.  */

      sz = kind;
      for (i=0; i<nelems; i++)
	{
	  reverse_memcpy(buffer, p, size);
 	  p+= size;
	  write_block_direct (dtp, buffer, &sz);
	}
    }
}


/* Return a pointer to the name of a type.  */

const char *
type_name (bt type)
{
  const char *p;

  switch (type)
    {
    case BT_INTEGER:
      p = "INTEGER";
      break;
    case BT_LOGICAL:
      p = "LOGICAL";
      break;
    case BT_CHARACTER:
      p = "CHARACTER";
      break;
    case BT_REAL:
      p = "REAL";
      break;
    case BT_COMPLEX:
      p = "COMPLEX";
      break;
    default:
      internal_error (NULL, "type_name(): Bad type");
    }

  return p;
}


/* Write a constant string to the output.
   This is complicated because the string can have doubled delimiters
   in it.  The length in the format node is the true length.  */

static void
write_constant_string (st_parameter_dt *dtp, const fnode *f)
{
  char c, delimiter, *p, *q;
  int length;

  length = f->u.string.length;
  if (length == 0)
    return;

  p = write_block (dtp, length);
  if (p == NULL)
    return;

  q = f->u.string.p;
  delimiter = q[-1];

  for (; length > 0; length--)
    {
      c = *p++ = *q++;
      if (c == delimiter && c != 'H' && c != 'h')
	q++;			/* Skip the doubled delimiter.  */
    }
}


/* Given actual and expected types in a formatted data transfer, make
   sure they agree.  If not, an error message is generated.  Returns
   nonzero if something went wrong.  */

static int
require_type (st_parameter_dt *dtp, bt expected, bt actual, const fnode *f)
{
  char buffer[100];

  if (actual == expected)
    return 0;

  st_sprintf (buffer, "Expected %s for item %d in formatted transfer, got %s",
	      type_name (expected), dtp->u.p.item_count, type_name (actual));

  format_error (dtp, f, buffer);
  return 1;
}


/* This subroutine is the main loop for a formatted data transfer
   statement.  It would be natural to implement this as a coroutine
   with the user program, but C makes that awkward.  We loop,
   processesing format elements.  When we actually have to transfer
   data instead of just setting flags, we return control to the user
   program which calls a subroutine that supplies the address and type
   of the next element, then comes back here to process it.  */

static void
formatted_transfer_scalar (st_parameter_dt *dtp, bt type, void *p, int len,
			   size_t size)
{
  char scratch[SCRATCH_SIZE];
  int pos, bytes_used;
  const fnode *f;
  format_token t;
  int n;
  int consume_data_flag;

  /* Change a complex data item into a pair of reals.  */

  n = (p == NULL) ? 0 : ((type != BT_COMPLEX) ? 1 : 2);
  if (type == BT_COMPLEX)
    {
      type = BT_REAL;
      size /= 2;
    }

  /* If there's an EOR condition, we simulate finalizing the transfer
     by doing nothing.  */
  if (dtp->u.p.eor_condition)
    return;

  dtp->u.p.line_buffer = scratch;
  for (;;)
    {
      /* If reversion has occurred and there is another real data item,
	 then we have to move to the next record.  */
      if (dtp->u.p.reversion_flag && n > 0)
	{
	  dtp->u.p.reversion_flag = 0;
	  next_record (dtp, 0);
	}

      consume_data_flag = 1 ;
      if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
	break;

      f = next_format (dtp);
      if (f == NULL)
	return;	      /* No data descriptors left (already raised).  */

      /* Now discharge T, TR and X movements to the right.  This is delayed
	 until a data producing format to suppress trailing spaces.  */
	 
      t = f->format;
      if (dtp->u.p.mode == WRITING && dtp->u.p.skips != 0
	&& ((n>0 && (  t == FMT_I  || t == FMT_B  || t == FMT_O
		    || t == FMT_Z  || t == FMT_F  || t == FMT_E
		    || t == FMT_EN || t == FMT_ES || t == FMT_G
		    || t == FMT_L  || t == FMT_A  || t == FMT_D))
	    || t == FMT_STRING))
	{
	  if (dtp->u.p.skips > 0)
	    {
	      write_x (dtp, dtp->u.p.skips, dtp->u.p.pending_spaces);
	      dtp->u.p.max_pos = (int)(dtp->u.p.current_unit->recl
				       - dtp->u.p.current_unit->bytes_left);
	    }
	  if (dtp->u.p.skips < 0)
	    {
	      move_pos_offset (dtp->u.p.current_unit->s, dtp->u.p.skips);
	      dtp->u.p.current_unit->bytes_left -= (gfc_offset) dtp->u.p.skips;
	    }
	  dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
	}

      bytes_used = (int)(dtp->u.p.current_unit->recl - dtp->u.p.current_unit->bytes_left);

      switch (t)
	{
	case FMT_I:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_INTEGER, type, f))
	    return;

	  if (dtp->u.p.mode == READING)
	    read_decimal (dtp, f, p, len);
	  else
	    write_i (dtp, f, p, len);

	  break;

	case FMT_B:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_INTEGER, type, f))
	    return;

	  if (dtp->u.p.mode == READING)
	    read_radix (dtp, f, p, len, 2);
	  else
	    write_b (dtp, f, p, len);

	  break;

	case FMT_O:
	  if (n == 0)
	    goto need_data;

	  if (dtp->u.p.mode == READING)
	    read_radix (dtp, f, p, len, 8);
	  else
	    write_o (dtp, f, p, len);

	  break;

	case FMT_Z:
	  if (n == 0)
	    goto need_data;

	  if (dtp->u.p.mode == READING)
	    read_radix (dtp, f, p, len, 16);
	  else
	    write_z (dtp, f, p, len);

	  break;

	case FMT_A:
	  if (n == 0)
	    goto need_data;

	  if (dtp->u.p.mode == READING)
	    read_a (dtp, f, p, len);
	  else
	    write_a (dtp, f, p, len);

	  break;

	case FMT_L:
	  if (n == 0)
	    goto need_data;

	  if (dtp->u.p.mode == READING)
	    read_l (dtp, f, p, len);
	  else
	    write_l (dtp, f, p, len);

	  break;

	case FMT_D:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;

	  if (dtp->u.p.mode == READING)
	    read_f (dtp, f, p, len);
	  else
	    write_d (dtp, f, p, len);

	  break;

	case FMT_E:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;

	  if (dtp->u.p.mode == READING)
	    read_f (dtp, f, p, len);
	  else
	    write_e (dtp, f, p, len);
	  break;

	case FMT_EN:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;

	  if (dtp->u.p.mode == READING)
	    read_f (dtp, f, p, len);
	  else
	    write_en (dtp, f, p, len);

	  break;

	case FMT_ES:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;

	  if (dtp->u.p.mode == READING)
	    read_f (dtp, f, p, len);
	  else
	    write_es (dtp, f, p, len);

	  break;

	case FMT_F:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;

	  if (dtp->u.p.mode == READING)
	    read_f (dtp, f, p, len);
	  else
	    write_f (dtp, f, p, len);

	  break;

	case FMT_G:
	  if (n == 0)
	    goto need_data;
	  if (dtp->u.p.mode == READING)
	    switch (type)
	      {
	      case BT_INTEGER:
		read_decimal (dtp, f, p, len);
		break;
	      case BT_LOGICAL:
		read_l (dtp, f, p, len);
		break;
	      case BT_CHARACTER:
		read_a (dtp, f, p, len);
		break;
	      case BT_REAL:
		read_f (dtp, f, p, len);
		break;
	      default:
		goto bad_type;
	      }
	  else
	    switch (type)
	      {
	      case BT_INTEGER:
		write_i (dtp, f, p, len);
		break;
	      case BT_LOGICAL:
		write_l (dtp, f, p, len);
		break;
	      case BT_CHARACTER:
		write_a (dtp, f, p, len);
		break;
	      case BT_REAL:
		write_d (dtp, f, p, len);
		break;
	      default:
	      bad_type:
		internal_error (&dtp->common,
				"formatted_transfer(): Bad type");
	      }

	  break;

	case FMT_STRING:
	  consume_data_flag = 0 ;
	  if (dtp->u.p.mode == READING)
	    {
	      format_error (dtp, f, "Constant string in input format");
	      return;
	    }
	  write_constant_string (dtp, f);
	  break;

	/* Format codes that don't transfer data.  */
	case FMT_X:
	case FMT_TR:
	  consume_data_flag = 0 ;

	  pos = bytes_used + f->u.n + dtp->u.p.skips;
	  dtp->u.p.skips = f->u.n + dtp->u.p.skips;
	  dtp->u.p.pending_spaces = pos - dtp->u.p.max_pos;

	  /* Writes occur just before the switch on f->format, above, so
	     that trailing blanks are suppressed, unless we are doing a
	     non-advancing write in which case we want to output the blanks
	     now.  */
	  if (dtp->u.p.mode == WRITING
	      && dtp->u.p.advance_status == ADVANCE_NO)
	    {
	      write_x (dtp, dtp->u.p.skips, dtp->u.p.pending_spaces);
	      dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
	    }
	  if (dtp->u.p.mode == READING)
	    read_x (dtp, f->u.n);

	  break;

	case FMT_TL:
	case FMT_T:
	  if (f->format == FMT_TL)
	    pos = bytes_used - f->u.n;
	  else /* FMT_T */
	    {
	      consume_data_flag = 0;
	      pos = f->u.n - 1;
	    }

	  /* Standard 10.6.1.1: excessive left tabbing is reset to the
	     left tab limit.  We do not check if the position has gone
	     beyond the end of record because a subsequent tab could
	     bring us back again.  */
	  pos = pos < 0 ? 0 : pos;

	  dtp->u.p.skips = dtp->u.p.skips + pos - bytes_used;
	  dtp->u.p.pending_spaces = dtp->u.p.pending_spaces
				    + pos - dtp->u.p.max_pos;

	  if (dtp->u.p.skips == 0)
	    break;

	  /* Writes occur just before the switch on f->format, above, so that
	     trailing blanks are suppressed.  */
	  if (dtp->u.p.mode == READING)
	    {
	      /* Adjust everything for end-of-record condition */
	      if (dtp->u.p.sf_seen_eor && !is_internal_unit (dtp))
		{
		  if (dtp->u.p.sf_seen_eor == 2)
		    {
		      /* The EOR was a CRLF (two bytes wide).  */
		      dtp->u.p.current_unit->bytes_left -= 2;
		      dtp->u.p.skips -= 2;
		    }
		  else
		    {
		      /* The EOR marker was only one byte wide.  */
		      dtp->u.p.current_unit->bytes_left--;
		      dtp->u.p.skips--;
		    }
		  bytes_used = pos;
		  dtp->u.p.sf_seen_eor = 0;
		}
	      if (dtp->u.p.skips < 0)
		{
		  move_pos_offset (dtp->u.p.current_unit->s, dtp->u.p.skips);
		  dtp->u.p.current_unit->bytes_left
		    -= (gfc_offset) dtp->u.p.skips;
		  dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
		}
	      else
		read_x (dtp, dtp->u.p.skips);
	    }

	  break;

	case FMT_S:
	  consume_data_flag = 0 ;
	  dtp->u.p.sign_status = SIGN_S;
	  break;

	case FMT_SS:
	  consume_data_flag = 0 ;
	  dtp->u.p.sign_status = SIGN_SS;
	  break;

	case FMT_SP:
	  consume_data_flag = 0 ;
	  dtp->u.p.sign_status = SIGN_SP;
	  break;

	case FMT_BN:
	  consume_data_flag = 0 ;
	  dtp->u.p.blank_status = BLANK_NULL;
	  break;

	case FMT_BZ:
	  consume_data_flag = 0 ;
	  dtp->u.p.blank_status = BLANK_ZERO;
	  break;

	case FMT_P:
	  consume_data_flag = 0 ;
	  dtp->u.p.scale_factor = f->u.k;
	  break;

	case FMT_DOLLAR:
	  consume_data_flag = 0 ;
	  dtp->u.p.seen_dollar = 1;
	  break;

	case FMT_SLASH:
	  consume_data_flag = 0 ;
	  dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
	  next_record (dtp, 0);
	  break;

	case FMT_COLON:
	  /* A colon descriptor causes us to exit this loop (in
	     particular preventing another / descriptor from being
	     processed) unless there is another data item to be
	     transferred.  */
	  consume_data_flag = 0 ;
	  if (n == 0)
	    return;
	  break;

	default:
	  internal_error (&dtp->common, "Bad format node");
	}

      /* Free a buffer that we had to allocate during a sequential
	 formatted read of a block that was larger than the static
	 buffer.  */

      if (dtp->u.p.line_buffer != scratch)
	{
	  free_mem (dtp->u.p.line_buffer);
	  dtp->u.p.line_buffer = scratch;
	}

      /* Adjust the item count and data pointer.  */

      if ((consume_data_flag > 0) && (n > 0))
      {
	n--;
	p = ((char *) p) + size;
      }

      if (dtp->u.p.mode == READING)
	dtp->u.p.skips = 0;

      pos = (int)(dtp->u.p.current_unit->recl - dtp->u.p.current_unit->bytes_left);
      dtp->u.p.max_pos = (dtp->u.p.max_pos > pos) ? dtp->u.p.max_pos : pos;

    }

  return;

  /* Come here when we need a data descriptor but don't have one.  We
     push the current format node back onto the input, then return and
     let the user program call us back with the data.  */
 need_data:
  unget_format (dtp, f);
}

static void
formatted_transfer (st_parameter_dt *dtp, bt type, void *p, int kind,
		    size_t size, size_t nelems)
{
  size_t elem;
  char *tmp;

  tmp = (char *) p;

  /* Big loop over all the elements.  */
  for (elem = 0; elem < nelems; elem++)
    {
      dtp->u.p.item_count++;
      formatted_transfer_scalar (dtp, type, tmp + size*elem, kind, size);
    }
}



/* Data transfer entry points.  The type of the data entity is
   implicit in the subroutine call.  This prevents us from having to
   share a common enum with the compiler.  */

void
transfer_integer (st_parameter_dt *dtp, void *p, int kind)
{
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;
  dtp->u.p.transfer (dtp, BT_INTEGER, p, kind, kind, 1);
}


void
transfer_real (st_parameter_dt *dtp, void *p, int kind)
{
  size_t size;
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;
  size = size_from_real_kind (kind);
  dtp->u.p.transfer (dtp, BT_REAL, p, kind, size, 1);
}


void
transfer_logical (st_parameter_dt *dtp, void *p, int kind)
{
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;
  dtp->u.p.transfer (dtp, BT_LOGICAL, p, kind, kind, 1);
}


void
transfer_character (st_parameter_dt *dtp, void *p, int len)
{
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;
  /* Currently we support only 1 byte chars, and the library is a bit
     confused of character kind vs. length, so we kludge it by setting
     kind = length.  */
  dtp->u.p.transfer (dtp, BT_CHARACTER, p, len, len, 1);
}


void
transfer_complex (st_parameter_dt *dtp, void *p, int kind)
{
  size_t size;
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;
  size = size_from_complex_kind (kind);
  dtp->u.p.transfer (dtp, BT_COMPLEX, p, kind, size, 1);
}


void
transfer_array (st_parameter_dt *dtp, gfc_array_char *desc, int kind,
		gfc_charlen_type charlen)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0, rank, size, type, n;
  size_t tsize;
  char *data;
  bt iotype;

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  type = GFC_DESCRIPTOR_TYPE (desc);
  size = GFC_DESCRIPTOR_SIZE (desc);

  /* FIXME: What a kludge: Array descriptors and the IO library use
     different enums for types.  */
  switch (type)
    {
    case GFC_DTYPE_UNKNOWN:
      iotype = BT_NULL;  /* Is this correct?  */
      break;
    case GFC_DTYPE_INTEGER:
      iotype = BT_INTEGER;
      break;
    case GFC_DTYPE_LOGICAL:
      iotype = BT_LOGICAL;
      break;
    case GFC_DTYPE_REAL:
      iotype = BT_REAL;
      break;
    case GFC_DTYPE_COMPLEX:
      iotype = BT_COMPLEX;
      break;
    case GFC_DTYPE_CHARACTER:
      iotype = BT_CHARACTER;
      /* FIXME: Currently dtype contains the charlen, which is
	 clobbered if charlen > 2**24. That's why we use a separate
	 argument for the charlen. However, if we want to support
	 non-8-bit charsets we need to fix dtype to contain
	 sizeof(chartype) and fix the code below.  */
      size = charlen;
      kind = charlen;
      break;
    case GFC_DTYPE_DERIVED:
      internal_error (&dtp->common,
		"Derived type I/O should have been handled via the frontend.");
      break;
    default:
      internal_error (&dtp->common, "transfer_array(): Bad type");
    }

  if (desc->dim[0].stride == 0)
    desc->dim[0].stride = 1;

  rank = GFC_DESCRIPTOR_RANK (desc);
  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      stride[n] = desc->dim[n].stride;
      extent[n] = desc->dim[n].ubound + 1 - desc->dim[n].lbound;

      /* If the extent of even one dimension is zero, then the entire
	 array section contains zero elements, so we return.  */
      if (extent[n] == 0)
	return;
    }

  stride0 = stride[0];

  /* If the innermost dimension has stride 1, we can do the transfer
     in contiguous chunks.  */
  if (stride0 == 1)
    tsize = extent[0];
  else
    tsize = 1;

  data = GFC_DESCRIPTOR_DATA (desc);

  while (data)
    {
      dtp->u.p.transfer (dtp, iotype, data, kind, size, tsize);
      data += stride0 * size * tsize;
      count[0] += tsize;
      n = 0;
      while (count[n] == extent[n])
	{
	  count[n] = 0;
	  data -= stride[n] * extent[n] * size;
	  n++;
	  if (n == rank)
	    {
	      data = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      data += stride[n] * size;
	    }
	}
    }
}


/* Preposition a sequential unformatted file while reading.  */

static void
us_read (st_parameter_dt *dtp)
{
  char *p;
  int n;
  gfc_offset i;

  n = sizeof (gfc_offset);
  p = salloc_r (dtp->u.p.current_unit->s, &n);

  if (n == 0)
    return;  /* end of file */

  if (p == NULL || n != sizeof (gfc_offset))
    {
      generate_error (&dtp->common, ERROR_BAD_US, NULL);
      return;
    }

  /* Only CONVERT_NATIVE and CONVERT_SWAP are valid here.  */
  if (dtp->u.p.current_unit->flags.convert == CONVERT_NATIVE)
    memcpy (&i, p, sizeof (gfc_offset));
  else
    reverse_memcpy (&i, p, sizeof (gfc_offset));
    
  dtp->u.p.current_unit->bytes_left = i;
}


/* Preposition a sequential unformatted file while writing.  This
   amount to writing a bogus length that will be filled in later.  */

static void
us_write (st_parameter_dt *dtp)
{
  char *p;
  int length;

  length = sizeof (gfc_offset);
  p = salloc_w (dtp->u.p.current_unit->s, &length);

  if (p == NULL)
    {
      generate_error (&dtp->common, ERROR_OS, NULL);
      return;
    }

  memset (p, '\0', sizeof (gfc_offset));	/* Bogus value for now.  */
  if (sfree (dtp->u.p.current_unit->s) == FAILURE)
    generate_error (&dtp->common, ERROR_OS, NULL);

  /* For sequential unformatted, we write until we have more bytes than
     can fit in the record markers. If disk space runs out first, it will
     error on the write.  */
  dtp->u.p.current_unit->recl = max_offset;

  dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
}


/* Position to the next record prior to transfer.  We are assumed to
   be before the next record.  We also calculate the bytes in the next
   record.  */

static void
pre_position (st_parameter_dt *dtp)
{
  if (dtp->u.p.current_unit->current_record)
    return;			/* Already positioned.  */

  switch (current_mode (dtp))
    {
    case UNFORMATTED_SEQUENTIAL:
      if (dtp->u.p.mode == READING)
	us_read (dtp);
      else
	us_write (dtp);

      break;

    case FORMATTED_SEQUENTIAL:
    case FORMATTED_DIRECT:
    case UNFORMATTED_DIRECT:
      dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
      break;
    }

  dtp->u.p.current_unit->current_record = 1;
}


/* Initialize things for a data transfer.  This code is common for
   both reading and writing.  */

static void
data_transfer_init (st_parameter_dt *dtp, int read_flag)
{
  unit_flags u_flags;  /* Used for creating a unit if needed.  */
  GFC_INTEGER_4 cf = dtp->common.flags;
  namelist_info *ionml;

  ionml = ((cf & IOPARM_DT_IONML_SET) != 0) ? dtp->u.p.ionml : NULL;
  memset (&dtp->u.p, 0, sizeof (dtp->u.p));
  dtp->u.p.ionml = ionml;
  dtp->u.p.mode = read_flag ? READING : WRITING;

  if ((cf & IOPARM_DT_HAS_SIZE) != 0)
    *dtp->size = 0;		/* Initialize the count.  */

  dtp->u.p.current_unit = get_unit (dtp, 1);
  if (dtp->u.p.current_unit->s == NULL)
  {  /* Open the unit with some default flags.  */
     st_parameter_open opp;
     if (dtp->common.unit < 0)
     {
       close_unit (dtp->u.p.current_unit);
       dtp->u.p.current_unit = NULL;
       generate_error (&dtp->common, ERROR_BAD_OPTION,
		       "Bad unit number in OPEN statement");
       return;
     }
     memset (&u_flags, '\0', sizeof (u_flags));
     u_flags.access = ACCESS_SEQUENTIAL;
     u_flags.action = ACTION_READWRITE;

     /* Is it unformatted?  */
     if (!(cf & (IOPARM_DT_HAS_FORMAT | IOPARM_DT_LIST_FORMAT
		 | IOPARM_DT_IONML_SET)))
       u_flags.form = FORM_UNFORMATTED;
     else
       u_flags.form = FORM_UNSPECIFIED;

     u_flags.delim = DELIM_UNSPECIFIED;
     u_flags.blank = BLANK_UNSPECIFIED;
     u_flags.pad = PAD_UNSPECIFIED;
     u_flags.status = STATUS_UNKNOWN;
     opp.common = dtp->common;
     opp.common.flags &= IOPARM_COMMON_MASK;
     dtp->u.p.current_unit = new_unit (&opp, dtp->u.p.current_unit, &u_flags);
     dtp->common.flags &= ~IOPARM_COMMON_MASK;
     dtp->common.flags |= (opp.common.flags & IOPARM_COMMON_MASK);
     if (dtp->u.p.current_unit == NULL)
       return;
  }

  /* Check the action.  */

  if (read_flag && dtp->u.p.current_unit->flags.action == ACTION_WRITE)
    generate_error (&dtp->common, ERROR_BAD_ACTION,
		    "Cannot read from file opened for WRITE");

  if (!read_flag && dtp->u.p.current_unit->flags.action == ACTION_READ)
    generate_error (&dtp->common, ERROR_BAD_ACTION,
		    "Cannot write to file opened for READ");

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  dtp->u.p.first_item = 1;

  /* Check the format.  */

  if ((cf & IOPARM_DT_HAS_FORMAT) != 0)
    parse_format (dtp);

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  if (dtp->u.p.current_unit->flags.form == FORM_UNFORMATTED
      && (cf & (IOPARM_DT_HAS_FORMAT | IOPARM_DT_LIST_FORMAT))
	 != 0)
    generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
		    "Format present for UNFORMATTED data transfer");

  if ((cf & IOPARM_DT_HAS_NAMELIST_NAME) != 0 && dtp->u.p.ionml != NULL)
     {
	if ((cf & IOPARM_DT_HAS_FORMAT) != 0)
	   generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
		    "A format cannot be specified with a namelist");
     }
  else if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED &&
	   !(cf & (IOPARM_DT_HAS_FORMAT | IOPARM_DT_LIST_FORMAT)))
    generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
		    "Missing format for FORMATTED data transfer");


  if (is_internal_unit (dtp)
      && dtp->u.p.current_unit->flags.form == FORM_UNFORMATTED)
    generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
		    "Internal file cannot be accessed by UNFORMATTED data transfer");

  /* Check the record number.  */

  if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT
      && (cf & IOPARM_DT_HAS_REC) == 0)
    {
      generate_error (&dtp->common, ERROR_MISSING_OPTION,
		      "Direct access data transfer requires record number");
      return;
    }

  if (dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL
      && (cf & IOPARM_DT_HAS_REC) != 0)
    {
      generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
		      "Record number not allowed for sequential access data transfer");
      return;
    }

  /* Process the ADVANCE option.  */

  dtp->u.p.advance_status
    = !(cf & IOPARM_DT_HAS_ADVANCE) ? ADVANCE_UNSPECIFIED :
      find_option (&dtp->common, dtp->advance, dtp->advance_len, advance_opt,
		   "Bad ADVANCE parameter in data transfer statement");

  if (dtp->u.p.advance_status != ADVANCE_UNSPECIFIED)
    {
      if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT)
	generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
			"ADVANCE specification conflicts with sequential access");

      if (is_internal_unit (dtp))
	generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
			"ADVANCE specification conflicts with internal file");

      if ((cf & (IOPARM_DT_HAS_FORMAT | IOPARM_DT_LIST_FORMAT))
	  != IOPARM_DT_HAS_FORMAT)
	generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
			"ADVANCE specification requires an explicit format");
    }

  if (read_flag)
    {
      if ((cf & IOPARM_EOR) != 0 && dtp->u.p.advance_status != ADVANCE_NO)
	generate_error (&dtp->common, ERROR_MISSING_OPTION,
			"EOR specification requires an ADVANCE specification of NO");

      if ((cf & IOPARM_DT_HAS_SIZE) != 0 && dtp->u.p.advance_status != ADVANCE_NO)
	generate_error (&dtp->common, ERROR_MISSING_OPTION,
			"SIZE specification requires an ADVANCE specification of NO");

    }
  else
    {				/* Write constraints.  */
      if ((cf & IOPARM_END) != 0)
	generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
			"END specification cannot appear in a write statement");

      if ((cf & IOPARM_EOR) != 0)
	generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
			"EOR specification cannot appear in a write statement");

      if ((cf & IOPARM_DT_HAS_SIZE) != 0)
	generate_error (&dtp->common, ERROR_OPTION_CONFLICT,
			"SIZE specification cannot appear in a write statement");
    }

  if (dtp->u.p.advance_status == ADVANCE_UNSPECIFIED)
    dtp->u.p.advance_status = ADVANCE_YES;
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  /* Sanity checks on the record number.  */

  if ((cf & IOPARM_DT_HAS_REC) != 0)
    {
      if (dtp->rec <= 0)
	{
	  generate_error (&dtp->common, ERROR_BAD_OPTION,
			  "Record number must be positive");
	  return;
	}

      if (dtp->rec >= dtp->u.p.current_unit->maxrec)
	{
	  generate_error (&dtp->common, ERROR_BAD_OPTION,
			  "Record number too large");
	  return;
	}

      /* Check to see if we might be reading what we wrote before  */

      if (dtp->u.p.mode == READING && dtp->u.p.current_unit->mode  == WRITING)
	 flush(dtp->u.p.current_unit->s);

      /* Check whether the record exists to be read.  Only
	 a partial record needs to exist.  */

      if (dtp->u.p.mode == READING && (dtp->rec -1)
	  * dtp->u.p.current_unit->recl >= file_length (dtp->u.p.current_unit->s))
	{
	  generate_error (&dtp->common, ERROR_BAD_OPTION,
			  "Non-existing record number");
	  return;
	}

      /* Position the file.  */
      if (sseek (dtp->u.p.current_unit->s,
	       (dtp->rec - 1) * dtp->u.p.current_unit->recl) == FAILURE)
	{
	  generate_error (&dtp->common, ERROR_OS, NULL);
	  return;
	}
    }

  /* Overwriting an existing sequential file ?
     it is always safe to truncate the file on the first write */
  if (dtp->u.p.mode == WRITING
      && dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL
      && dtp->u.p.current_unit->last_record == 0 && !is_preconnected(dtp->u.p.current_unit->s))
	struncate(dtp->u.p.current_unit->s);

  /* Bugware for badly written mixed C-Fortran I/O.  */
  flush_if_preconnected(dtp->u.p.current_unit->s);

  dtp->u.p.current_unit->mode = dtp->u.p.mode;

  /* Set the initial value of flags.  */

  dtp->u.p.blank_status = dtp->u.p.current_unit->flags.blank;
  dtp->u.p.sign_status = SIGN_S;

  pre_position (dtp);

  /* Set up the subroutine that will handle the transfers.  */

  if (read_flag)
    {
      if (dtp->u.p.current_unit->flags.form == FORM_UNFORMATTED)
	dtp->u.p.transfer = unformatted_read;
      else
	{
	  if ((cf & IOPARM_DT_LIST_FORMAT) != 0)
	    dtp->u.p.transfer = list_formatted_read;
	  else
	    dtp->u.p.transfer = formatted_transfer;
	}
    }
  else
    {
      if (dtp->u.p.current_unit->flags.form == FORM_UNFORMATTED)
	dtp->u.p.transfer = unformatted_write;
      else
	{
	  if ((cf & IOPARM_DT_LIST_FORMAT) != 0)
	    dtp->u.p.transfer = list_formatted_write;
	  else
	    dtp->u.p.transfer = formatted_transfer;
	}
    }

  /* Make sure that we don't do a read after a nonadvancing write.  */

  if (read_flag)
    {
      if (dtp->u.p.current_unit->read_bad)
	{
	  generate_error (&dtp->common, ERROR_BAD_OPTION,
			  "Cannot READ after a nonadvancing WRITE");
	  return;
	}
    }
  else
    {
      if (dtp->u.p.advance_status == ADVANCE_YES && !dtp->u.p.seen_dollar)
	dtp->u.p.current_unit->read_bad = 1;
    }

  /* Start the data transfer if we are doing a formatted transfer.  */
  if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED
      && ((cf & (IOPARM_DT_LIST_FORMAT | IOPARM_DT_HAS_NAMELIST_NAME)) == 0)
      && dtp->u.p.ionml == NULL)
    formatted_transfer (dtp, 0, NULL, 0, 0, 1);
}

/* Initialize an array_loop_spec given the array descriptor.  The function
   returns the index of the last element of the array.  */
   
gfc_offset
init_loop_spec (gfc_array_char *desc, array_loop_spec *ls)
{
  int rank = GFC_DESCRIPTOR_RANK(desc);
  int i;
  gfc_offset index; 

  index = 1;
  for (i=0; i<rank; i++)
    {
      ls[i].idx = 1;
      ls[i].start = desc->dim[i].lbound;
      ls[i].end = desc->dim[i].ubound;
      ls[i].step = desc->dim[i].stride;
      
      index += (desc->dim[i].ubound - desc->dim[i].lbound)
                      * desc->dim[i].stride;
    }
  return index;
}

/* Determine the index to the next record in an internal unit array by
   by incrementing through the array_loop_spec.  TODO:  Implement handling
   negative strides. */
   
gfc_offset
next_array_record (st_parameter_dt *dtp, array_loop_spec *ls)
{
  int i, carry;
  gfc_offset index;
  
  carry = 1;
  index = 0;
  
  for (i = 0; i < dtp->u.p.current_unit->rank; i++)
    {
      if (carry)
        {
          ls[i].idx++;
          if (ls[i].idx > ls[i].end)
            {
              ls[i].idx = ls[i].start;
              carry = 1;
            }
          else
            carry = 0;
        }
      index = index + (ls[i].idx - 1) * ls[i].step;
    }
  return index;
}

/* Space to the next record for read mode.  If the file is not
   seekable, we read MAX_READ chunks until we get to the right
   position.  */

#define MAX_READ 4096

static void
next_record_r (st_parameter_dt *dtp)
{
  gfc_offset new, record;
  int bytes_left, rlength, length;
  char *p;

  switch (current_mode (dtp))
    {
    case UNFORMATTED_SEQUENTIAL:
      dtp->u.p.current_unit->bytes_left += sizeof (gfc_offset);	/* Skip over tail */

      /* Fall through...  */

    case FORMATTED_DIRECT:
    case UNFORMATTED_DIRECT:
      if (dtp->u.p.current_unit->bytes_left == 0)
	break;

      if (is_seekable (dtp->u.p.current_unit->s))
	{
	  new = file_position (dtp->u.p.current_unit->s) + dtp->u.p.current_unit->bytes_left;

	  /* Direct access files do not generate END conditions,
	     only I/O errors.  */
	  if (sseek (dtp->u.p.current_unit->s, new) == FAILURE)
	    generate_error (&dtp->common, ERROR_OS, NULL);

	}
      else
	{			/* Seek by reading data.  */
	  while (dtp->u.p.current_unit->bytes_left > 0)
	    {
	      rlength = length = (MAX_READ > dtp->u.p.current_unit->bytes_left) ?
		MAX_READ : dtp->u.p.current_unit->bytes_left;

	      p = salloc_r (dtp->u.p.current_unit->s, &rlength);
	      if (p == NULL)
		{
		  generate_error (&dtp->common, ERROR_OS, NULL);
		  break;
		}

	      dtp->u.p.current_unit->bytes_left -= length;
	    }
	}
      break;

    case FORMATTED_SEQUENTIAL:
      length = 1;
      /* sf_read has already terminated input because of an '\n'  */
      if (dtp->u.p.sf_seen_eor)
	{
	  dtp->u.p.sf_seen_eor = 0;
	  break;
	}

      if (is_internal_unit (dtp))
	{
	  if (is_array_io (dtp))
	    {
	      record = next_array_record (dtp, dtp->u.p.current_unit->ls);

	      /* Now seek to this record.  */
	      record = record * dtp->u.p.current_unit->recl;
	      if (sseek (dtp->u.p.current_unit->s, record) == FAILURE)
		{
		  generate_error (&dtp->common, ERROR_OS, NULL);
		  break;
		}
	      dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	    }
	  else  
	    {
	      bytes_left = (int) dtp->u.p.current_unit->bytes_left;
	      p = salloc_r (dtp->u.p.current_unit->s, &bytes_left);
	      if (p != NULL)
		dtp->u.p.current_unit->bytes_left
		  = dtp->u.p.current_unit->recl;
	    } 
	  break;
	}
      else do
	{
	  p = salloc_r (dtp->u.p.current_unit->s, &length);

	  if (p == NULL)
	    {
	      generate_error (&dtp->common, ERROR_OS, NULL);
	      break;
	    }

	  if (length == 0)
	    {
	      dtp->u.p.current_unit->endfile = AT_ENDFILE;
	      break;
	    }
	}
      while (*p != '\n');

      break;
    }

  if (dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
    test_endfile (dtp->u.p.current_unit);
}


/* Position to the next record in write mode.  */

static void
next_record_w (st_parameter_dt *dtp)
{
  gfc_offset c, m, record;
  int bytes_left, length;
  char *p;

  /* Zero counters for X- and T-editing.  */
  dtp->u.p.max_pos = dtp->u.p.skips = dtp->u.p.pending_spaces = 0;

  switch (current_mode (dtp))
    {
    case FORMATTED_DIRECT:
      if (dtp->u.p.current_unit->bytes_left == 0)
	break;

      length = dtp->u.p.current_unit->bytes_left;
      p = salloc_w (dtp->u.p.current_unit->s, &length);

      if (p == NULL)
	goto io_error;

      memset (p, ' ', dtp->u.p.current_unit->bytes_left);
      if (sfree (dtp->u.p.current_unit->s) == FAILURE)
	goto io_error;
      break;

    case UNFORMATTED_DIRECT:
      if (sfree (dtp->u.p.current_unit->s) == FAILURE)
	goto io_error;
      break;

    case UNFORMATTED_SEQUENTIAL:
      /* Bytes written.  */
      m = dtp->u.p.current_unit->recl - dtp->u.p.current_unit->bytes_left;
      c = file_position (dtp->u.p.current_unit->s);

      length = sizeof (gfc_offset);

      /* Write the length tail.  */

      p = salloc_w (dtp->u.p.current_unit->s, &length);
      if (p == NULL)
	goto io_error;

      /* Only CONVERT_NATIVE and CONVERT_SWAP are valid here.  */
      if (dtp->u.p.current_unit->flags.convert == CONVERT_NATIVE)
	memcpy (p, &m, sizeof (gfc_offset));
      else
	reverse_memcpy (p, &m, sizeof (gfc_offset));
      
      if (sfree (dtp->u.p.current_unit->s) == FAILURE)
	goto io_error;

      /* Seek to the head and overwrite the bogus length with the real
	 length.  */

      p = salloc_w_at (dtp->u.p.current_unit->s, &length, c - m - length);
      if (p == NULL)
	generate_error (&dtp->common, ERROR_OS, NULL);

      /* Only CONVERT_NATIVE and CONVERT_SWAP are valid here.  */
      if (dtp->u.p.current_unit->flags.convert == CONVERT_NATIVE)
	memcpy (p, &m, sizeof (gfc_offset));
      else
	reverse_memcpy (p, &m, sizeof (gfc_offset));
	
      if (sfree (dtp->u.p.current_unit->s) == FAILURE)
	goto io_error;

      /* Seek past the end of the current record.  */

      if (sseek (dtp->u.p.current_unit->s, c + sizeof (gfc_offset)) == FAILURE)
	goto io_error;

      break;

    case FORMATTED_SEQUENTIAL:

      if (dtp->u.p.current_unit->bytes_left == 0)
	break;
	
      if (is_internal_unit (dtp))
	{
	  if (is_array_io (dtp))
	    {
	      bytes_left = (int) dtp->u.p.current_unit->bytes_left;
	      p = salloc_w (dtp->u.p.current_unit->s, &bytes_left);
	      if (p == NULL)
		{
		  generate_error (&dtp->common, ERROR_END, NULL);
		  return;
		}
	      memset(p, ' ', bytes_left);

	      /* Now that the current record has been padded out,
		 determine where the next record in the array is. */

	      record = next_array_record (dtp, dtp->u.p.current_unit->ls);

	      /* Now seek to this record */
	      record = record * dtp->u.p.current_unit->recl;

	      if (sseek (dtp->u.p.current_unit->s, record) == FAILURE)
		goto io_error;

	      dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	    }
	  else
	    {
	      length = 1;
	      p = salloc_w (dtp->u.p.current_unit->s, &length);
	      if (p == NULL)
		goto io_error;
	    }
 	}
      else
	{
#ifdef HAVE_CRLF
	  length = 2;
#else
	  length = 1;
#endif
	  p = salloc_w (dtp->u.p.current_unit->s, &length);
	  if (p)
	    {  /* No new line for internal writes.  */
#ifdef HAVE_CRLF
	      p[0] = '\r';
	      p[1] = '\n';
#else
	      *p = '\n';
#endif
	    }
	  else
	    goto io_error;
	}

      break;

    io_error:
      generate_error (&dtp->common, ERROR_OS, NULL);
      break;
    }
}

/* Position to the next record, which means moving to the end of the
   current record.  This can happen under several different
   conditions.  If the done flag is not set, we get ready to process
   the next record.  */

void
next_record (st_parameter_dt *dtp, int done)
{
  gfc_offset fp; /* File position.  */

  dtp->u.p.current_unit->read_bad = 0;

  if (dtp->u.p.mode == READING)
    next_record_r (dtp);
  else
    next_record_w (dtp);

  /* keep position up to date for INQUIRE */
  dtp->u.p.current_unit->flags.position = POSITION_ASIS;

  dtp->u.p.current_unit->current_record = 0;
  if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT)
   {
    fp = file_position (dtp->u.p.current_unit->s);
    /* Calculate next record, rounding up partial records.  */
    dtp->u.p.current_unit->last_record = (fp + dtp->u.p.current_unit->recl - 1)
				/ dtp->u.p.current_unit->recl;
   }
  else
    dtp->u.p.current_unit->last_record++;

  if (!done)
    pre_position (dtp);
}


/* Finalize the current data transfer.  For a nonadvancing transfer,
   this means advancing to the next record.  For internal units close the
   stream associated with the unit.  */

static void
finalize_transfer (st_parameter_dt *dtp)
{
  jmp_buf eof_jump;
  GFC_INTEGER_4 cf = dtp->common.flags;

  if (dtp->u.p.eor_condition)
    {
      generate_error (&dtp->common, ERROR_EOR, NULL);
      return;
    }

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  if ((dtp->u.p.ionml != NULL)
      && (cf & IOPARM_DT_HAS_NAMELIST_NAME) != 0)
    {
       if ((cf & IOPARM_DT_NAMELIST_READ_MODE) != 0)
	 namelist_read (dtp);
       else
	 namelist_write (dtp);
    }

  dtp->u.p.transfer = NULL;
  if (dtp->u.p.current_unit == NULL)
    return;

  dtp->u.p.eof_jump = &eof_jump;
  if (setjmp (eof_jump))
    {
      generate_error (&dtp->common, ERROR_END, NULL);
      return;
    }

  if ((cf & IOPARM_DT_LIST_FORMAT) != 0 && dtp->u.p.mode == READING)
    finish_list_read (dtp);
  else
    {
      if (dtp->u.p.advance_status == ADVANCE_NO || dtp->u.p.seen_dollar)
	{
	  /* Most systems buffer lines, so force the partial record
	     to be written out.  */
	  flush (dtp->u.p.current_unit->s);
	  dtp->u.p.seen_dollar = 0;
	  return;
	}

      next_record (dtp, 1);
      dtp->u.p.current_unit->current_record = 0;
    }

  sfree (dtp->u.p.current_unit->s);

  if (is_internal_unit (dtp))
    {
      if (is_array_io (dtp) && dtp->u.p.current_unit->ls != NULL)
	free_mem (dtp->u.p.current_unit->ls);
      sclose (dtp->u.p.current_unit->s);
    }
}


/* Transfer function for IOLENGTH. It doesn't actually do any
   data transfer, it just updates the length counter.  */

static void
iolength_transfer (st_parameter_dt *dtp, bt type __attribute__((unused)), 
		   void *dest __attribute__ ((unused)),
		   int kind __attribute__((unused)), 
		   size_t size, size_t nelems)
{
  if ((dtp->common.flags & IOPARM_DT_HAS_IOLENGTH) != 0)
    *dtp->iolength += (GFC_INTEGER_4) size * nelems;
}


/* Initialize the IOLENGTH data transfer. This function is in essence
   a very much simplified version of data_transfer_init(), because it
   doesn't have to deal with units at all.  */

static void
iolength_transfer_init (st_parameter_dt *dtp)
{
  if ((dtp->common.flags & IOPARM_DT_HAS_IOLENGTH) != 0)
    *dtp->iolength = 0;

  memset (&dtp->u.p, 0, sizeof (dtp->u.p));

  /* Set up the subroutine that will handle the transfers.  */

  dtp->u.p.transfer = iolength_transfer;
}


/* Library entry point for the IOLENGTH form of the INQUIRE
   statement. The IOLENGTH form requires no I/O to be performed, but
   it must still be a runtime library call so that we can determine
   the iolength for dynamic arrays and such.  */

extern void st_iolength (st_parameter_dt *);
export_proto(st_iolength);

void
st_iolength (st_parameter_dt *dtp)
{
  library_start (&dtp->common);
  iolength_transfer_init (dtp);
}

extern void st_iolength_done (st_parameter_dt *);
export_proto(st_iolength_done);

void
st_iolength_done (st_parameter_dt *dtp __attribute__((unused)))
{
  free_ionml (dtp);
  if (dtp->u.p.scratch != NULL)
    free_mem (dtp->u.p.scratch);
  library_end ();
}


/* The READ statement.  */

extern void st_read (st_parameter_dt *);
export_proto(st_read);

void
st_read (st_parameter_dt *dtp)
{

  library_start (&dtp->common);

  data_transfer_init (dtp, 1);

  /* Handle complications dealing with the endfile record.  It is
     significant that this is the only place where ERROR_END is
     generated.  Reading an end of file elsewhere is either end of
     record or an I/O error. */

  if (dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
    switch (dtp->u.p.current_unit->endfile)
      {
      case NO_ENDFILE:
	break;

      case AT_ENDFILE:
	if (!is_internal_unit (dtp))
	  {
	    generate_error (&dtp->common, ERROR_END, NULL);
	    dtp->u.p.current_unit->endfile = AFTER_ENDFILE;
	    dtp->u.p.current_unit->current_record = 0;
	  }
	break;

      case AFTER_ENDFILE:
	generate_error (&dtp->common, ERROR_ENDFILE, NULL);
	dtp->u.p.current_unit->current_record = 0;
	break;
      }
}

extern void st_read_done (st_parameter_dt *);
export_proto(st_read_done);

void
st_read_done (st_parameter_dt *dtp)
{
  finalize_transfer (dtp);
  free_format_data (dtp);
  free_ionml (dtp);
  if (dtp->u.p.scratch != NULL)
    free_mem (dtp->u.p.scratch);
  if (dtp->u.p.current_unit != NULL)
    unlock_unit (dtp->u.p.current_unit);
  library_end ();
}

extern void st_write (st_parameter_dt *);
export_proto(st_write);

void
st_write (st_parameter_dt *dtp)
{
  library_start (&dtp->common);
  data_transfer_init (dtp, 0);
}

extern void st_write_done (st_parameter_dt *);
export_proto(st_write_done);

void
st_write_done (st_parameter_dt *dtp)
{
  finalize_transfer (dtp);

  /* Deal with endfile conditions associated with sequential files.  */

  if (dtp->u.p.current_unit != NULL && dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
    switch (dtp->u.p.current_unit->endfile)
      {
      case AT_ENDFILE:		/* Remain at the endfile record.  */
	break;

      case AFTER_ENDFILE:
	dtp->u.p.current_unit->endfile = AT_ENDFILE;	/* Just at it now.  */
	break;

      case NO_ENDFILE:
	if (dtp->u.p.current_unit->current_record > dtp->u.p.current_unit->last_record)
	  {
	    /* Get rid of whatever is after this record.  */
	    if (struncate (dtp->u.p.current_unit->s) == FAILURE)
	      generate_error (&dtp->common, ERROR_OS, NULL);
	  }

	dtp->u.p.current_unit->endfile = AT_ENDFILE;
	break;
      }

  free_format_data (dtp);
  free_ionml (dtp);
  if (dtp->u.p.scratch != NULL)
    free_mem (dtp->u.p.scratch);
  if (dtp->u.p.current_unit != NULL)
    unlock_unit (dtp->u.p.current_unit);
  library_end ();
}

/* Receives the scalar information for namelist objects and stores it
   in a linked list of namelist_info types.  */

extern void st_set_nml_var (st_parameter_dt *dtp, void *, char *,
			    GFC_INTEGER_4, gfc_charlen_type, GFC_INTEGER_4);
export_proto(st_set_nml_var);


void
st_set_nml_var (st_parameter_dt *dtp, void * var_addr, char * var_name,
		GFC_INTEGER_4 len, gfc_charlen_type string_length,
		GFC_INTEGER_4 dtype)
{
  namelist_info *t1 = NULL;
  namelist_info *nml;

  nml = (namelist_info*) get_mem (sizeof (namelist_info));

  nml->mem_pos = var_addr;

  nml->var_name = (char*) get_mem (strlen (var_name) + 1);
  strcpy (nml->var_name, var_name);

  nml->len = (int) len;
  nml->string_length = (index_type) string_length;

  nml->var_rank = (int) (dtype & GFC_DTYPE_RANK_MASK);
  nml->size = (index_type) (dtype >> GFC_DTYPE_SIZE_SHIFT);
  nml->type = (bt) ((dtype & GFC_DTYPE_TYPE_MASK) >> GFC_DTYPE_TYPE_SHIFT);

  if (nml->var_rank > 0)
    {
      nml->dim = (descriptor_dimension*)
		   get_mem (nml->var_rank * sizeof (descriptor_dimension));
      nml->ls = (array_loop_spec*)
		  get_mem (nml->var_rank * sizeof (array_loop_spec));
    }
  else
    {
      nml->dim = NULL;
      nml->ls = NULL;
    }

  nml->next = NULL;

  if ((dtp->common.flags & IOPARM_DT_IONML_SET) == 0)
    {
      dtp->common.flags |= IOPARM_DT_IONML_SET;
      dtp->u.p.ionml = nml;
    }
  else
    {
      for (t1 = dtp->u.p.ionml; t1->next; t1 = t1->next);
      t1->next = nml;
    }
}

/* Store the dimensional information for the namelist object.  */
extern void st_set_nml_var_dim (st_parameter_dt *, GFC_INTEGER_4,
				GFC_INTEGER_4, GFC_INTEGER_4,
				GFC_INTEGER_4);
export_proto(st_set_nml_var_dim);

void
st_set_nml_var_dim (st_parameter_dt *dtp, GFC_INTEGER_4 n_dim,
		    GFC_INTEGER_4 stride, GFC_INTEGER_4 lbound,
		    GFC_INTEGER_4 ubound)
{
  namelist_info * nml;
  int n;

  n = (int)n_dim;

  for (nml = dtp->u.p.ionml; nml->next; nml = nml->next);

  nml->dim[n].stride = (ssize_t)stride;
  nml->dim[n].lbound = (ssize_t)lbound;
  nml->dim[n].ubound = (ssize_t)ubound;
}

/* Reverse memcpy - used for byte swapping.  */

void reverse_memcpy (void *dest, const void *src, size_t n)
{
  char *d, *s;
  size_t i;

  d = (char *) dest;
  s = (char *) src + n - 1;

  /* Write with ascending order - this is likely faster
     on modern architectures because of write combining.  */
  for (i=0; i<n; i++)
      *(d++) = *(s--);
}
