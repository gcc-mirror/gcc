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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


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

extern void transfer_integer (void *, int);
export_proto(transfer_integer);

extern void transfer_real (void *, int);
export_proto(transfer_real);

extern void transfer_logical (void *, int);
export_proto(transfer_logical);

extern void transfer_character (void *, int);
export_proto(transfer_character);

extern void transfer_complex (void *, int);
export_proto(transfer_complex);

gfc_unit *current_unit = NULL;
static int sf_seen_eor = 0;
static int eor_condition = 0;

/* Maximum righthand column written to.  */
static int max_pos;
/* Number of skips + spaces to be done for T and X-editing.  */
static int skips;
/* Number of spaces to be done for T and X-editing.  */
static int pending_spaces;

char scratch[SCRATCH_SIZE] = { };
static char *line_buffer = NULL;

static unit_advance advance_status;

static st_option advance_opt[] = {
  {"yes", ADVANCE_YES},
  {"no", ADVANCE_NO},
  {NULL}
};


static void (*transfer) (bt, void *, int);


typedef enum
{ FORMATTED_SEQUENTIAL, UNFORMATTED_SEQUENTIAL,
  FORMATTED_DIRECT, UNFORMATTED_DIRECT
}
file_mode;


static file_mode
current_mode (void)
{
  file_mode m;

  if (current_unit->flags.access == ACCESS_DIRECT)
    {
      m = current_unit->flags.form == FORM_FORMATTED ?
	FORMATTED_DIRECT : UNFORMATTED_DIRECT;
    }
  else
    {
      m = current_unit->flags.form == FORM_FORMATTED ?
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
read_sf (int *length)
{
  static char data[SCRATCH_SIZE];
  char *base, *p, *q;
  int n, readlen;

  if (*length > SCRATCH_SIZE)
    p = base = line_buffer = get_mem (*length);
  else
    p = base = data;

  /* If we have seen an eor previously, return a length of 0.  The
     caller is responsible for correctly padding the input field.  */
  if (sf_seen_eor)
    {
      *length = 0;
      return base;
    }

  readlen = 1;
  n = 0;

  do
    {
      if (is_internal_unit())
	{
	  /* readlen may be modified inside salloc_r if 
	     is_internal_unit() is true.  */
	  readlen = 1;
	}

      q = salloc_r (current_unit->s, &readlen);
      if (q == NULL)
	break;

      /* If we have a line without a terminating \n, drop through to
	 EOR below.  */
      if (readlen < 1 && n == 0)
	{
	  generate_error (ERROR_END, NULL);
	  return NULL;
	}

      if (readlen < 1 || *q == '\n' || *q == '\r')
	{
	  /* Unexpected end of line.  */

	  /* If we see an EOR during non-advancing I/O, we need to skip
	     the rest of the I/O statement.  Set the corresponding flag.  */
	  if (advance_status == ADVANCE_NO || g.seen_dollar)
	    eor_condition = 1;

	  /* Without padding, terminate the I/O statement without assigning
	     the value.  With padding, the value still needs to be assigned,
	     so we can just continue with a short read.  */
	  if (current_unit->flags.pad == PAD_NO)
	    {
	      generate_error (ERROR_EOR, NULL);
	      return NULL;
	    }

	  current_unit->bytes_left = 0;
	  *length = n;
	  sf_seen_eor = 1;
	  break;
	}

      n++;
      *p++ = *q;
      sf_seen_eor = 0;
    }
  while (n < *length);
  current_unit->bytes_left -= *length;

  if (ioparm.size != NULL)
    *ioparm.size += *length;

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
read_block (int *length)
{
  char *source;
  int nread;

  if (current_unit->flags.form == FORM_FORMATTED &&
      current_unit->flags.access == ACCESS_SEQUENTIAL)
    return read_sf (length);	/* Special case.  */

  if (current_unit->bytes_left < *length)
    {
      if (current_unit->flags.pad == PAD_NO)
	{
	  generate_error (ERROR_EOR, NULL); /* Not enough data left.  */
	  return NULL;
	}

      *length = current_unit->bytes_left;
    }

  current_unit->bytes_left -= *length;

  nread = *length;
  source = salloc_r (current_unit->s, &nread);

  if (ioparm.size != NULL)
    *ioparm.size += nread;

  if (nread != *length)
    {				/* Short read, this shouldn't happen.  */
      if (current_unit->flags.pad == PAD_YES)
	*length = nread;
      else
	{
	  generate_error (ERROR_EOR, NULL);
	  source = NULL;
	}
    }

  return source;
}


/* Function for writing a block of bytes to the current file at the
   current position, advancing the file pointer. We are given a length
   and return a pointer to a buffer that the caller must (completely)
   fill in.  Returns NULL on error.  */

void *
write_block (int length)
{
  char *dest;

  if (!is_internal_unit() && current_unit->bytes_left < length)
    {
      generate_error (ERROR_EOR, NULL);
      return NULL;
    }

  current_unit->bytes_left -= length;
  dest = salloc_w (current_unit->s, &length);

  if (ioparm.size != NULL)
    *ioparm.size += length;

  return dest;
}


/* Master function for unformatted reads.  */

static void
unformatted_read (bt type, void *dest, int length)
{
  void *source;
  int w;

  /* Transfer functions get passed the kind of the entity, so we have
     to fix this for COMPLEX data which are twice the size of their
     kind.  */
  if (type == BT_COMPLEX)
    length *= 2;

  w = length;
  source = read_block (&w);

  if (source != NULL)
    {
      memcpy (dest, source, w);
      if (length != w)
	memset (((char *) dest) + w, ' ', length - w);
    }
}

/* Master function for unformatted writes.  */

static void
unformatted_write (bt type, void *source, int length)
{
  void *dest;

  /* Correction for kind vs. length as in unformatted_read.  */
  if (type == BT_COMPLEX)
    length *= 2;

  dest = write_block (length);
  if (dest != NULL)
    memcpy (dest, source, length);
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
      internal_error ("type_name(): Bad type");
    }

  return p;
}


/* Write a constant string to the output.
   This is complicated because the string can have doubled delimiters
   in it.  The length in the format node is the true length.  */

static void
write_constant_string (fnode * f)
{
  char c, delimiter, *p, *q;
  int length;

  length = f->u.string.length;
  if (length == 0)
    return;

  p = write_block (length);
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
require_type (bt expected, bt actual, fnode * f)
{
  char buffer[100];

  if (actual == expected)
    return 0;

  st_sprintf (buffer, "Expected %s for item %d in formatted transfer, got %s",
	      type_name (expected), g.item_count, type_name (actual));

  format_error (f, buffer);
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
formatted_transfer (bt type, void *p, int len)
{
  int pos, bytes_used;
  fnode *f;
  format_token t;
  int n;
  int consume_data_flag;

  /* Change a complex data item into a pair of reals.  */

  n = (p == NULL) ? 0 : ((type != BT_COMPLEX) ? 1 : 2);
  if (type == BT_COMPLEX)
    type = BT_REAL;

  /* If there's an EOR condition, we simulate finalizing the transfer
     by doing nothing.  */
  if (eor_condition)
    return;

  for (;;)
    {
      /* If reversion has occurred and there is another real data item,
	 then we have to move to the next record.  */
      if (g.reversion_flag && n > 0)
	{
	  g.reversion_flag = 0;
	  next_record (0);
	}

      consume_data_flag = 1 ;
      if (ioparm.library_return != LIBRARY_OK)
	break;

      f = next_format ();
      if (f == NULL)
	return;	      /* No data descriptors left (already raised).  */

      /* Now discharge T, TR and X movements to the right.  This is delayed
	 until a data producing format to supress trailing spaces.  */
      t = f->format;
      if (g.mode == WRITING && skips != 0
	&& ((n>0 && (  t == FMT_I  || t == FMT_B  || t == FMT_O
		    || t == FMT_Z  || t == FMT_F  || t == FMT_E
		    || t == FMT_EN || t == FMT_ES || t == FMT_G
		    || t == FMT_L  || t == FMT_A  || t == FMT_D))
	    || t == FMT_STRING))
	{
	  if (skips > 0)
	    {
	      write_x (skips, pending_spaces);
	      max_pos = (int)(current_unit->recl - current_unit->bytes_left);
	    }
	  if (skips < 0)
	    {
	      move_pos_offset (current_unit->s, skips);
	      current_unit->bytes_left -= (gfc_offset)skips;
	    }
	skips = pending_spaces = 0;
	}

      bytes_used = (int)(current_unit->recl - current_unit->bytes_left);

      switch (t)
	{
	case FMT_I:
	  if (n == 0)
	    goto need_data;
	  if (require_type (BT_INTEGER, type, f))
	    return;

	  if (g.mode == READING)
	    read_decimal (f, p, len);
	  else
	    write_i (f, p, len);

	  break;

	case FMT_B:
	  if (n == 0)
	    goto need_data;
	  if (require_type (BT_INTEGER, type, f))
	    return;

	  if (g.mode == READING)
	    read_radix (f, p, len, 2);
	  else
	    write_b (f, p, len);

	  break;

	case FMT_O:
	  if (n == 0)
	    goto need_data;

	  if (g.mode == READING)
	    read_radix (f, p, len, 8);
	  else
	    write_o (f, p, len);

	  break;

	case FMT_Z:
	  if (n == 0)
	    goto need_data;

	  if (g.mode == READING)
	    read_radix (f, p, len, 16);
	  else
	    write_z (f, p, len);

	  break;

	case FMT_A:
	  if (n == 0)
	    goto need_data;

	  if (g.mode == READING)
	    read_a (f, p, len);
	  else
	    write_a (f, p, len);

	  break;

	case FMT_L:
	  if (n == 0)
	    goto need_data;

	  if (g.mode == READING)
	    read_l (f, p, len);
	  else
	    write_l (f, p, len);

	  break;

	case FMT_D:
	  if (n == 0)
	    goto need_data;
	  if (require_type (BT_REAL, type, f))
	    return;

	  if (g.mode == READING)
	    read_f (f, p, len);
	  else
	    write_d (f, p, len);

	  break;

	case FMT_E:
	  if (n == 0)
	    goto need_data;
	  if (require_type (BT_REAL, type, f))
	    return;

	  if (g.mode == READING)
	    read_f (f, p, len);
	  else
	    write_e (f, p, len);
	  break;

	case FMT_EN:
	  if (n == 0)
	    goto need_data;
	  if (require_type (BT_REAL, type, f))
	    return;

	  if (g.mode == READING)
	    read_f (f, p, len);
	  else
	    write_en (f, p, len);

	  break;

	case FMT_ES:
	  if (n == 0)
	    goto need_data;
	  if (require_type (BT_REAL, type, f))
	    return;

	  if (g.mode == READING)
	    read_f (f, p, len);
	  else
	    write_es (f, p, len);

	  break;

	case FMT_F:
	  if (n == 0)
	    goto need_data;
	  if (require_type (BT_REAL, type, f))
	    return;

	  if (g.mode == READING)
	    read_f (f, p, len);
	  else
	    write_f (f, p, len);

	  break;

	case FMT_G:
	  if (n == 0)
	    goto need_data;
	  if (g.mode == READING)
	    switch (type)
	      {
	      case BT_INTEGER:
		read_decimal (f, p, len);
		break;
	      case BT_LOGICAL:
		read_l (f, p, len);
		break;
	      case BT_CHARACTER:
		read_a (f, p, len);
		break;
	      case BT_REAL:
		read_f (f, p, len);
		break;
	      default:
		goto bad_type;
	      }
	  else
	    switch (type)
	      {
	      case BT_INTEGER:
		write_i (f, p, len);
		break;
	      case BT_LOGICAL:
		write_l (f, p, len);
		break;
	      case BT_CHARACTER:
		write_a (f, p, len);
		break;
	      case BT_REAL:
		write_d (f, p, len);
		break;
	      default:
	      bad_type:
		internal_error ("formatted_transfer(): Bad type");
	      }

	  break;

	case FMT_STRING:
	  consume_data_flag = 0 ;
	  if (g.mode == READING)
	    {
	      format_error (f, "Constant string in input format");
	      return;
	    }
	  write_constant_string (f);
	  break;

	/* Format codes that don't transfer data.  */
	case FMT_X:
	case FMT_TR:
	  consume_data_flag = 0 ;

	  pos = bytes_used + f->u.n + skips;
	  skips = f->u.n + skips;
	  pending_spaces = pos - max_pos;

	  /* Writes occur just before the switch on f->format, above, so that
	     trailing blanks are suppressed.  */
	  if (g.mode == READING)
	    read_x (f->u.n);

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

	  skips = skips + pos - bytes_used;
	  pending_spaces =  pending_spaces + pos - max_pos;

	  if (skips == 0)
	    break;

	  /* Writes occur just before the switch on f->format, above, so that
	     trailing blanks are suppressed.  */
	  if (g.mode == READING)
	    {
	      if (skips > 0)
		read_x (skips);
	      if (skips < 0)
		{
		  move_pos_offset (current_unit->s, skips);
		  current_unit->bytes_left -= skips;
		  skips = pending_spaces = 0;
		}
	    }

	break;

	case FMT_S:
	  consume_data_flag = 0 ;
	  g.sign_status = SIGN_S;
	break;

	case FMT_SS:
	  consume_data_flag = 0 ;
	  g.sign_status = SIGN_SS;
	break;

	case FMT_SP:
	  consume_data_flag = 0 ;
	  g.sign_status = SIGN_SP;
	break;

	case FMT_BN:
	  consume_data_flag = 0 ;
	  g.blank_status = BLANK_NULL;
	break;

	case FMT_BZ:
	  consume_data_flag = 0 ;
	  g.blank_status = BLANK_ZERO;
	break;

	case FMT_P:
	  consume_data_flag = 0 ;
	  g.scale_factor = f->u.k;
	  break;

	case FMT_DOLLAR:
	  consume_data_flag = 0 ;
	  g.seen_dollar = 1;
	  break;

	case FMT_SLASH:
	  consume_data_flag = 0 ;
	  skips = pending_spaces = 0;
	  next_record (0);
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
	  internal_error ("Bad format node");
	}

      /* Free a buffer that we had to allocate during a sequential
	 formatted read of a block that was larger than the static
	 buffer.  */

      if (line_buffer != NULL)
	{
	  free_mem (line_buffer);
	  line_buffer = NULL;
	}

      /* Adjust the item count and data pointer.  */

      if ((consume_data_flag > 0) && (n > 0))
      {
	n--;
	p = ((char *) p) + len;
      }

      if (g.mode == READING)
	skips = 0;

      pos = (int)(current_unit->recl - current_unit->bytes_left);
      max_pos = (max_pos > pos) ? max_pos : pos;

    }

  return;

  /* Come here when we need a data descriptor but don't have one.  We
     push the current format node back onto the input, then return and
     let the user program call us back with the data.  */
 need_data:
  unget_format (f);
}


/* Data transfer entry points.  The type of the data entity is
   implicit in the subroutine call.  This prevents us from having to
   share a common enum with the compiler.  */

void
transfer_integer (void *p, int kind)
{
  g.item_count++;
  if (ioparm.library_return != LIBRARY_OK)
    return;
  transfer (BT_INTEGER, p, kind);
}


void
transfer_real (void *p, int kind)
{
  g.item_count++;
  if (ioparm.library_return != LIBRARY_OK)
    return;
  transfer (BT_REAL, p, kind);
}


void
transfer_logical (void *p, int kind)
{
  g.item_count++;
  if (ioparm.library_return != LIBRARY_OK)
    return;
  transfer (BT_LOGICAL, p, kind);
}


void
transfer_character (void *p, int len)
{
  g.item_count++;
  if (ioparm.library_return != LIBRARY_OK)
    return;
  transfer (BT_CHARACTER, p, len);
}


void
transfer_complex (void *p, int kind)
{
  g.item_count++;
  if (ioparm.library_return != LIBRARY_OK)
    return;
  transfer (BT_COMPLEX, p, kind);
}


/* Preposition a sequential unformatted file while reading.  */

static void
us_read (void)
{
  char *p;
  int n;
  gfc_offset i;

  n = sizeof (gfc_offset);
  p = salloc_r (current_unit->s, &n);

  if (n == 0)
    return;  /* end of file */

  if (p == NULL || n != sizeof (gfc_offset))
    {
      generate_error (ERROR_BAD_US, NULL);
      return;
    }

  memcpy (&i, p, sizeof (gfc_offset));
  current_unit->bytes_left = i;
}


/* Preposition a sequential unformatted file while writing.  This
   amount to writing a bogus length that will be filled in later.  */

static void
us_write (void)
{
  char *p;
  int length;

  length = sizeof (gfc_offset);
  p = salloc_w (current_unit->s, &length);

  if (p == NULL)
    {
      generate_error (ERROR_OS, NULL);
      return;
    }

  memset (p, '\0', sizeof (gfc_offset));	/* Bogus value for now.  */
  if (sfree (current_unit->s) == FAILURE)
    generate_error (ERROR_OS, NULL);

  /* For sequential unformatted, we write until we have more bytes than
     can fit in the record markers. If disk space runs out first, it will
     error on the write.  */
  current_unit->recl = g.max_offset;

  current_unit->bytes_left = current_unit->recl;
}


/* Position to the next record prior to transfer.  We are assumed to
   be before the next record.  We also calculate the bytes in the next
   record.  */

static void
pre_position (void)
{
  if (current_unit->current_record)
    return;			/* Already positioned.  */

  switch (current_mode ())
    {
    case UNFORMATTED_SEQUENTIAL:
      if (g.mode == READING)
	us_read ();
      else
	us_write ();

      break;

    case FORMATTED_SEQUENTIAL:
    case FORMATTED_DIRECT:
    case UNFORMATTED_DIRECT:
      current_unit->bytes_left = current_unit->recl;
      break;
    }

  current_unit->current_record = 1;
}


/* Initialize things for a data transfer.  This code is common for
   both reading and writing.  */

static void
data_transfer_init (int read_flag)
{
  unit_flags u_flags;  /* Used for creating a unit if needed.  */

  g.mode = read_flag ? READING : WRITING;

  if (ioparm.size != NULL)
    *ioparm.size = 0;		/* Initialize the count.  */

  current_unit = get_unit (read_flag);
  if (current_unit == NULL)
  {  /* Open the unit with some default flags.  */
     if (ioparm.unit < 0)
     {
       generate_error (ERROR_BAD_OPTION, "Bad unit number in OPEN statement");
       library_end ();
       return;
     }
     memset (&u_flags, '\0', sizeof (u_flags));
     u_flags.access = ACCESS_SEQUENTIAL;
     u_flags.action = ACTION_READWRITE;
     /* Is it unformatted?  */
     if (ioparm.format == NULL && !ioparm.list_format)
       u_flags.form = FORM_UNFORMATTED;
     else
       u_flags.form = FORM_UNSPECIFIED;
     u_flags.delim = DELIM_UNSPECIFIED;
     u_flags.blank = BLANK_UNSPECIFIED;
     u_flags.pad = PAD_UNSPECIFIED;
     u_flags.status = STATUS_UNKNOWN;
     new_unit(&u_flags);
     current_unit = get_unit (read_flag);
  }

  if (current_unit == NULL)
    return;

  if (is_internal_unit())
    {
      current_unit->recl = file_length(current_unit->s);
      if (g.mode==WRITING)
	empty_internal_buffer (current_unit->s);
      else
        current_unit->bytes_left = current_unit->recl;	
    }

  /* Check the action.  */

  if (read_flag && current_unit->flags.action == ACTION_WRITE)
    generate_error (ERROR_BAD_ACTION,
		    "Cannot read from file opened for WRITE");

  if (!read_flag && current_unit->flags.action == ACTION_READ)
    generate_error (ERROR_BAD_ACTION, "Cannot write to file opened for READ");

  if (ioparm.library_return != LIBRARY_OK)
    return;

  /* Check the format.  */

  if (ioparm.format)
    parse_format ();

  if (ioparm.library_return != LIBRARY_OK)
    return;

  if (current_unit->flags.form == FORM_UNFORMATTED
      && (ioparm.format != NULL || ioparm.list_format))
    generate_error (ERROR_OPTION_CONFLICT,
		    "Format present for UNFORMATTED data transfer");

  if (ioparm.namelist_name != NULL && ionml != NULL)
     {
	if(ioparm.format != NULL)
	   generate_error (ERROR_OPTION_CONFLICT,
		    "A format cannot be specified with a namelist");
     }
  else if (current_unit->flags.form == FORM_FORMATTED &&
	   ioparm.format == NULL && !ioparm.list_format)
    generate_error (ERROR_OPTION_CONFLICT,
		    "Missing format for FORMATTED data transfer");


  if (is_internal_unit () && current_unit->flags.form == FORM_UNFORMATTED)
    generate_error (ERROR_OPTION_CONFLICT,
		    "Internal file cannot be accessed by UNFORMATTED data transfer");

  /* Check the record number.  */

  if (current_unit->flags.access == ACCESS_DIRECT && ioparm.rec == 0)
    {
      generate_error (ERROR_MISSING_OPTION,
		      "Direct access data transfer requires record number");
      return;
    }

  if (current_unit->flags.access == ACCESS_SEQUENTIAL && ioparm.rec != 0)
    {
      generate_error (ERROR_OPTION_CONFLICT,
		      "Record number not allowed for sequential access data transfer");
      return;
    }

  /* Process the ADVANCE option.  */

  advance_status = (ioparm.advance == NULL) ? ADVANCE_UNSPECIFIED :
    find_option (ioparm.advance, ioparm.advance_len, advance_opt,
		 "Bad ADVANCE parameter in data transfer statement");

  if (advance_status != ADVANCE_UNSPECIFIED)
    {
      if (current_unit->flags.access == ACCESS_DIRECT)
	generate_error (ERROR_OPTION_CONFLICT,
			"ADVANCE specification conflicts with sequential access");

      if (is_internal_unit ())
	generate_error (ERROR_OPTION_CONFLICT,
			"ADVANCE specification conflicts with internal file");

      if (ioparm.format == NULL || ioparm.list_format)
	generate_error (ERROR_OPTION_CONFLICT,
			"ADVANCE specification requires an explicit format");
    }

  if (read_flag)
    {
      if (ioparm.eor != 0 && advance_status != ADVANCE_NO)
	generate_error (ERROR_MISSING_OPTION,
			"EOR specification requires an ADVANCE specification of NO");

      if (ioparm.size != NULL && advance_status != ADVANCE_NO)
	generate_error (ERROR_MISSING_OPTION,
			"SIZE specification requires an ADVANCE specification of NO");

    }
  else
    {				/* Write constraints.  */
      if (ioparm.end != 0)
	generate_error (ERROR_OPTION_CONFLICT,
			"END specification cannot appear in a write statement");

      if (ioparm.eor != 0)
	generate_error (ERROR_OPTION_CONFLICT,
			"EOR specification cannot appear in a write statement");

      if (ioparm.size != 0)
	generate_error (ERROR_OPTION_CONFLICT,
			"SIZE specification cannot appear in a write statement");
    }

  if (advance_status == ADVANCE_UNSPECIFIED)
    advance_status = ADVANCE_YES;
  if (ioparm.library_return != LIBRARY_OK)
    return;

  /* Sanity checks on the record number.  */

  if (ioparm.rec)
    {
      if (ioparm.rec <= 0)
	{
	  generate_error (ERROR_BAD_OPTION, "Record number must be positive");
	  return;
	}

      if (ioparm.rec >= current_unit->maxrec)
	{
	  generate_error (ERROR_BAD_OPTION, "Record number too large");
	  return;
	}

      /* Check to see if we might be reading what we wrote before  */

      if (g.mode == READING && current_unit->mode  == WRITING)
	 flush(current_unit->s);

      /* Check whether the record exists to be read.  Only
	 a partial record needs to exist.  */

      if (g.mode == READING && (ioparm.rec -1)
	  * current_unit->recl >= file_length (current_unit->s))
	{
	  generate_error (ERROR_BAD_OPTION, "Non-existing record number");
	  return;
	}

      /* Position the file.  */
      if (sseek (current_unit->s,
	       (ioparm.rec - 1) * current_unit->recl) == FAILURE)
	{
	  generate_error (ERROR_OS, NULL);
	  return;
	}
    }

  current_unit->mode = g.mode;

  /* Set the initial value of flags.  */

  g.blank_status = current_unit->flags.blank;
  g.sign_status = SIGN_S;
  g.scale_factor = 0;
  g.seen_dollar = 0;
  g.first_item = 1;
  g.item_count = 0;
  sf_seen_eor = 0;
  eor_condition = 0;

  pre_position ();

  /* Set up the subroutine that will handle the transfers.  */

  if (read_flag)
    {
      if (current_unit->flags.form == FORM_UNFORMATTED)
	transfer = unformatted_read;
      else
	{
	  if (ioparm.list_format)
	    {
	       transfer = list_formatted_read;
	       init_at_eol();
	    }
	  else
	    transfer = formatted_transfer;
	}
    }
  else
    {
      if (current_unit->flags.form == FORM_UNFORMATTED)
	transfer = unformatted_write;
      else
	{
	  if (ioparm.list_format)
	    transfer = list_formatted_write;
	  else
	    transfer = formatted_transfer;
	}
    }

  /* Make sure that we don't do a read after a nonadvancing write.  */

  if (read_flag)
    {
      if (current_unit->read_bad)
	{
	  generate_error (ERROR_BAD_OPTION,
			  "Cannot READ after a nonadvancing WRITE");
	  return;
	}
    }
  else
    {
      if (advance_status == ADVANCE_YES && !g.seen_dollar)
	current_unit->read_bad = 1;
    }

  /* Reset counters for T and X-editing.  */
  max_pos = skips = pending_spaces = 0;

  /* Start the data transfer if we are doing a formatted transfer.  */
  if (current_unit->flags.form == FORM_FORMATTED && !ioparm.list_format
      && ioparm.namelist_name == NULL && ionml == NULL)
    formatted_transfer (0, NULL, 0);
}


/* Space to the next record for read mode.  If the file is not
   seekable, we read MAX_READ chunks until we get to the right
   position.  */

#define MAX_READ 4096

static void
next_record_r (int done)
{
  int rlength, length;
  gfc_offset new;
  char *p;

  switch (current_mode ())
    {
    case UNFORMATTED_SEQUENTIAL:
      current_unit->bytes_left += sizeof (gfc_offset);	/* Skip over tail */

      /* Fall through...  */

    case FORMATTED_DIRECT:
    case UNFORMATTED_DIRECT:
      if (current_unit->bytes_left == 0)
	break;

      if (is_seekable (current_unit->s))
	{
	  new = file_position (current_unit->s) + current_unit->bytes_left;

	  /* Direct access files do not generate END conditions, 
	     only I/O errors.  */
	  if (sseek (current_unit->s, new) == FAILURE)
	    generate_error (ERROR_OS, NULL);

	}
      else
	{			/* Seek by reading data.  */
	  while (current_unit->bytes_left > 0)
	    {
	      rlength = length = (MAX_READ > current_unit->bytes_left) ?
		MAX_READ : current_unit->bytes_left;

	      p = salloc_r (current_unit->s, &rlength);
	      if (p == NULL)
		{
		  generate_error (ERROR_OS, NULL);
		  break;
		}

	      current_unit->bytes_left -= length;
	    }
	}
      break;

    case FORMATTED_SEQUENTIAL:
      length = 1;
      /* sf_read has already terminated input because of an '\n'  */
      if (sf_seen_eor) 
	{
	  sf_seen_eor=0;
	  break;
	}

      do
	{
	  p = salloc_r (current_unit->s, &length);

	  /* In case of internal file, there may not be any '\n'.  */
	  if (is_internal_unit() && p == NULL)
	    {
	       break;
	    }

	  if (p == NULL)
	    {
	      generate_error (ERROR_OS, NULL);
	      break;
	    }

	  if (length == 0)
	    {
	      current_unit->endfile = AT_ENDFILE;
	      break;
	    }
	}
      while (*p != '\n');

      break;
    }

  if (current_unit->flags.access == ACCESS_SEQUENTIAL)
    test_endfile (current_unit);
}


/* Position to the next record in write mode.  */

static void
next_record_w (int done)
{
  gfc_offset c, m;
  int length;
  char *p;

  /* Zero counters for X- and T-editing.  */
  max_pos = skips = pending_spaces = 0;

  switch (current_mode ())
    {
    case FORMATTED_DIRECT:
      if (current_unit->bytes_left == 0)
	break;

      length = current_unit->bytes_left;
      p = salloc_w (current_unit->s, &length);

      if (p == NULL)
	goto io_error;

      memset (p, ' ', current_unit->bytes_left);
      if (sfree (current_unit->s) == FAILURE)
	goto io_error;
      break;

    case UNFORMATTED_DIRECT:
      if (sfree (current_unit->s) == FAILURE)
	goto io_error;
      break;

    case UNFORMATTED_SEQUENTIAL:
      m = current_unit->recl - current_unit->bytes_left; /* Bytes written.  */
      c = file_position (current_unit->s);

      length = sizeof (gfc_offset);

      /* Write the length tail.  */

      p = salloc_w (current_unit->s, &length);
      if (p == NULL)
	goto io_error;

      memcpy (p, &m, sizeof (gfc_offset));
      if (sfree (current_unit->s) == FAILURE)
	goto io_error;

      /* Seek to the head and overwrite the bogus length with the real
	 length.  */

      p = salloc_w_at (current_unit->s, &length, c - m - length);
      if (p == NULL)
	generate_error (ERROR_OS, NULL);

      memcpy (p, &m, sizeof (gfc_offset));
      if (sfree (current_unit->s) == FAILURE)
	goto io_error;

      /* Seek past the end of the current record.  */

      if (sseek (current_unit->s, c + sizeof (gfc_offset)) == FAILURE)
	goto io_error;

      break;

    case FORMATTED_SEQUENTIAL:
#ifdef HAVE_CRLF
      length = 2;
#else
      length = 1;
#endif
      p = salloc_w (current_unit->s, &length);

      if (!is_internal_unit())
	{
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

      if (sfree (current_unit->s) == FAILURE)
 	goto io_error;

      break;

    io_error:
      generate_error (ERROR_OS, NULL);
      break;
    }
}


/* Position to the next record, which means moving to the end of the
   current record.  This can happen under several different
   conditions.  If the done flag is not set, we get ready to process
   the next record.  */

void
next_record (int done)
{
  gfc_offset fp; /* File position.  */

  current_unit->read_bad = 0;

  if (g.mode == READING)
    next_record_r (done);
  else
    next_record_w (done);

  /* keep position up to date for INQUIRE */
  current_unit->flags.position = POSITION_ASIS;

  current_unit->current_record = 0;
  if (current_unit->flags.access == ACCESS_DIRECT)
   {
    fp = file_position (current_unit->s);
    /* Calculate next record, rounding up partial records.  */
    current_unit->last_record = (fp + current_unit->recl - 1)
				/ current_unit->recl;
   }
  else
    current_unit->last_record++;

  if (!done)
    pre_position ();
}


/* Finalize the current data transfer.  For a nonadvancing transfer,
   this means advancing to the next record.  For internal units close the
   steam associated with the unit.  */

static void
finalize_transfer (void)
{

  if (eor_condition)
    {
      generate_error (ERROR_EOR, NULL);
      return;
    }

  if (ioparm.library_return != LIBRARY_OK)
    return;

  if ((ionml != NULL) && (ioparm.namelist_name != NULL))
    {
       if (ioparm.namelist_read_mode)
	 namelist_read();
       else
	 namelist_write();
    }

  transfer = NULL;
  if (current_unit == NULL)
    return;

  if (setjmp (g.eof_jump))
    {
      generate_error (ERROR_END, NULL);
      return;
    }

  if (ioparm.list_format && g.mode == READING)
    finish_list_read ();
  else
    {
      free_fnodes ();

      if (advance_status == ADVANCE_NO || g.seen_dollar)
	{
	  /* Most systems buffer lines, so force the partial record
	     to be written out.  */
	  flush (current_unit->s);
	  g.seen_dollar = 0;
	  return;
	}

      next_record (1);
      current_unit->current_record = 0;
    }

  sfree (current_unit->s);

  if (is_internal_unit ())
    sclose (current_unit->s);
}


/* Transfer function for IOLENGTH. It doesn't actually do any
   data transfer, it just updates the length counter.  */

static void
iolength_transfer (bt type, void *dest, int len)
{
  if (ioparm.iolength != NULL)
    {
      if (type == BT_COMPLEX)
	*ioparm.iolength += 2*len;
      else
	*ioparm.iolength += len;
    }
}


/* Initialize the IOLENGTH data transfer. This function is in essence
   a very much simplified version of data_transfer_init(), because it
   doesn't have to deal with units at all.  */

static void
iolength_transfer_init (void)
{
  if (ioparm.iolength != NULL)
    *ioparm.iolength = 0;

  g.item_count = 0;

  /* Set up the subroutine that will handle the transfers.  */

  transfer = iolength_transfer;
}


/* Library entry point for the IOLENGTH form of the INQUIRE
   statement. The IOLENGTH form requires no I/O to be performed, but
   it must still be a runtime library call so that we can determine
   the iolength for dynamic arrays and such.  */

extern void st_iolength (void);
export_proto(st_iolength);

void
st_iolength (void)
{
  library_start ();
  iolength_transfer_init ();
}

extern void st_iolength_done (void);
export_proto(st_iolength_done);

void
st_iolength_done (void)
{
  library_end ();
}


/* The READ statement.  */

extern void st_read (void);
export_proto(st_read);

void
st_read (void)
{

  library_start ();

  data_transfer_init (1);

  /* Handle complications dealing with the endfile record.  It is
     significant that this is the only place where ERROR_END is
     generated.  Reading an end of file elsewhere is either end of
     record or an I/O error. */

  if (current_unit->flags.access == ACCESS_SEQUENTIAL)
    switch (current_unit->endfile)
      {
      case NO_ENDFILE:
	break;

      case AT_ENDFILE:
	if (!is_internal_unit())
	  {
	    generate_error (ERROR_END, NULL);
	    current_unit->endfile = AFTER_ENDFILE;
	  }
	break;

      case AFTER_ENDFILE:
	generate_error (ERROR_ENDFILE, NULL);
	break;
      }
}

extern void st_read_done (void);
export_proto(st_read_done);

void
st_read_done (void)
{
  finalize_transfer ();
  library_end ();
}

extern void st_write (void);
export_proto(st_write);

void
st_write (void)
{

  library_start ();
  data_transfer_init (0);
}

extern void st_write_done (void);
export_proto(st_write_done);

void
st_write_done (void)
{
  finalize_transfer ();

  /* Deal with endfile conditions associated with sequential files.  */

  if (current_unit != NULL && current_unit->flags.access == ACCESS_SEQUENTIAL)
    switch (current_unit->endfile)
      {
      case AT_ENDFILE:		/* Remain at the endfile record.  */
	break;

      case AFTER_ENDFILE:
	current_unit->endfile = AT_ENDFILE;	/* Just at it now.  */
	break;

      case NO_ENDFILE:
	if (current_unit->current_record > current_unit->last_record)
	  {
	    /* Get rid of whatever is after this record.  */
	    if (struncate (current_unit->s) == FAILURE)
	      generate_error (ERROR_OS, NULL);
	  }

	current_unit->endfile = AT_ENDFILE;
	break;
      }

  library_end ();
}

/* Receives the scalar information for namelist objects and stores it
   in a linked list of namelist_info types.  */

void
st_set_nml_var (void * var_addr, char * var_name, GFC_INTEGER_4 len,
		gfc_charlen_type string_length, GFC_INTEGER_4 dtype)
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
      nml->ls = (nml_loop_spec*)
		  get_mem (nml->var_rank * sizeof (nml_loop_spec));
    }
  else
    {
      nml->dim = NULL;
      nml->ls = NULL;
    }

  nml->next = NULL;

  if (ionml == NULL)
    ionml = nml;
  else
    {
      for (t1 = ionml; t1->next; t1 = t1->next);
      t1->next = nml;
    }
  return;
}

/* Store the dimensional information for the namelist object.  */

void
st_set_nml_var_dim (GFC_INTEGER_4 n_dim, GFC_INTEGER_4 stride,
		    GFC_INTEGER_4 lbound, GFC_INTEGER_4 ubound)
{
  namelist_info * nml;
  int n;

  n = (int)n_dim;

  for (nml = ionml; nml->next; nml = nml->next);

  nml->dim[n].stride = (ssize_t)stride;
  nml->dim[n].lbound = (ssize_t)lbound;
  nml->dim[n].ubound = (ssize_t)ubound;
}

extern void st_set_nml_var (void * ,char * ,
			    GFC_INTEGER_4 ,gfc_charlen_type ,GFC_INTEGER_4);
export_proto(st_set_nml_var);

extern void st_set_nml_var_dim (GFC_INTEGER_4, GFC_INTEGER_4,
				GFC_INTEGER_4 ,GFC_INTEGER_4);
export_proto(st_set_nml_var_dim);

