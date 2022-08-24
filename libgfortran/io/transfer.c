/* Copyright (C) 2002-2022 Free Software Foundation, Inc.
   Contributed by Andy Vaught
   Namelist transfer functions contributed by Paul Thomas
   F2003 I/O support contributed by Jerry DeLisle

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


/* transfer.c -- Top level handling of data transfer statements.  */

#include "io.h"
#include "fbuf.h"
#include "format.h"
#include "unix.h"
#include "async.h"
#include <string.h>
#include <errno.h>


/* Calling conventions:  Data transfer statements are unlike other
   library calls in that they extend over several calls.

   The first call is always a call to st_read() or st_write().  These
   subroutines return no status unless a namelist read or write is
   being done, in which case there is the usual status.  No further
   calls are necessary in this case.

   For other sorts of data transfer, there are zero or more data
   transfer statement that depend on the format of the data transfer
   statement. For READ (and for backwards compatibily: for WRITE), one has

      transfer_integer
      transfer_logical
      transfer_character
      transfer_character_wide
      transfer_real
      transfer_complex
      transfer_real128
      transfer_complex128

    and for WRITE

      transfer_integer_write
      transfer_logical_write
      transfer_character_write
      transfer_character_wide_write
      transfer_real_write
      transfer_complex_write
      transfer_real128_write
      transfer_complex128_write

    These subroutines do not return status. The *128 functions
    are in the file transfer128.c.

    The last call is a call to st_[read|write]_done().  While
    something can easily go wrong with the initial st_read() or
    st_write(), an error inhibits any data from actually being
    transferred.  */

extern void transfer_integer (st_parameter_dt *, void *, int);
export_proto(transfer_integer);

extern void transfer_integer_write (st_parameter_dt *, void *, int);
export_proto(transfer_integer_write);

extern void transfer_real (st_parameter_dt *, void *, int);
export_proto(transfer_real);

extern void transfer_real_write (st_parameter_dt *, void *, int);
export_proto(transfer_real_write);

extern void transfer_logical (st_parameter_dt *, void *, int);
export_proto(transfer_logical);

extern void transfer_logical_write (st_parameter_dt *, void *, int);
export_proto(transfer_logical_write);

extern void transfer_character (st_parameter_dt *, void *, gfc_charlen_type);
export_proto(transfer_character);

extern void transfer_character_write (st_parameter_dt *, void *, gfc_charlen_type);
export_proto(transfer_character_write);

extern void transfer_character_wide (st_parameter_dt *, void *, gfc_charlen_type, int);
export_proto(transfer_character_wide);

extern void transfer_character_wide_write (st_parameter_dt *,
					   void *, gfc_charlen_type, int);
export_proto(transfer_character_wide_write);

extern void transfer_complex (st_parameter_dt *, void *, int);
export_proto(transfer_complex);

extern void transfer_complex_write (st_parameter_dt *, void *, int);
export_proto(transfer_complex_write);

extern void transfer_array (st_parameter_dt *, gfc_array_char *, int,
			    gfc_charlen_type);
export_proto(transfer_array);

extern void transfer_array_write (st_parameter_dt *, gfc_array_char *, int,
			    gfc_charlen_type);
export_proto(transfer_array_write);

/* User defined derived type input/output.  */
extern void
transfer_derived (st_parameter_dt *dtp, void *dtio_source, void *dtio_proc);
export_proto(transfer_derived);

extern void
transfer_derived_write (st_parameter_dt *dtp, void *dtio_source, void *dtio_proc);
export_proto(transfer_derived_write);

static void us_read (st_parameter_dt *, int);
static void us_write (st_parameter_dt *, int);
static void next_record_r_unf (st_parameter_dt *, int);
static void next_record_w_unf (st_parameter_dt *, int);

static const st_option advance_opt[] = {
  {"yes", ADVANCE_YES},
  {"no", ADVANCE_NO},
  {NULL, 0}
};


static const st_option decimal_opt[] = {
  {"point", DECIMAL_POINT},
  {"comma", DECIMAL_COMMA},
  {NULL, 0}
};

static const st_option round_opt[] = {
  {"up", ROUND_UP},
  {"down", ROUND_DOWN},
  {"zero", ROUND_ZERO},
  {"nearest", ROUND_NEAREST},
  {"compatible", ROUND_COMPATIBLE},
  {"processor_defined", ROUND_PROCDEFINED},
  {NULL, 0}
};


static const st_option sign_opt[] = {
  {"plus", SIGN_SP},
  {"suppress", SIGN_SS},
  {"processor_defined", SIGN_S},
  {NULL, 0}
};

static const st_option blank_opt[] = {
  {"null", BLANK_NULL},
  {"zero", BLANK_ZERO},
  {NULL, 0}
};

static const st_option delim_opt[] = {
  {"apostrophe", DELIM_APOSTROPHE},
  {"quote", DELIM_QUOTE},
  {"none", DELIM_NONE},
  {NULL, 0}
};

static const st_option pad_opt[] = {
  {"yes", PAD_YES},
  {"no", PAD_NO},
  {NULL, 0}
};

static const st_option async_opt[] = {
  {"yes", ASYNC_YES},
  {"no", ASYNC_NO},
  {NULL, 0}
};

typedef enum
{ FORMATTED_SEQUENTIAL, UNFORMATTED_SEQUENTIAL,
  FORMATTED_DIRECT, UNFORMATTED_DIRECT, FORMATTED_STREAM,
  UNFORMATTED_STREAM, FORMATTED_UNSPECIFIED
}
file_mode;


static file_mode
current_mode (st_parameter_dt *dtp)
{
  file_mode m;

  m = FORMATTED_UNSPECIFIED;

  if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT)
    {
      m = dtp->u.p.current_unit->flags.form == FORM_FORMATTED ?
	FORMATTED_DIRECT : UNFORMATTED_DIRECT;
    }
  else if (dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
    {
      m = dtp->u.p.current_unit->flags.form == FORM_FORMATTED ?
	FORMATTED_SEQUENTIAL : UNFORMATTED_SEQUENTIAL;
    }
  else if (dtp->u.p.current_unit->flags.access == ACCESS_STREAM)
    {
      m = dtp->u.p.current_unit->flags.form == FORM_FORMATTED ?
	FORMATTED_STREAM : UNFORMATTED_STREAM;
    }

  return m;
}


/* Mid level data transfer statements.  */

/* Read sequential file - internal unit  */

static char *
read_sf_internal (st_parameter_dt *dtp, size_t *length)
{
  static char *empty_string[0];
  char *base = NULL;
  size_t lorig;

  /* Zero size array gives internal unit len of 0.  Nothing to read. */
  if (dtp->internal_unit_len == 0
      && dtp->u.p.current_unit->pad_status == PAD_NO)
    hit_eof (dtp);

  /* There are some cases with mixed DTIO where we have read a character
     and saved it in the last character buffer, so we need to backup.  */
  if (unlikely (dtp->u.p.current_unit->child_dtio > 0 &&
		dtp->u.p.current_unit->last_char != EOF - 1))
    {
      dtp->u.p.current_unit->last_char = EOF - 1;
      sseek (dtp->u.p.current_unit->s, -1, SEEK_CUR);
    }

  /* To support legacy code we have to scan the input string one byte
     at a time because we don't know where an early comma may be and the
     requested length could go past the end of a comma shortened
     string.  We only do this if -std=legacy was given at compile
     time.  We also do not support this on kind=4 strings.  */
  if (unlikely(compile_options.warn_std == 0)) // the slow legacy way.
    {
      size_t n;
      size_t tmp = 1;
      char *q;

      /* If we have seen an eor previously, return a length of 0.  The
	 caller is responsible for correctly padding the input field.  */
      if (dtp->u.p.sf_seen_eor)
	{
	  *length = 0;
	  /* Just return something that isn't a NULL pointer, otherwise the
	     caller thinks an error occurred.  */
	  return (char*) empty_string;
	}

      /* Get the first character of the string to establish the base
	 address and check for comma or end-of-record condition.  */
      base = mem_alloc_r (dtp->u.p.current_unit->s, &tmp);
      if (tmp == 0)
	{
	  dtp->u.p.sf_seen_eor = 1;
	  *length = 0;
	  return (char*) empty_string;
	}
      if (*base == ',')
	{
	  dtp->u.p.current_unit->bytes_left--;
	  *length = 0;
	  return (char*) empty_string;
	}

      /* Now we scan the rest and deal with either an end-of-file
         condition or a comma, as needed.  */
      for (n = 1; n < *length; n++)
	{
	  q = mem_alloc_r (dtp->u.p.current_unit->s, &tmp);
	  if (tmp == 0)
	    {
	      hit_eof (dtp);
	      return NULL;
	    }
	  if (*q == ',')
	    {
	      dtp->u.p.current_unit->bytes_left -= n;
	      *length = n;
	      break;
	    }
	}
    }
  else // the fast way
    {
      lorig = *length;
      if (is_char4_unit(dtp))
	{
	  gfc_char4_t *p = (gfc_char4_t *) mem_alloc_r4 (dtp->u.p.current_unit->s,
			    length);
	  base = fbuf_alloc (dtp->u.p.current_unit, lorig);
	  for (size_t i = 0; i < *length; i++, p++)
	    base[i] = *p > 255 ? '?' : (unsigned char) *p;
	}
      else
	base = mem_alloc_r (dtp->u.p.current_unit->s, length);

      if (unlikely (lorig > *length))
	{
	  hit_eof (dtp);
	  return NULL;
	}
    }

  dtp->u.p.current_unit->bytes_left -= *length;

  if (((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0) ||
      dtp->u.p.current_unit->has_size)
    dtp->u.p.current_unit->size_used += (GFC_IO_INT) *length;

  return base;

}

/* When reading sequential formatted records we have a problem.  We
   don't know how long the line is until we read the trailing newline,
   and we don't want to read too much.  If we read too much, we might
   have to do a physical seek backwards depending on how much data is
   present, and devices like terminals aren't seekable and would cause
   an I/O error.

   Given this, the solution is to read a byte at a time, stopping if
   we hit the newline.  For small allocations, we use a static buffer.
   For larger allocations, we are forced to allocate memory on the
   heap.  Hopefully this won't happen very often.  */

/* Read sequential file - external unit */

static char *
read_sf (st_parameter_dt *dtp, size_t *length)
{
  static char *empty_string[0];
  size_t lorig, n;
  int q, q2;
  int seen_comma;

  /* If we have seen an eor previously, return a length of 0.  The
     caller is responsible for correctly padding the input field.  */
  if (dtp->u.p.sf_seen_eor)
    {
      *length = 0;
      /* Just return something that isn't a NULL pointer, otherwise the
         caller thinks an error occurred.  */
      return (char*) empty_string;
    }

  /* There are some cases with mixed DTIO where we have read a character
     and saved it in the last character buffer, so we need to backup.  */
  if (unlikely (dtp->u.p.current_unit->child_dtio > 0 &&
		dtp->u.p.current_unit->last_char != EOF - 1))
    {
      dtp->u.p.current_unit->last_char = EOF - 1;
      fbuf_seek (dtp->u.p.current_unit, -1, SEEK_CUR);
    }

  n = seen_comma = 0;

  /* Read data into format buffer and scan through it.  */
  lorig = *length;

  while (n < *length)
    {
      q = fbuf_getc (dtp->u.p.current_unit);
      if (q == EOF)
	break;
      else if (dtp->u.p.current_unit->flags.cc != CC_NONE
	       && (q == '\n' || q == '\r'))
	{
	  /* Unexpected end of line. Set the position.  */
	  dtp->u.p.sf_seen_eor = 1;

	  /* If we see an EOR during non-advancing I/O, we need to skip
	     the rest of the I/O statement.  Set the corresponding flag.  */
	  if (dtp->u.p.advance_status == ADVANCE_NO || dtp->u.p.seen_dollar)
	    dtp->u.p.eor_condition = 1;

	  /* If we encounter a CR, it might be a CRLF.  */
	  if (q == '\r') /* Probably a CRLF */
	    {
	      /* See if there is an LF.  */
	      q2 = fbuf_getc (dtp->u.p.current_unit);
	      if (q2 == '\n')
		dtp->u.p.sf_seen_eor = 2;
	      else if (q2 != EOF) /* Oops, seek back.  */
		fbuf_seek (dtp->u.p.current_unit, -1, SEEK_CUR);
	    }

	  /* Without padding, terminate the I/O statement without assigning
	     the value.  With padding, the value still needs to be assigned,
	     so we can just continue with a short read.  */
	  if (dtp->u.p.current_unit->pad_status == PAD_NO)
	    {
	      generate_error (&dtp->common, LIBERROR_EOR, NULL);
	      return NULL;
	    }

	  *length = n;
	  goto done;
	}
      /*  Short circuit the read if a comma is found during numeric input.
	  The flag is set to zero during character reads so that commas in
	  strings are not ignored  */
      else if (q == ',')
	if (dtp->u.p.sf_read_comma == 1)
	  {
            seen_comma = 1;
	    notify_std (&dtp->common, GFC_STD_GNU,
			"Comma in formatted numeric read.");
	    break;
	  }
      n++;
    }

  *length = n;

  /* A short read implies we hit EOF, unless we hit EOR, a comma, or
     some other stuff. Set the relevant flags.  */
  if (lorig > *length && !dtp->u.p.sf_seen_eor && !seen_comma)
    {
      if (n > 0)
        {
	  if (dtp->u.p.advance_status == ADVANCE_NO)
	    {
	      if (dtp->u.p.current_unit->pad_status == PAD_NO)
	        {
		  hit_eof (dtp);
		  return NULL;
		}
	      else
		dtp->u.p.eor_condition = 1;
	    }
	  else
	    dtp->u.p.at_eof = 1;
	}
      else if (dtp->u.p.advance_status == ADVANCE_NO
	       || dtp->u.p.current_unit->pad_status == PAD_NO
	       || dtp->u.p.current_unit->bytes_left
		    == dtp->u.p.current_unit->recl)
	{
	  hit_eof (dtp);
	  return NULL;
	}
    }

 done:

  dtp->u.p.current_unit->bytes_left -= n;

  if (((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0) ||
      dtp->u.p.current_unit->has_size)
    dtp->u.p.current_unit->size_used += (GFC_IO_INT) n;

  /* We can't call fbuf_getptr before the loop doing fbuf_getc, because
     fbuf_getc might reallocate the buffer.  So return current pointer
     minus all the advances, which is n plus up to two characters
     of newline or comma.  */
  return fbuf_getptr (dtp->u.p.current_unit)
	 - n - dtp->u.p.sf_seen_eor - seen_comma;
}


/* Function for reading the next couple of bytes from the current
   file, advancing the current position. We return NULL on end of record or
   end of file. This function is only for formatted I/O, unformatted uses
   read_block_direct.

   If the read is short, then it is because the current record does not
   have enough data to satisfy the read request and the file was
   opened with PAD=YES.  The caller must assume trailing spaces for
   short reads.  */

void *
read_block_form (st_parameter_dt *dtp, size_t *nbytes)
{
  char *source;
  size_t norig;

  if (!is_stream_io (dtp))
    {
      if (dtp->u.p.current_unit->bytes_left < (gfc_offset) *nbytes)
	{
	  /* For preconnected units with default record length, set bytes left
	   to unit record length and proceed, otherwise error.  */
	  if (dtp->u.p.current_unit->unit_number == options.stdin_unit
	      && dtp->u.p.current_unit->recl == default_recl)
            dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	  else
	    {
	      if (unlikely (dtp->u.p.current_unit->pad_status == PAD_NO)
		  && !is_internal_unit (dtp))
		{
		  /* Not enough data left.  */
		  generate_error (&dtp->common, LIBERROR_EOR, NULL);
		  return NULL;
		}
	    }

	  if (is_internal_unit(dtp))
	    {
	      if (*nbytes > 0 && dtp->u.p.current_unit->bytes_left == 0)
	        {
		  if (dtp->u.p.advance_status == ADVANCE_NO)
		    {
		      generate_error (&dtp->common, LIBERROR_EOR, NULL);
		      return NULL;
		    }
		}
	    }
	  else
	    {
	      if (unlikely (dtp->u.p.current_unit->bytes_left == 0))
		{
		  hit_eof (dtp);
		  return NULL;
		}
	    }

	  *nbytes = dtp->u.p.current_unit->bytes_left;
	}
    }

  if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED &&
      (dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL ||
       dtp->u.p.current_unit->flags.access == ACCESS_STREAM))
    {
      if (is_internal_unit (dtp))
	source = read_sf_internal (dtp, nbytes);
      else
	source = read_sf (dtp, nbytes);

      dtp->u.p.current_unit->strm_pos +=
	(gfc_offset) (*nbytes + dtp->u.p.sf_seen_eor);
      return source;
    }

  /* If we reach here, we can assume it's direct access.  */

  dtp->u.p.current_unit->bytes_left -= (gfc_offset) *nbytes;

  norig = *nbytes;
  source = fbuf_read (dtp->u.p.current_unit, nbytes);
  fbuf_seek (dtp->u.p.current_unit, *nbytes, SEEK_CUR);

  if (((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0) ||
      dtp->u.p.current_unit->has_size)
    dtp->u.p.current_unit->size_used += (GFC_IO_INT) *nbytes;

  if (norig != *nbytes)
    {
      /* Short read, this shouldn't happen.  */
      if (dtp->u.p.current_unit->pad_status == PAD_NO)
	{
	  generate_error (&dtp->common, LIBERROR_EOR, NULL);
	  source = NULL;
	}
    }

  dtp->u.p.current_unit->strm_pos += (gfc_offset) *nbytes;

  return source;
}


/* Read a block from a character(kind=4) internal unit, to be transferred into
   a character(kind=4) variable.  Note: Portions of this code borrowed from
   read_sf_internal.  */
void *
read_block_form4 (st_parameter_dt *dtp, size_t *nbytes)
{
  static gfc_char4_t *empty_string[0];
  gfc_char4_t *source;
  size_t lorig;

  if (dtp->u.p.current_unit->bytes_left < (gfc_offset) *nbytes)
    *nbytes = dtp->u.p.current_unit->bytes_left;

  /* Zero size array gives internal unit len of 0.  Nothing to read. */
  if (dtp->internal_unit_len == 0
      && dtp->u.p.current_unit->pad_status == PAD_NO)
    hit_eof (dtp);

  /* If we have seen an eor previously, return a length of 0.  The
     caller is responsible for correctly padding the input field.  */
  if (dtp->u.p.sf_seen_eor)
    {
      *nbytes = 0;
      /* Just return something that isn't a NULL pointer, otherwise the
         caller thinks an error occurred.  */
      return empty_string;
    }

  lorig = *nbytes;
  source = (gfc_char4_t *) mem_alloc_r4 (dtp->u.p.current_unit->s, nbytes);

  if (unlikely (lorig > *nbytes))
    {
      hit_eof (dtp);
      return NULL;
    }

  dtp->u.p.current_unit->bytes_left -= *nbytes;

  if (((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0) ||
      dtp->u.p.current_unit->has_size)
    dtp->u.p.current_unit->size_used += (GFC_IO_INT) *nbytes;

  return source;
}


/* Reads a block directly into application data space.  This is for
   unformatted files.  */

static void
read_block_direct (st_parameter_dt *dtp, void *buf, size_t nbytes)
{
  ssize_t to_read_record;
  ssize_t have_read_record;
  ssize_t to_read_subrecord;
  ssize_t have_read_subrecord;
  int short_record;

  if (is_stream_io (dtp))
    {
      have_read_record = sread (dtp->u.p.current_unit->s, buf,
				nbytes);
      if (unlikely (have_read_record < 0))
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return;
	}

      dtp->u.p.current_unit->strm_pos += (gfc_offset) have_read_record;

      if (unlikely ((ssize_t) nbytes != have_read_record))
	{
	  /* Short read,  e.g. if we hit EOF.  For stream files,
	   we have to set the end-of-file condition.  */
          hit_eof (dtp);
	}
      return;
    }

  if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT)
    {
      if (dtp->u.p.current_unit->bytes_left < (gfc_offset) nbytes)
	{
	  short_record = 1;
	  to_read_record = dtp->u.p.current_unit->bytes_left;
	  nbytes = to_read_record;
	}
      else
	{
	  short_record = 0;
	  to_read_record = nbytes;
	}

      dtp->u.p.current_unit->bytes_left -= to_read_record;

      to_read_record = sread (dtp->u.p.current_unit->s, buf, to_read_record);
      if (unlikely (to_read_record < 0))
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return;
	}

      if (to_read_record != (ssize_t) nbytes)
	{
	  /* Short read, e.g. if we hit EOF.  Apparently, we read
	   more than was written to the last record.  */
	  return;
	}

      if (unlikely (short_record))
	{
	  generate_error (&dtp->common, LIBERROR_SHORT_RECORD, NULL);
	}
      return;
    }

  /* Unformatted sequential.  We loop over the subrecords, reading
     until the request has been fulfilled or the record has run out
     of continuation subrecords.  */

  /* Check whether we exceed the total record length.  */

  if (dtp->u.p.current_unit->flags.has_recl
      && ((gfc_offset) nbytes > dtp->u.p.current_unit->bytes_left))
    {
      to_read_record = dtp->u.p.current_unit->bytes_left;
      short_record = 1;
    }
  else
    {
      to_read_record = nbytes;
      short_record = 0;
    }
  have_read_record = 0;

  while(1)
    {
      if (dtp->u.p.current_unit->bytes_left_subrecord
	  < (gfc_offset) to_read_record)
	{
	  to_read_subrecord = dtp->u.p.current_unit->bytes_left_subrecord;
	  to_read_record -= to_read_subrecord;
	}
      else
	{
	  to_read_subrecord = to_read_record;
	  to_read_record = 0;
	}

      dtp->u.p.current_unit->bytes_left_subrecord -= to_read_subrecord;

      have_read_subrecord = sread (dtp->u.p.current_unit->s,
				   buf + have_read_record, to_read_subrecord);
      if (unlikely (have_read_subrecord < 0))
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return;
	}

      have_read_record += have_read_subrecord;

      if (unlikely (to_read_subrecord != have_read_subrecord))
	{
	  /* Short read, e.g. if we hit EOF.  This means the record
	     structure has been corrupted, or the trailing record
	     marker would still be present.  */

	  generate_error (&dtp->common, LIBERROR_CORRUPT_FILE, NULL);
	  return;
	}

      if (to_read_record > 0)
	{
	  if (likely (dtp->u.p.current_unit->continued))
	    {
	      next_record_r_unf (dtp, 0);
	      us_read (dtp, 1);
	    }
	  else
	    {
	      /* Let's make sure the file position is correctly pre-positioned
		 for the next read statement.  */

	      dtp->u.p.current_unit->current_record = 0;
	      next_record_r_unf (dtp, 0);
	      generate_error (&dtp->common, LIBERROR_SHORT_RECORD, NULL);
	      return;
	    }
	}
      else
	{
	  /* Normal exit, the read request has been fulfilled.  */
	  break;
	}
    }

  dtp->u.p.current_unit->bytes_left -= have_read_record;
  if (unlikely (short_record))
    {
      generate_error (&dtp->common, LIBERROR_SHORT_RECORD, NULL);
      return;
    }
  return;
}


/* Function for writing a block of bytes to the current file at the
   current position, advancing the file pointer. We are given a length
   and return a pointer to a buffer that the caller must (completely)
   fill in.  Returns NULL on error.  */

void *
write_block (st_parameter_dt *dtp, size_t length)
{
  char *dest;

  if (!is_stream_io (dtp))
    {
      if (dtp->u.p.current_unit->bytes_left < (gfc_offset) length)
	{
	  /* For preconnected units with default record length, set bytes left
	     to unit record length and proceed, otherwise error.  */
	  if (likely ((dtp->u.p.current_unit->unit_number
		       == options.stdout_unit
		       || dtp->u.p.current_unit->unit_number
		       == options.stderr_unit)
		      && dtp->u.p.current_unit->recl == default_recl))
	    dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	  else
	    {
	      generate_error (&dtp->common, LIBERROR_EOR, NULL);
	      return NULL;
	    }
	}

      dtp->u.p.current_unit->bytes_left -= (gfc_offset) length;
    }

  if (is_internal_unit (dtp))
    {
      if (is_char4_unit(dtp)) /* char4 internel unit.  */
	{
	  gfc_char4_t *dest4;
	  dest4 = mem_alloc_w4 (dtp->u.p.current_unit->s, &length);
	  if (dest4 == NULL)
	  {
            generate_error (&dtp->common, LIBERROR_END, NULL);
            return NULL;
	  }
	  return dest4;
	}
      else
	dest = mem_alloc_w (dtp->u.p.current_unit->s, &length);

      if (dest == NULL)
	{
          generate_error (&dtp->common, LIBERROR_END, NULL);
          return NULL;
	}

      if (unlikely (dtp->u.p.current_unit->endfile == AT_ENDFILE))
	generate_error (&dtp->common, LIBERROR_END, NULL);
    }
  else
    {
      dest = fbuf_alloc (dtp->u.p.current_unit, length);
      if (dest == NULL)
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return NULL;
	}
    }

  if (((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0) ||
      dtp->u.p.current_unit->has_size)
    dtp->u.p.current_unit->size_used += (GFC_IO_INT) length;

  dtp->u.p.current_unit->strm_pos += (gfc_offset) length;

  return dest;
}


/* High level interface to swrite(), taking care of errors.  This is only
   called for unformatted files.  There are three cases to consider:
   Stream I/O, unformatted direct, unformatted sequential.  */

static bool
write_buf (st_parameter_dt *dtp, void *buf, size_t nbytes)
{

  ssize_t have_written;
  ssize_t to_write_subrecord;
  int short_record;

  /* Stream I/O.  */

  if (is_stream_io (dtp))
    {
      have_written = swrite (dtp->u.p.current_unit->s, buf, nbytes);
      if (unlikely (have_written < 0))
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return false;
	}

      dtp->u.p.current_unit->strm_pos += (gfc_offset) have_written;

      return true;
    }

  /* Unformatted direct access.  */

  if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT)
    {
      if (unlikely (dtp->u.p.current_unit->bytes_left < (gfc_offset) nbytes))
	{
	  generate_error (&dtp->common, LIBERROR_DIRECT_EOR, NULL);
	  return false;
	}

      if (buf == NULL && nbytes == 0)
	return true;

      have_written = swrite (dtp->u.p.current_unit->s, buf, nbytes);
      if (unlikely (have_written < 0))
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return false;
	}

      dtp->u.p.current_unit->strm_pos += (gfc_offset) have_written;
      dtp->u.p.current_unit->bytes_left -= (gfc_offset) have_written;

      return true;
    }

  /* Unformatted sequential.  */

  have_written = 0;

  if (dtp->u.p.current_unit->flags.has_recl
      && (gfc_offset) nbytes > dtp->u.p.current_unit->bytes_left)
    {
      nbytes = dtp->u.p.current_unit->bytes_left;
      short_record = 1;
    }
  else
    {
      short_record = 0;
    }

  while (1)
    {

      to_write_subrecord =
	(size_t) dtp->u.p.current_unit->bytes_left_subrecord < nbytes ?
	(size_t) dtp->u.p.current_unit->bytes_left_subrecord : nbytes;

      dtp->u.p.current_unit->bytes_left_subrecord -=
	(gfc_offset) to_write_subrecord;

      to_write_subrecord = swrite (dtp->u.p.current_unit->s,
				   buf + have_written, to_write_subrecord);
      if (unlikely (to_write_subrecord < 0))
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return false;
	}

      dtp->u.p.current_unit->strm_pos += (gfc_offset) to_write_subrecord;
      nbytes -= to_write_subrecord;
      have_written += to_write_subrecord;

      if (nbytes == 0)
	break;

      next_record_w_unf (dtp, 1);
      us_write (dtp, 1);
    }
  dtp->u.p.current_unit->bytes_left -= have_written;
  if (unlikely (short_record))
    {
      generate_error (&dtp->common, LIBERROR_SHORT_RECORD, NULL);
      return false;
    }
  return true;
}


/* Reverse memcpy - used for byte swapping.  */

static void
reverse_memcpy (void *dest, const void *src, size_t n)
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


/* Utility function for byteswapping an array, using the bswap
   builtins if possible. dest and src can overlap completely, or then
   they must point to separate objects; partial overlaps are not
   allowed.  */

static void
bswap_array (void *dest, const void *src, size_t size, size_t nelems)
{
  const char *ps;
  char *pd;

  switch (size)
    {
    case 1:
      break;
    case 2:
      for (size_t i = 0; i < nelems; i++)
	((uint16_t*)dest)[i] = __builtin_bswap16 (((uint16_t*)src)[i]);
      break;
    case 4:
      for (size_t i = 0; i < nelems; i++)
	((uint32_t*)dest)[i] = __builtin_bswap32 (((uint32_t*)src)[i]);
      break;
    case 8:
      for (size_t i = 0; i < nelems; i++)
	((uint64_t*)dest)[i] = __builtin_bswap64 (((uint64_t*)src)[i]);
      break;
    case 12:
      ps = src;
      pd = dest;
      for (size_t i = 0; i < nelems; i++)
	{
	  uint32_t tmp;
	  memcpy (&tmp, ps, 4);
	  *(uint32_t*)pd = __builtin_bswap32 (*(uint32_t*)(ps + 8));
	  *(uint32_t*)(pd + 4) = __builtin_bswap32 (*(uint32_t*)(ps + 4));
	  *(uint32_t*)(pd + 8) = __builtin_bswap32 (tmp);
	  ps += size;
	  pd += size;
	}
      break;
    case 16:
      ps = src;
      pd = dest;
      for (size_t i = 0; i < nelems; i++)
	{
	  uint64_t tmp;
	  memcpy (&tmp, ps, 8);
	  *(uint64_t*)pd = __builtin_bswap64 (*(uint64_t*)(ps + 8));
	  *(uint64_t*)(pd + 8) = __builtin_bswap64 (tmp);
	  ps += size;
	  pd += size;
	}
      break;
    default:
      pd = dest;
      if (dest != src)
	{
	  ps = src;
	  for (size_t i = 0; i < nelems; i++)
	    {
	      reverse_memcpy (pd, ps, size);
	      ps += size;
	      pd += size;
	    }
	}
      else
	{
	  /* In-place byte swap.  */
	  for (size_t i = 0; i < nelems; i++)
	    {
	      char tmp, *low = pd, *high = pd + size - 1;
	      for (size_t j = 0; j < size/2; j++)
		{
		  tmp = *low;
		  *low = *high;
		  *high = tmp;
		  low++;
		  high--;
		}
	      pd += size;
	    }
	}
    }
}


/* Master function for unformatted reads.  */

static void
unformatted_read (st_parameter_dt *dtp, bt type,
		  void *dest, int kind, size_t size, size_t nelems)
{
  unit_convert convert;

  if (type == BT_CLASS)
    {
	  int unit = dtp->u.p.current_unit->unit_number;
	  char tmp_iomsg[IOMSG_LEN] = "";
	  char *child_iomsg;
	  gfc_charlen_type child_iomsg_len;
	  int noiostat;
	  int *child_iostat = NULL;

	  /* Set iostat, intent(out).  */
	  noiostat = 0;
	  child_iostat = (dtp->common.flags & IOPARM_HAS_IOSTAT) ?
			  dtp->common.iostat : &noiostat;

	  /* Set iomsg, intent(inout).  */
	  if (dtp->common.flags & IOPARM_HAS_IOMSG)
	    {
	      child_iomsg = dtp->common.iomsg;
	      child_iomsg_len = dtp->common.iomsg_len;
	    }
	  else
	    {
	      child_iomsg = tmp_iomsg;
	      child_iomsg_len = IOMSG_LEN;
	    }

	  /* Call the user defined unformatted READ procedure.  */
	  dtp->u.p.current_unit->child_dtio++;
	  dtp->u.p.ufdtio_ptr (dest, &unit, child_iostat, child_iomsg,
			      child_iomsg_len);
	  dtp->u.p.current_unit->child_dtio--;
	  return;
    }

  if (type == BT_CHARACTER)
    size *= GFC_SIZE_OF_CHAR_KIND(kind);
  read_block_direct (dtp, dest, size * nelems);

  convert = dtp->u.p.current_unit->flags.convert;
  if (unlikely (convert != GFC_CONVERT_NATIVE) && kind != 1)
    {
      /* Handle wide chracters.  */
      if (type == BT_CHARACTER)
  	{
  	  nelems *= size;
  	  size = kind;
  	}

      /* Break up complex into its constituent reals.  */
      else if (type == BT_COMPLEX)
  	{
  	  nelems *= 2;
  	  size /= 2;
  	}
#ifndef HAVE_GFC_REAL_17
#if defined(HAVE_GFC_REAL_16) && GFC_REAL_16_DIGITS == 106
      /* IBM extended format is stored as a pair of IEEE754
	 double values, with the more significant value first
	 in both big and little endian.  */
      if (kind == 16 && (type == BT_REAL || type == BT_COMPLEX))
	{
	  nelems *= 2;
	  size /= 2;
	}
#endif
      bswap_array (dest, dest, size, nelems);
#else
      unit_convert bswap = convert & ~(GFC_CONVERT_R16_IEEE | GFC_CONVERT_R16_IBM);
      if (bswap == GFC_CONVERT_SWAP)
	{
	  if ((type == BT_REAL || type == BT_COMPLEX)
	      && ((kind == 16 && (convert & GFC_CONVERT_R16_IEEE) == 0)
		  || (kind == 17 && (convert & GFC_CONVERT_R16_IBM))))
	    bswap_array (dest, dest, size / 2, nelems * 2);
	  else
	    bswap_array (dest, dest, size, nelems);
	}

      if ((convert & GFC_CONVERT_R16_IEEE)
	  && kind == 16
	  && (type == BT_REAL || type == BT_COMPLEX))
	{
	  char *pd = dest;
	  for (size_t i = 0; i < nelems; i++)
	    {
	      GFC_REAL_16 r16;
	      GFC_REAL_17 r17;
	      memcpy (&r17, pd, 16);
	      r16 = r17;
	      memcpy (pd, &r16, 16);
	      pd += size;
	    }
	}
      else if ((dtp->u.p.current_unit->flags.convert & GFC_CONVERT_R16_IBM)
	       && kind == 17
	       && (type == BT_REAL || type == BT_COMPLEX))
	{
	  if (type == BT_COMPLEX && size == 32)
	    {
	      nelems *= 2;
	      size /= 2;
	    }

	  char *pd = dest;
	  for (size_t i = 0; i < nelems; i++)
	    {
	      GFC_REAL_16 r16;
	      GFC_REAL_17 r17;
	      memcpy (&r16, pd, 16);
	      r17 = r16;
	      memcpy (pd, &r17, 16);
	      pd += size;
	    }
	}
#endif /* HAVE_GFC_REAL_17.  */
    }
}


/* Master function for unformatted writes.  NOTE: For kind=10 the size is 16
   bytes on 64 bit machines.  The unused bytes are not initialized and never
   used, which can show an error with memory checking analyzers like
   valgrind.  We us BT_CLASS to denote a User Defined I/O call.  */

static void
unformatted_write (st_parameter_dt *dtp, bt type,
		   void *source, int kind, size_t size, size_t nelems)
{
  unit_convert convert;

  if (type == BT_CLASS)
    {
	  int unit = dtp->u.p.current_unit->unit_number;
	  char tmp_iomsg[IOMSG_LEN] = "";
	  char *child_iomsg;
	  gfc_charlen_type child_iomsg_len;
	  int noiostat;
	  int *child_iostat = NULL;

	  /* Set iostat, intent(out).  */
	  noiostat = 0;
	  child_iostat = (dtp->common.flags & IOPARM_HAS_IOSTAT) ?
			  dtp->common.iostat : &noiostat;

	  /* Set iomsg, intent(inout).  */
	  if (dtp->common.flags & IOPARM_HAS_IOMSG)
	    {
	      child_iomsg = dtp->common.iomsg;
	      child_iomsg_len = dtp->common.iomsg_len;
	    }
	  else
	    {
	      child_iomsg = tmp_iomsg;
	      child_iomsg_len = IOMSG_LEN;
	    }

	  /* Call the user defined unformatted WRITE procedure.  */
	  dtp->u.p.current_unit->child_dtio++;
	  dtp->u.p.ufdtio_ptr (source, &unit, child_iostat, child_iomsg,
			      child_iomsg_len);
	  dtp->u.p.current_unit->child_dtio--;
	  return;
    }

  convert = dtp->u.p.current_unit->flags.convert;
  if (likely (convert == GFC_CONVERT_NATIVE) || kind == 1
#ifdef HAVE_GFC_REAL_17
      || ((type == BT_REAL || type == BT_COMPLEX)
	  && ((kind == 16 && convert == GFC_CONVERT_R16_IBM)
	      || (kind == 17 && convert == GFC_CONVERT_R16_IEEE)))
#endif
      )
    {
      size_t stride = type == BT_CHARACTER ?
		  size * GFC_SIZE_OF_CHAR_KIND(kind) : size;

      write_buf (dtp, source, stride * nelems);
    }
  else
    {
#define BSWAP_BUFSZ 512
      char buffer[BSWAP_BUFSZ];
      char *p;
      size_t nrem;

      p = source;

      /* Handle wide chracters.  */
      if (type == BT_CHARACTER && kind != 1)
	{
	  nelems *= size;
	  size = kind;
	}

      /* Break up complex into its constituent reals.  */
      if (type == BT_COMPLEX)
	{
	  nelems *= 2;
	  size /= 2;
	}

#if !defined(HAVE_GFC_REAL_17) && defined(HAVE_GFC_REAL_16) \
    && GFC_REAL_16_DIGITS == 106
      /* IBM extended format is stored as a pair of IEEE754
	 double values, with the more significant value first
	 in both big and little endian.  */
      if (kind == 16 && (type == BT_REAL || type == BT_COMPLEX))
	{
	  nelems *= 2;
	  size /= 2;
	}
#endif

      /* By now, all complex variables have been split into their
	 constituent reals.  */

      nrem = nelems;
      do
	{
	  size_t nc;
	  if (size * nrem > BSWAP_BUFSZ)
	    nc = BSWAP_BUFSZ / size;
	  else
	    nc = nrem;

#ifdef HAVE_GFC_REAL_17
	  if ((dtp->u.p.current_unit->flags.convert & GFC_CONVERT_R16_IEEE)
	      && kind == 16
	      && (type == BT_REAL || type == BT_COMPLEX))
	    {
	      for (size_t i = 0; i < nc; i++)
		{
		  GFC_REAL_16 r16;
		  GFC_REAL_17 r17;
		  memcpy (&r16, p, 16);
		  r17 = r16;
		  memcpy (&buffer[i * 16], &r17, 16);
		  p += 16;
		}
	      if ((dtp->u.p.current_unit->flags.convert
		   & ~(GFC_CONVERT_R16_IEEE | GFC_CONVERT_R16_IBM))
		  == GFC_CONVERT_SWAP)
		bswap_array (buffer, buffer, size, nc);
	    }
	  else if ((dtp->u.p.current_unit->flags.convert & GFC_CONVERT_R16_IBM)
		   && kind == 17
		   && (type == BT_REAL || type == BT_COMPLEX))
	    {
	      for (size_t i = 0; i < nc; i++)
		{
		  GFC_REAL_16 r16;
		  GFC_REAL_17 r17;
		  memcpy (&r17, p, 16);
		  r16 = r17;
		  memcpy (&buffer[i * 16], &r16, 16);
		  p += 16;
		}
	      if ((dtp->u.p.current_unit->flags.convert
		   & ~(GFC_CONVERT_R16_IEEE | GFC_CONVERT_R16_IBM))
		  == GFC_CONVERT_SWAP)
		bswap_array (buffer, buffer, size / 2, nc * 2);
	    }
	  else if (kind == 16 && (type == BT_REAL || type == BT_COMPLEX))
	    {
	      bswap_array (buffer, p, size / 2, nc * 2);
	      p += size * nc;
	    }
	  else
#endif
	    {
	      bswap_array (buffer, p, size, nc);
	      p += size * nc;
	    }
	  write_buf (dtp, buffer, size * nc);
	  nrem -= nc;
	}
      while (nrem > 0);
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
    case BT_CLASS:
      p = "CLASS or DERIVED";
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
#define BUFLEN 100
  char buffer[BUFLEN];

  if (actual == expected)
    return 0;

  /* Adjust item_count before emitting error message.  */
  snprintf (buffer, BUFLEN,
	    "Expected %s for item %d in formatted transfer, got %s",
	   type_name (expected), dtp->u.p.item_count - 1, type_name (actual));

  format_error (dtp, f, buffer);
  return 1;
}


/* Check that the dtio procedure required for formatted IO is present.  */

static int
check_dtio_proc (st_parameter_dt *dtp, const fnode *f)
{
  char buffer[BUFLEN];

  if (dtp->u.p.fdtio_ptr != NULL)
    return 0;

  snprintf (buffer, BUFLEN,
	    "Missing DTIO procedure or intrinsic type passed for item %d "
	    "in formatted transfer",
	    dtp->u.p.item_count - 1);

  format_error (dtp, f, buffer);
  return 1;
}


static int
require_numeric_type (st_parameter_dt *dtp, bt actual, const fnode *f)
{
#define BUFLEN 100
  char buffer[BUFLEN];

  if (actual == BT_INTEGER || actual == BT_REAL || actual == BT_COMPLEX)
    return 0;

  /* Adjust item_count before emitting error message.  */
  snprintf (buffer, BUFLEN,
	    "Expected numeric type for item %d in formatted transfer, got %s",
	    dtp->u.p.item_count - 1, type_name (actual));

  format_error (dtp, f, buffer);
  return 1;
}

static char *
get_dt_format (char *p, gfc_charlen_type *length)
{
  char delim = p[-1];  /* The delimiter is always the first character back.  */
  char c, *q, *res;
  gfc_charlen_type len = *length; /* This length already correct, less 'DT'.  */

  res = q = xmalloc (len + 2);

  /* Set the beginning of the string to 'DT', length adjusted below.  */
  *q++ = 'D';
  *q++ = 'T';

  /* The string may contain doubled quotes so scan and skip as needed.  */
  for (; len > 0; len--)
    {
      c = *q++ = *p++;
      if (c == delim)
	p++;  /* Skip the doubled delimiter.  */
    }

  /* Adjust the string length by two now that we are done.  */
  *length += 2;

  return res;
}


/* This function is in the main loop for a formatted data transfer
   statement.  It would be natural to implement this as a coroutine
   with the user program, but C makes that awkward.  We loop,
   processing format elements.  When we actually have to transfer
   data instead of just setting flags, we return control to the user
   program which calls a function that supplies the address and type
   of the next element, then comes back here to process it.  */

static void
formatted_transfer_scalar_read (st_parameter_dt *dtp, bt type, void *p, int kind,
				size_t size)
{
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

  /* Set this flag so that commas in reads cause the read to complete before
     the entire field has been read.  The next read field will start right after
     the comma in the stream.  (Set to 0 for character reads).  */
  dtp->u.p.sf_read_comma =
    dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA ? 0 : 1;

  for (;;)
    {
      /* If reversion has occurred and there is another real data item,
	 then we have to move to the next record.  */
      if (dtp->u.p.reversion_flag && n > 0)
	{
	  dtp->u.p.reversion_flag = 0;
	  next_record (dtp, 0);
	}

      consume_data_flag = 1;
      if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
	break;

      f = next_format (dtp);
      if (f == NULL)
	{
	  /* No data descriptors left.  */
	  if (unlikely (n > 0))
	    generate_error (&dtp->common, LIBERROR_FORMAT,
		"Insufficient data descriptors in format after reversion");
	  return;
	}

      t = f->format;

      bytes_used = (int)(dtp->u.p.current_unit->recl
		   - dtp->u.p.current_unit->bytes_left);

      if (is_stream_io(dtp))
	bytes_used = 0;

      switch (t)
	{
	case FMT_I:
	  if (n == 0)
	    goto need_read_data;
	  if (require_type (dtp, BT_INTEGER, type, f))
	    return;
	  read_decimal (dtp, f, p, kind);
	  break;

	case FMT_B:
	  if (n == 0)
	    goto need_read_data;
	  if (!(compile_options.allow_std & GFC_STD_GNU)
	      && require_numeric_type (dtp, type, f))
	    return;
	  if (!(compile_options.allow_std & GFC_STD_F2008)
              && require_type (dtp, BT_INTEGER, type, f))
	    return;
#ifdef HAVE_GFC_REAL_17
	  if (type == BT_REAL && kind == 17)
	    kind = 16;
#endif
	  read_radix (dtp, f, p, kind, 2);
	  break;

	case FMT_O:
	  if (n == 0)
	    goto need_read_data;
	  if (!(compile_options.allow_std & GFC_STD_GNU)
	      && require_numeric_type (dtp, type, f))
	    return;
	  if (!(compile_options.allow_std & GFC_STD_F2008)
              && require_type (dtp, BT_INTEGER, type, f))
	    return;
#ifdef HAVE_GFC_REAL_17
	  if (type == BT_REAL && kind == 17)
	    kind = 16;
#endif
	  read_radix (dtp, f, p, kind, 8);
	  break;

	case FMT_Z:
	  if (n == 0)
	    goto need_read_data;
	  if (!(compile_options.allow_std & GFC_STD_GNU)
	      && require_numeric_type (dtp, type, f))
	    return;
	  if (!(compile_options.allow_std & GFC_STD_F2008)
              && require_type (dtp, BT_INTEGER, type, f))
	    return;
#ifdef HAVE_GFC_REAL_17
	  if (type == BT_REAL && kind == 17)
	    kind = 16;
#endif
	  read_radix (dtp, f, p, kind, 16);
	  break;

	case FMT_A:
	  if (n == 0)
	    goto need_read_data;

	  /* It is possible to have FMT_A with something not BT_CHARACTER such
	     as when writing out hollerith strings, so check both type
	     and kind before calling wide character routines.  */
	  if (type == BT_CHARACTER && kind == 4)
	    read_a_char4 (dtp, f, p, size);
	  else
	    read_a (dtp, f, p, size);
	  break;

	case FMT_L:
	  if (n == 0)
	    goto need_read_data;
	  read_l (dtp, f, p, kind);
	  break;

	case FMT_D:
	  if (n == 0)
	    goto need_read_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  read_f (dtp, f, p, kind);
	  break;

	case FMT_DT:
	  if (n == 0)
	    goto need_read_data;

	  if (check_dtio_proc (dtp, f))
	    return;
	  if (require_type (dtp, BT_CLASS, type, f))
	    return;
	  int unit = dtp->u.p.current_unit->unit_number;
	  char dt[] = "DT";
	  char tmp_iomsg[IOMSG_LEN] = "";
	  char *child_iomsg;
	  gfc_charlen_type child_iomsg_len;
	  int noiostat;
	  int *child_iostat = NULL;
	  char *iotype;
	  gfc_charlen_type iotype_len = f->u.udf.string_len;

	  /* Build the iotype string.  */
	  if (iotype_len == 0)
	    {
	      iotype_len = 2;
	      iotype = dt;
	    }
	  else
	    iotype = get_dt_format (f->u.udf.string, &iotype_len);

	  /* Set iostat, intent(out).  */
	  noiostat = 0;
	  child_iostat = (dtp->common.flags & IOPARM_HAS_IOSTAT) ?
			  dtp->common.iostat : &noiostat;

	  /* Set iomsg, intent(inout).  */
	  if (dtp->common.flags & IOPARM_HAS_IOMSG)
	    {
	      child_iomsg = dtp->common.iomsg;
	      child_iomsg_len = dtp->common.iomsg_len;
	    }
	  else
	    {
	      child_iomsg = tmp_iomsg;
	      child_iomsg_len = IOMSG_LEN;
	    }

	  /* Call the user defined formatted READ procedure.  */
	  dtp->u.p.current_unit->child_dtio++;
	  dtp->u.p.current_unit->last_char = EOF - 1;
	  dtp->u.p.fdtio_ptr (p, &unit, iotype, f->u.udf.vlist,
			      child_iostat, child_iomsg,
			      iotype_len, child_iomsg_len);
	  dtp->u.p.current_unit->child_dtio--;

	  if (f->u.udf.string_len != 0)
	    free (iotype);
	  /* Note: vlist is freed in free_format_data.  */
	  break;

	case FMT_E:
	  if (n == 0)
	    goto need_read_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  read_f (dtp, f, p, kind);
	  break;

	case FMT_EN:
	  if (n == 0)
	    goto need_read_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  read_f (dtp, f, p, kind);
	  break;

	case FMT_ES:
	  if (n == 0)
	    goto need_read_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  read_f (dtp, f, p, kind);
	  break;

	case FMT_F:
	  if (n == 0)
	    goto need_read_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  read_f (dtp, f, p, kind);
	  break;

	case FMT_G:
	  if (n == 0)
	    goto need_read_data;
	  switch (type)
	    {
	      case BT_INTEGER:
		read_decimal (dtp, f, p, kind);
		break;
	      case BT_LOGICAL:
		read_l (dtp, f, p, kind);
		break;
	      case BT_CHARACTER:
		if (kind == 4)
		  read_a_char4 (dtp, f, p, size);
		else
		  read_a (dtp, f, p, size);
		break;
	      case BT_REAL:
		read_f (dtp, f, p, kind);
		break;
	      default:
		internal_error (&dtp->common,
				"formatted_transfer (): Bad type");
	    }
	  break;

	case FMT_STRING:
	  consume_data_flag = 0;
	  format_error (dtp, f, "Constant string in input format");
	  return;

	/* Format codes that don't transfer data.  */
	case FMT_X:
	case FMT_TR:
	  consume_data_flag = 0;
	  dtp->u.p.skips += f->u.n;
	  pos = bytes_used + dtp->u.p.skips - 1;
	  dtp->u.p.pending_spaces = pos - dtp->u.p.max_pos + 1;
	  read_x (dtp, f->u.n);
	  break;

	case FMT_TL:
	case FMT_T:
	  consume_data_flag = 0;

	  if (f->format == FMT_TL)
	    {
	      /* Handle the special case when no bytes have been used yet.
	         Cannot go below zero. */
	      if (bytes_used == 0)
		{
		  dtp->u.p.pending_spaces -= f->u.n;
		  dtp->u.p.skips -= f->u.n;
		  dtp->u.p.skips = dtp->u.p.skips < 0 ? 0 : dtp->u.p.skips;
		}

	      pos = bytes_used - f->u.n;
	    }
	  else /* FMT_T */
	    pos = f->u.n - 1;

	  /* Standard 10.6.1.1: excessive left tabbing is reset to the
	     left tab limit.  We do not check if the position has gone
	     beyond the end of record because a subsequent tab could
	     bring us back again.  */
	  pos = pos < 0 ? 0 : pos;

	  dtp->u.p.skips = dtp->u.p.skips + pos - bytes_used;
	  dtp->u.p.pending_spaces = dtp->u.p.pending_spaces
				    + pos - dtp->u.p.max_pos;
	  dtp->u.p.pending_spaces = dtp->u.p.pending_spaces < 0
				    ? 0 : dtp->u.p.pending_spaces;
	  if (dtp->u.p.skips == 0)
	    break;

	  /* Adjust everything for end-of-record condition */
	  if (dtp->u.p.sf_seen_eor && !is_internal_unit (dtp))
	    {
              dtp->u.p.current_unit->bytes_left -= dtp->u.p.sf_seen_eor;
              dtp->u.p.skips -= dtp->u.p.sf_seen_eor;
	      bytes_used = pos;
	      if (dtp->u.p.pending_spaces == 0)
		dtp->u.p.sf_seen_eor = 0;
	    }
	  if (dtp->u.p.skips < 0)
	    {
              if (is_internal_unit (dtp))
                sseek (dtp->u.p.current_unit->s, dtp->u.p.skips, SEEK_CUR);
              else
                fbuf_seek (dtp->u.p.current_unit, dtp->u.p.skips, SEEK_CUR);
	      dtp->u.p.current_unit->bytes_left -= (gfc_offset) dtp->u.p.skips;
	      dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
	    }
	  else
	    read_x (dtp, dtp->u.p.skips);
	  break;

	case FMT_S:
	  consume_data_flag = 0;
	  dtp->u.p.sign_status = SIGN_PROCDEFINED;
	  break;

	case FMT_SS:
	  consume_data_flag = 0;
	  dtp->u.p.sign_status = SIGN_SUPPRESS;
	  break;

	case FMT_SP:
	  consume_data_flag = 0;
	  dtp->u.p.sign_status = SIGN_PLUS;
	  break;

	case FMT_BN:
	  consume_data_flag = 0 ;
	  dtp->u.p.blank_status = BLANK_NULL;
	  break;

	case FMT_BZ:
	  consume_data_flag = 0;
	  dtp->u.p.blank_status = BLANK_ZERO;
	  break;

	case FMT_DC:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->decimal_status = DECIMAL_COMMA;
	  break;

	case FMT_DP:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->decimal_status = DECIMAL_POINT;
	  break;

	case FMT_RC:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_COMPATIBLE;
	  break;

	case FMT_RD:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_DOWN;
	  break;

	case FMT_RN:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_NEAREST;
	  break;

	case FMT_RP:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_PROCDEFINED;
	  break;

	case FMT_RU:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_UP;
	  break;

	case FMT_RZ:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_ZERO;
	  break;

	case FMT_P:
	  consume_data_flag = 0;
	  dtp->u.p.scale_factor = f->u.k;
	  break;

	case FMT_DOLLAR:
	  consume_data_flag = 0;
	  dtp->u.p.seen_dollar = 1;
	  break;

	case FMT_SLASH:
	  consume_data_flag = 0;
	  dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
	  next_record (dtp, 0);
	  break;

	case FMT_COLON:
	  /* A colon descriptor causes us to exit this loop (in
	     particular preventing another / descriptor from being
	     processed) unless there is another data item to be
	     transferred.  */
	  consume_data_flag = 0;
	  if (n == 0)
	    return;
	  break;

	default:
	  internal_error (&dtp->common, "Bad format node");
	}

      /* Adjust the item count and data pointer.  */

      if ((consume_data_flag > 0) && (n > 0))
	{
	  n--;
	  p = ((char *) p) + size;
	}

      dtp->u.p.skips = 0;

      pos = (int)(dtp->u.p.current_unit->recl - dtp->u.p.current_unit->bytes_left);
      dtp->u.p.max_pos = (dtp->u.p.max_pos > pos) ? dtp->u.p.max_pos : pos;
    }

  return;

  /* Come here when we need a data descriptor but don't have one.  We
     push the current format node back onto the input, then return and
     let the user program call us back with the data.  */
 need_read_data:
  unget_format (dtp, f);
}


static void
formatted_transfer_scalar_write (st_parameter_dt *dtp, bt type, void *p, int kind,
				 size_t size)
{
  gfc_offset pos, bytes_used;
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

  /* Set this flag so that commas in reads cause the read to complete before
     the entire field has been read.  The next read field will start right after
     the comma in the stream.  (Set to 0 for character reads).  */
  dtp->u.p.sf_read_comma =
    dtp->u.p.current_unit->decimal_status == DECIMAL_COMMA ? 0 : 1;

  for (;;)
    {
      /* If reversion has occurred and there is another real data item,
	 then we have to move to the next record.  */
      if (dtp->u.p.reversion_flag && n > 0)
	{
	  dtp->u.p.reversion_flag = 0;
	  next_record (dtp, 0);
	}

      consume_data_flag = 1;
      if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
	break;

      f = next_format (dtp);
      if (f == NULL)
	{
	  /* No data descriptors left.  */
	  if (unlikely (n > 0))
	    generate_error (&dtp->common, LIBERROR_FORMAT,
		"Insufficient data descriptors in format after reversion");
	  return;
	}

      /* Now discharge T, TR and X movements to the right.  This is delayed
	 until a data producing format to suppress trailing spaces.  */

      t = f->format;
      if (dtp->u.p.mode == WRITING && dtp->u.p.skips != 0
	&& ((n>0 && (  t == FMT_I  || t == FMT_B  || t == FMT_O
		    || t == FMT_Z  || t == FMT_F  || t == FMT_E
		    || t == FMT_EN || t == FMT_ES || t == FMT_G
		    || t == FMT_L  || t == FMT_A  || t == FMT_D
		    || t == FMT_DT))
	    || t == FMT_STRING))
	{
	  if (dtp->u.p.skips > 0)
	    {
	      gfc_offset tmp;
	      write_x (dtp, dtp->u.p.skips, dtp->u.p.pending_spaces);
	      tmp = dtp->u.p.current_unit->recl
			  - dtp->u.p.current_unit->bytes_left;
	      dtp->u.p.max_pos =
		dtp->u.p.max_pos > tmp ? dtp->u.p.max_pos : tmp;
	      dtp->u.p.skips = 0;
	    }
	  if (dtp->u.p.skips < 0)
	    {
              if (is_internal_unit (dtp))
	        sseek (dtp->u.p.current_unit->s, dtp->u.p.skips, SEEK_CUR);
              else
                fbuf_seek (dtp->u.p.current_unit, dtp->u.p.skips, SEEK_CUR);
	      dtp->u.p.current_unit->bytes_left -= (gfc_offset) dtp->u.p.skips;
	    }
	  dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
	}

      bytes_used = dtp->u.p.current_unit->recl
		   - dtp->u.p.current_unit->bytes_left;

      if (is_stream_io(dtp))
	bytes_used = 0;

      switch (t)
	{
	case FMT_I:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_INTEGER, type, f))
	    return;
	  write_i (dtp, f, p, kind);
	  break;

	case FMT_B:
	  if (n == 0)
	    goto need_data;
	  if (!(compile_options.allow_std & GFC_STD_GNU)
	      && require_numeric_type (dtp, type, f))
	    return;
	  if (!(compile_options.allow_std & GFC_STD_F2008)
              && require_type (dtp, BT_INTEGER, type, f))
	    return;
#ifdef HAVE_GFC_REAL_17
	  if (type == BT_REAL && kind == 17)
	    kind = 16;
#endif
	  write_b (dtp, f, p, kind);
	  break;

	case FMT_O:
	  if (n == 0)
	    goto need_data;
	  if (!(compile_options.allow_std & GFC_STD_GNU)
	      && require_numeric_type (dtp, type, f))
	    return;
	  if (!(compile_options.allow_std & GFC_STD_F2008)
              && require_type (dtp, BT_INTEGER, type, f))
	    return;
#ifdef HAVE_GFC_REAL_17
	  if (type == BT_REAL && kind == 17)
	    kind = 16;
#endif
	  write_o (dtp, f, p, kind);
	  break;

	case FMT_Z:
	  if (n == 0)
	    goto need_data;
	  if (!(compile_options.allow_std & GFC_STD_GNU)
	      && require_numeric_type (dtp, type, f))
	    return;
	  if (!(compile_options.allow_std & GFC_STD_F2008)
              && require_type (dtp, BT_INTEGER, type, f))
	    return;
#ifdef HAVE_GFC_REAL_17
	  if (type == BT_REAL && kind == 17)
	    kind = 16;
#endif
	  write_z (dtp, f, p, kind);
	  break;

	case FMT_A:
	  if (n == 0)
	    goto need_data;

	  /* It is possible to have FMT_A with something not BT_CHARACTER such
	     as when writing out hollerith strings, so check both type
	     and kind before calling wide character routines.  */
	  if (type == BT_CHARACTER && kind == 4)
	    write_a_char4 (dtp, f, p, size);
	  else
	    write_a (dtp, f, p, size);
	  break;

	case FMT_L:
	  if (n == 0)
	    goto need_data;
	  write_l (dtp, f, p, kind);
	  break;

	case FMT_D:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  if (f->u.real.w == 0)
	    write_real_w0 (dtp, p, kind, f);
	  else
	    write_d (dtp, f, p, kind);
	  break;

	case FMT_DT:
	  if (n == 0)
	    goto need_data;
	  int unit = dtp->u.p.current_unit->unit_number;
	  char dt[] = "DT";
	  char tmp_iomsg[IOMSG_LEN] = "";
	  char *child_iomsg;
	  gfc_charlen_type child_iomsg_len;
	  int noiostat;
	  int *child_iostat = NULL;
	  char *iotype;
	  gfc_charlen_type iotype_len = f->u.udf.string_len;

	  /* Build the iotype string.  */
	  if (iotype_len == 0)
	    {
	      iotype_len = 2;
	      iotype = dt;
	    }
	  else
	    iotype = get_dt_format (f->u.udf.string, &iotype_len);

	  /* Set iostat, intent(out).  */
	  noiostat = 0;
	  child_iostat = (dtp->common.flags & IOPARM_HAS_IOSTAT) ?
			  dtp->common.iostat : &noiostat;

	  /* Set iomsg, intent(inout).  */
	  if (dtp->common.flags & IOPARM_HAS_IOMSG)
	    {
	      child_iomsg = dtp->common.iomsg;
	      child_iomsg_len = dtp->common.iomsg_len;
	    }
	  else
	    {
	      child_iomsg = tmp_iomsg;
	      child_iomsg_len = IOMSG_LEN;
	    }

	  if (check_dtio_proc (dtp, f))
	    return;

	  /* Call the user defined formatted WRITE procedure.  */
	  dtp->u.p.current_unit->child_dtio++;

	  dtp->u.p.fdtio_ptr (p, &unit, iotype, f->u.udf.vlist,
			      child_iostat, child_iomsg,
			      iotype_len, child_iomsg_len);
	  dtp->u.p.current_unit->child_dtio--;

	  if (f->u.udf.string_len != 0)
	    free (iotype);
	  /* Note: vlist is freed in free_format_data.  */
	  break;

	case FMT_E:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  if (f->u.real.w == 0)
	    write_real_w0 (dtp, p, kind, f);
	  else
	    write_e (dtp, f, p, kind);
	  break;

	case FMT_EN:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  if (f->u.real.w == 0)
	    write_real_w0 (dtp, p, kind, f);
	  else
	    write_en (dtp, f, p, kind);
	  break;

	case FMT_ES:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  if (f->u.real.w == 0)
	    write_real_w0 (dtp, p, kind, f);
	  else
	    write_es (dtp, f, p, kind);
	  break;

	case FMT_F:
	  if (n == 0)
	    goto need_data;
	  if (require_type (dtp, BT_REAL, type, f))
	    return;
	  write_f (dtp, f, p, kind);
	  break;

	case FMT_G:
	  if (n == 0)
	    goto need_data;
	  switch (type)
	    {
	      case BT_INTEGER:
		write_i (dtp, f, p, kind);
		break;
	      case BT_LOGICAL:
		write_l (dtp, f, p, kind);
		break;
	      case BT_CHARACTER:
		if (kind == 4)
		  write_a_char4 (dtp, f, p, size);
		else
		  write_a (dtp, f, p, size);
		break;
	      case BT_REAL:
		if (f->u.real.w == 0)
		  write_real_w0 (dtp, p, kind, f);
		else
		  write_d (dtp, f, p, kind);
		break;
	      default:
		internal_error (&dtp->common,
				"formatted_transfer (): Bad type");
	    }
	  break;

	case FMT_STRING:
	  consume_data_flag = 0;
	  write_constant_string (dtp, f);
	  break;

	/* Format codes that don't transfer data.  */
	case FMT_X:
	case FMT_TR:
	  consume_data_flag = 0;

	  dtp->u.p.skips += f->u.n;
	  pos = bytes_used + dtp->u.p.skips - 1;
	  dtp->u.p.pending_spaces = pos - dtp->u.p.max_pos + 1;
	  /* Writes occur just before the switch on f->format, above, so
	     that trailing blanks are suppressed, unless we are doing a
	     non-advancing write in which case we want to output the blanks
	     now.  */
	  if (dtp->u.p.advance_status == ADVANCE_NO)
	    {
	      write_x (dtp, dtp->u.p.skips, dtp->u.p.pending_spaces);
	      dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
	    }
	  break;

	case FMT_TL:
	case FMT_T:
	  consume_data_flag = 0;

	  if (f->format == FMT_TL)
	    {

	      /* Handle the special case when no bytes have been used yet.
	         Cannot go below zero. */
	      if (bytes_used == 0)
		{
		  dtp->u.p.pending_spaces -= f->u.n;
		  dtp->u.p.skips -= f->u.n;
		  dtp->u.p.skips = dtp->u.p.skips < 0 ? 0 : dtp->u.p.skips;
		}

	      pos = bytes_used - f->u.n;
	    }
	  else /* FMT_T */
	    pos = f->u.n - dtp->u.p.pending_spaces - 1;

	  /* Standard 10.6.1.1: excessive left tabbing is reset to the
	     left tab limit.  We do not check if the position has gone
	     beyond the end of record because a subsequent tab could
	     bring us back again.  */
	  pos = pos < 0 ? 0 : pos;

	  dtp->u.p.skips = dtp->u.p.skips + pos - bytes_used;
	  dtp->u.p.pending_spaces = dtp->u.p.pending_spaces
				    + pos - dtp->u.p.max_pos;
	  dtp->u.p.pending_spaces = dtp->u.p.pending_spaces < 0
				    ? 0 : dtp->u.p.pending_spaces;
	  break;

	case FMT_S:
	  consume_data_flag = 0;
	  dtp->u.p.sign_status = SIGN_PROCDEFINED;
	  break;

	case FMT_SS:
	  consume_data_flag = 0;
	  dtp->u.p.sign_status = SIGN_SUPPRESS;
	  break;

	case FMT_SP:
	  consume_data_flag = 0;
	  dtp->u.p.sign_status = SIGN_PLUS;
	  break;

	case FMT_BN:
	  consume_data_flag = 0 ;
	  dtp->u.p.blank_status = BLANK_NULL;
	  break;

	case FMT_BZ:
	  consume_data_flag = 0;
	  dtp->u.p.blank_status = BLANK_ZERO;
	  break;

	case FMT_DC:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->decimal_status = DECIMAL_COMMA;
	  break;

	case FMT_DP:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->decimal_status = DECIMAL_POINT;
	  break;

	case FMT_RC:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_COMPATIBLE;
	  break;

	case FMT_RD:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_DOWN;
	  break;

	case FMT_RN:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_NEAREST;
	  break;

	case FMT_RP:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_PROCDEFINED;
	  break;

	case FMT_RU:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_UP;
	  break;

	case FMT_RZ:
	  consume_data_flag = 0;
	  dtp->u.p.current_unit->round_status = ROUND_ZERO;
	  break;

	case FMT_P:
	  consume_data_flag = 0;
	  dtp->u.p.scale_factor = f->u.k;
	  break;

	case FMT_DOLLAR:
	  consume_data_flag = 0;
	  dtp->u.p.seen_dollar = 1;
	  break;

	case FMT_SLASH:
	  consume_data_flag = 0;
	  dtp->u.p.skips = dtp->u.p.pending_spaces = 0;
	  next_record (dtp, 0);
	  break;

	case FMT_COLON:
	  /* A colon descriptor causes us to exit this loop (in
	     particular preventing another / descriptor from being
	     processed) unless there is another data item to be
	     transferred.  */
	  consume_data_flag = 0;
	  if (n == 0)
	    return;
	  break;

	default:
	  internal_error (&dtp->common, "Bad format node");
	}

      /* Adjust the item count and data pointer.  */

      if ((consume_data_flag > 0) && (n > 0))
	{
	  n--;
	  p = ((char *) p) + size;
	}

      pos = dtp->u.p.current_unit->recl - dtp->u.p.current_unit->bytes_left;
      dtp->u.p.max_pos = (dtp->u.p.max_pos > pos) ? dtp->u.p.max_pos : pos;
    }

  return;

  /* Come here when we need a data descriptor but don't have one.  We
     push the current format node back onto the input, then return and
     let the user program call us back with the data.  */
 need_data:
  unget_format (dtp, f);
}

  /* This function is first called from data_init_transfer to initiate the loop
     over each item in the format, transferring data as required.  Subsequent
     calls to this function occur for each data item foound in the READ/WRITE
     statement.  The item_count is incremented for each call.  Since the first
     call is from data_transfer_init, the item_count is always one greater than
     the actual count number of the item being transferred.  */

static void
formatted_transfer (st_parameter_dt *dtp, bt type, void *p, int kind,
		    size_t size, size_t nelems)
{
  size_t elem;
  char *tmp;

  tmp = (char *) p;
  size_t stride = type == BT_CHARACTER ?
		  size * GFC_SIZE_OF_CHAR_KIND(kind) : size;
  if (dtp->u.p.mode == READING)
    {
      /* Big loop over all the elements.  */
      for (elem = 0; elem < nelems; elem++)
	{
	  dtp->u.p.item_count++;
	  formatted_transfer_scalar_read (dtp, type, tmp + stride*elem, kind, size);
	}
    }
  else
    {
      /* Big loop over all the elements.  */
      for (elem = 0; elem < nelems; elem++)
	{
	  dtp->u.p.item_count++;
	  formatted_transfer_scalar_write (dtp, type, tmp + stride*elem, kind, size);
	}
    }
}

/* Wrapper function for I/O of scalar types.  If this should be an async I/O
   request, queue it.  For a synchronous write on an async unit, perform the
   wait operation and return an error.  For all synchronous writes, call the
   right transfer function.  */

static void
wrap_scalar_transfer (st_parameter_dt *dtp, bt type, void *p, int kind,
		      size_t size, size_t n_elem)
{
  if (dtp->u.p.current_unit && dtp->u.p.current_unit->au)
    {
      if (dtp->u.p.async)
	{
	  transfer_args args;
	  args.scalar.transfer = dtp->u.p.transfer;
	  args.scalar.arg_bt = type;
	  args.scalar.data = p;
	  args.scalar.i = kind;
	  args.scalar.s1 = size;
	  args.scalar.s2 = n_elem;
	  enqueue_transfer (dtp->u.p.current_unit->au, &args,
			    AIO_TRANSFER_SCALAR);
	  return;
	}
    }
  /* Come here if there was no asynchronous I/O to be scheduled.  */
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  dtp->u.p.transfer (dtp, type, p, kind, size, 1);
}


/* Data transfer entry points.  The type of the data entity is
   implicit in the subroutine call.  This prevents us from having to
   share a common enum with the compiler.  */

void
transfer_integer (st_parameter_dt *dtp, void *p, int kind)
{
    wrap_scalar_transfer (dtp, BT_INTEGER, p, kind, kind, 1);
}

void
transfer_integer_write (st_parameter_dt *dtp, void *p, int kind)
{
  transfer_integer (dtp, p, kind);
}

void
transfer_real (st_parameter_dt *dtp, void *p, int kind)
{
  size_t size;
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;
  size = size_from_real_kind (kind);
  wrap_scalar_transfer (dtp, BT_REAL, p, kind, size, 1);
}

void
transfer_real_write (st_parameter_dt *dtp, void *p, int kind)
{
  transfer_real (dtp, p, kind);
}

void
transfer_logical (st_parameter_dt *dtp, void *p, int kind)
{
  wrap_scalar_transfer (dtp, BT_LOGICAL, p, kind, kind, 1);
}

void
transfer_logical_write (st_parameter_dt *dtp, void *p, int kind)
{
  transfer_logical (dtp, p, kind);
}

void
transfer_character (st_parameter_dt *dtp, void *p, gfc_charlen_type len)
{
  static char *empty_string[0];

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  /* Strings of zero length can have p == NULL, which confuses the
     transfer routines into thinking we need more data elements.  To avoid
     this, we give them a nice pointer.  */
  if (len == 0 && p == NULL)
    p = empty_string;

  /* Set kind here to 1.  */
  wrap_scalar_transfer (dtp, BT_CHARACTER, p, 1, len, 1);
}

void
transfer_character_write (st_parameter_dt *dtp, void *p, gfc_charlen_type len)
{
  transfer_character (dtp, p, len);
}

void
transfer_character_wide (st_parameter_dt *dtp, void *p, gfc_charlen_type len, int kind)
{
  static char *empty_string[0];

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  /* Strings of zero length can have p == NULL, which confuses the
     transfer routines into thinking we need more data elements.  To avoid
     this, we give them a nice pointer.  */
  if (len == 0 && p == NULL)
    p = empty_string;

  /* Here we pass the actual kind value.  */
  wrap_scalar_transfer (dtp, BT_CHARACTER, p, kind, len, 1);
}

void
transfer_character_wide_write (st_parameter_dt *dtp, void *p, gfc_charlen_type len, int kind)
{
  transfer_character_wide (dtp, p, len, kind);
}

void
transfer_complex (st_parameter_dt *dtp, void *p, int kind)
{
  size_t size;
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;
  size = size_from_complex_kind (kind);
  wrap_scalar_transfer (dtp, BT_COMPLEX, p, kind, size, 1);
}

void
transfer_complex_write (st_parameter_dt *dtp, void *p, int kind)
{
  transfer_complex (dtp, p, kind);
}

void
transfer_array_inner (st_parameter_dt *dtp, gfc_array_char *desc, int kind,
		      gfc_charlen_type charlen)
{
  index_type count[GFC_MAX_DIMENSIONS];
  index_type extent[GFC_MAX_DIMENSIONS];
  index_type stride[GFC_MAX_DIMENSIONS];
  index_type stride0, rank, size, n;
  size_t tsize;
  char *data;
  bt iotype;

  /* Adjust item_count before emitting error message.  */

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  iotype = (bt) GFC_DESCRIPTOR_TYPE (desc);
  size = iotype == BT_CHARACTER ? charlen : GFC_DESCRIPTOR_SIZE (desc);

  rank = GFC_DESCRIPTOR_RANK (desc);

  for (n = 0; n < rank; n++)
    {
      count[n] = 0;
      stride[n] = GFC_DESCRIPTOR_STRIDE_BYTES(desc,n);
      extent[n] = GFC_DESCRIPTOR_EXTENT(desc,n);

      /* If the extent of even one dimension is zero, then the entire
	 array section contains zero elements, so we return after writing
	 a zero array record.  */
      if (extent[n] <= 0)
	{
	  data = NULL;
	  tsize = 0;
	  dtp->u.p.transfer (dtp, iotype, data, kind, size, tsize);
	  return;
	}
    }

  stride0 = stride[0];

  /* If the innermost dimension has a stride of 1, we can do the transfer
     in contiguous chunks.  */
  if (stride0 == size)
    tsize = extent[0];
  else
    tsize = 1;

  data = GFC_DESCRIPTOR_DATA (desc);

  /* When reading, we need to check endfile conditions so we do not miss
     an END=label.  Make this separate so we do not have an extra test
     in a tight loop when it is not needed.  */

  if (dtp->u.p.current_unit && dtp->u.p.mode == READING)
    {
      while (data)
	{
	  if (unlikely (dtp->u.p.current_unit->endfile == AFTER_ENDFILE))
	    return;

	  dtp->u.p.transfer (dtp, iotype, data, kind, size, tsize);
	  data += stride0 * tsize;
	  count[0] += tsize;
	  n = 0;
	  while (count[n] == extent[n])
	    {
	      count[n] = 0;
	      data -= stride[n] * extent[n];
	      n++;
	      if (n == rank)
		{
		  data = NULL;
		  break;
		}
	      else
		{
		  count[n]++;
		  data += stride[n];
		}
	    }
	}
    }
  else
    {
      while (data)
	{
	  dtp->u.p.transfer (dtp, iotype, data, kind, size, tsize);
	  data += stride0 * tsize;
	  count[0] += tsize;
	  n = 0;
	  while (count[n] == extent[n])
	    {
	      count[n] = 0;
	      data -= stride[n] * extent[n];
	      n++;
	      if (n == rank)
		{
		  data = NULL;
		  break;
		}
	      else
		{
		  count[n]++;
		  data += stride[n];
		}
	    }
	}
    }
}

void
transfer_array (st_parameter_dt *dtp, gfc_array_char *desc, int kind,
	        gfc_charlen_type charlen)
{
  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  if (dtp->u.p.current_unit && dtp->u.p.current_unit->au)
    {
      if (dtp->u.p.async)
	{
	  transfer_args args;
	  size_t sz = sizeof (gfc_array_char)
			+ sizeof (descriptor_dimension)
       			* GFC_DESCRIPTOR_RANK (desc);
	  args.array.desc = xmalloc (sz);
	  NOTE ("desc = %p", (void *) args.array.desc);
	  memcpy (args.array.desc, desc, sz);
	  args.array.kind = kind;
	  args.array.charlen = charlen;
	  enqueue_transfer (dtp->u.p.current_unit->au, &args,
			    AIO_TRANSFER_ARRAY);
	  return;
	}
    }
  /* Come here if there was no asynchronous I/O to be scheduled.  */
  transfer_array_inner (dtp, desc, kind, charlen);
}


void
transfer_array_write (st_parameter_dt *dtp, gfc_array_char *desc, int kind,
		      gfc_charlen_type charlen)
{
  transfer_array (dtp, desc, kind, charlen);
}


/* User defined input/output iomsg. */

#define IOMSG_LEN 256

void
transfer_derived (st_parameter_dt *parent, void *dtio_source, void *dtio_proc)
{
  if (parent->u.p.current_unit)
    {
      if (parent->u.p.current_unit->flags.form == FORM_UNFORMATTED)
	parent->u.p.ufdtio_ptr = (unformatted_dtio) dtio_proc;
      else
	parent->u.p.fdtio_ptr = (formatted_dtio) dtio_proc;
    }
  wrap_scalar_transfer (parent, BT_CLASS, dtio_source, 0, 0, 1);
}


/* Preposition a sequential unformatted file while reading.  */

static void
us_read (st_parameter_dt *dtp, int continued)
{
  ssize_t n, nr;
  GFC_INTEGER_4 i4;
  GFC_INTEGER_8 i8;
  gfc_offset i;

  if (compile_options.record_marker == 0)
    n = sizeof (GFC_INTEGER_4);
  else
    n = compile_options.record_marker;

  nr = sread (dtp->u.p.current_unit->s, &i, n);
  if (unlikely (nr < 0))
    {
      generate_error (&dtp->common, LIBERROR_BAD_US, NULL);
      return;
    }
  else if (nr == 0)
    {
      hit_eof (dtp);
      return;  /* end of file */
    }
  else if (unlikely (n != nr))
    {
      generate_error (&dtp->common, LIBERROR_BAD_US, NULL);
      return;
    }

  int convert = dtp->u.p.current_unit->flags.convert;
#ifdef HAVE_GFC_REAL_17
  convert &= ~(GFC_CONVERT_R16_IEEE | GFC_CONVERT_R16_IBM);
#endif
  /* Only GFC_CONVERT_NATIVE and GFC_CONVERT_SWAP are valid here.  */
  if (likely (convert == GFC_CONVERT_NATIVE))
    {
      switch (nr)
	{
	case sizeof(GFC_INTEGER_4):
	  memcpy (&i4, &i, sizeof (i4));
	  i = i4;
	  break;

	case sizeof(GFC_INTEGER_8):
	  memcpy (&i8, &i, sizeof (i8));
	  i = i8;
	  break;

	default:
	  runtime_error ("Illegal value for record marker");
	  break;
	}
    }
  else
    {
      uint32_t u32;
      uint64_t u64;
      switch (nr)
	{
	case sizeof(GFC_INTEGER_4):
	  memcpy (&u32, &i, sizeof (u32));
	  u32 = __builtin_bswap32 (u32);
	  memcpy (&i4, &u32, sizeof (i4));
	  i = i4;
	  break;

	case sizeof(GFC_INTEGER_8):
	  memcpy (&u64, &i, sizeof (u64));
	  u64 = __builtin_bswap64 (u64);
	  memcpy (&i8, &u64, sizeof (i8));
	  i = i8;
	  break;

	default:
	  runtime_error ("Illegal value for record marker");
	  break;
	}
    }

  if (i >= 0)
    {
      dtp->u.p.current_unit->bytes_left_subrecord = i;
      dtp->u.p.current_unit->continued = 0;
    }
  else
    {
      dtp->u.p.current_unit->bytes_left_subrecord = -i;
      dtp->u.p.current_unit->continued = 1;
    }

  if (! continued)
    dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
}


/* Preposition a sequential unformatted file while writing.  This
   amount to writing a bogus length that will be filled in later.  */

static void
us_write (st_parameter_dt *dtp, int continued)
{
  ssize_t nbytes;
  gfc_offset dummy;

  dummy = 0;

  if (compile_options.record_marker == 0)
    nbytes = sizeof (GFC_INTEGER_4);
  else
    nbytes = compile_options.record_marker ;

  if (swrite (dtp->u.p.current_unit->s, &dummy, nbytes) != nbytes)
    generate_error (&dtp->common, LIBERROR_OS, NULL);

  /* For sequential unformatted, if RECL= was not specified in the OPEN
     we write until we have more bytes than can fit in the subrecord
     markers, then we write a new subrecord.  */

  dtp->u.p.current_unit->bytes_left_subrecord =
    dtp->u.p.current_unit->recl_subrecord;
  dtp->u.p.current_unit->continued = continued;
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
    case FORMATTED_STREAM:
    case UNFORMATTED_STREAM:
      /* There are no records with stream I/O.  If the position was specified
	 data_transfer_init has already positioned the file. If no position
	 was specified, we continue from where we last left off.  I.e.
	 there is nothing to do here.  */
      break;

    case UNFORMATTED_SEQUENTIAL:
      if (dtp->u.p.mode == READING)
	us_read (dtp, 0);
      else
	us_write (dtp, 0);

      break;

    case FORMATTED_SEQUENTIAL:
    case FORMATTED_DIRECT:
    case UNFORMATTED_DIRECT:
      dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
      break;
    case FORMATTED_UNSPECIFIED:
      gcc_unreachable ();
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
  async_unit *au;

  NOTE ("data_transfer_init");

  ionml = ((cf & IOPARM_DT_IONML_SET) != 0) ? dtp->u.p.ionml : NULL;

  memset (&dtp->u.p, 0, sizeof (dtp->u.p));

  dtp->u.p.ionml = ionml;
  dtp->u.p.mode = read_flag ? READING : WRITING;
  dtp->u.p.namelist_mode = 0;
  dtp->u.p.cc.len = 0;

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    return;

  dtp->u.p.current_unit = get_unit (dtp, 1);

  if (dtp->u.p.current_unit == NULL)
    {
      /* This means we tried to access an external unit < 0 without
	 having opened it first with NEWUNIT=.  */
      generate_error (&dtp->common, LIBERROR_BAD_OPTION,
		      "Unit number is negative and unit was not already "
		      "opened with OPEN(NEWUNIT=...)");
      return;
    }
  else if (dtp->u.p.current_unit->s == NULL)
    {  /* Open the unit with some default flags.  */
      st_parameter_open opp;
      unit_convert conv;
      NOTE ("Open the unit with some default flags.");
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
      u_flags.decimal = DECIMAL_UNSPECIFIED;
      u_flags.encoding = ENCODING_UNSPECIFIED;
      u_flags.async = ASYNC_UNSPECIFIED;
      u_flags.round = ROUND_UNSPECIFIED;
      u_flags.sign = SIGN_UNSPECIFIED;
      u_flags.share = SHARE_UNSPECIFIED;
      u_flags.cc = CC_UNSPECIFIED;
      u_flags.readonly = 0;

      u_flags.status = STATUS_UNKNOWN;

      conv = get_unformatted_convert (dtp->common.unit);

      if (conv == GFC_CONVERT_NONE)
	conv = compile_options.convert;

      u_flags.convert = 0;

#ifdef HAVE_GFC_REAL_17
      u_flags.convert = conv & (GFC_CONVERT_R16_IEEE | GFC_CONVERT_R16_IBM);
      conv &= ~(GFC_CONVERT_R16_IEEE | GFC_CONVERT_R16_IBM);
#endif

      switch (conv)
	{
	case GFC_CONVERT_NATIVE:
	case GFC_CONVERT_SWAP:
	  break;

	case GFC_CONVERT_BIG:
	  conv = __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__ ? GFC_CONVERT_NATIVE : GFC_CONVERT_SWAP;
	  break;

	case GFC_CONVERT_LITTLE:
	  conv = __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__ ? GFC_CONVERT_SWAP : GFC_CONVERT_NATIVE;
	  break;

	default:
	  internal_error (&opp.common, "Illegal value for CONVERT");
	  break;
	}

      u_flags.convert |= conv;

      opp.common = dtp->common;
      opp.common.flags &= IOPARM_COMMON_MASK;
      dtp->u.p.current_unit = new_unit (&opp, dtp->u.p.current_unit, &u_flags);
      dtp->common.flags &= ~IOPARM_COMMON_MASK;
      dtp->common.flags |= (opp.common.flags & IOPARM_COMMON_MASK);
      if (dtp->u.p.current_unit == NULL)
	return;
    }

  if (dtp->u.p.current_unit->child_dtio == 0)
    {
      if ((cf & IOPARM_DT_HAS_SIZE) != 0)
	{
	  dtp->u.p.current_unit->has_size = true;
	  /* Initialize the count.  */
	  dtp->u.p.current_unit->size_used = 0;
	}
      else
	dtp->u.p.current_unit->has_size = false;
    }
  else if (dtp->u.p.current_unit->internal_unit_kind > 0)
    dtp->u.p.unit_is_internal = 1;

  if ((cf & IOPARM_DT_HAS_ASYNCHRONOUS) != 0)
    {
      int f;
      f = find_option (&dtp->common, dtp->asynchronous, dtp->asynchronous_len,
		       async_opt, "Bad ASYNCHRONOUS in data transfer "
		       "statement");
      if (f == ASYNC_YES && dtp->u.p.current_unit->flags.async != ASYNC_YES)
	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			  "ASYNCHRONOUS transfer without "
			  "ASYHCRONOUS='YES' in OPEN");
	  return;
	}
      dtp->u.p.async = f == ASYNC_YES;
    }

  au = dtp->u.p.current_unit->au;
  if (au)
    {
      if (dtp->u.p.async)
	{
	  /* If this is an asynchronous I/O statement, collect errors and
	     return if there are any.  */
	  if (collect_async_errors (&dtp->common, au))
	    return;
	}
      else
	{
	  /* Synchronous statement: Perform a wait operation for any pending
	     asynchronous I/O.  This needs to be done before all other error
	     checks.  See F2008, 9.6.4.1.  */
	  if (async_wait (&(dtp->common), au))
	    return;
	}
    }

  /* Check the action.  */

  if (read_flag && dtp->u.p.current_unit->flags.action == ACTION_WRITE)
    {
      generate_error (&dtp->common, LIBERROR_BAD_ACTION,
		      "Cannot read from file opened for WRITE");
      return;
    }

  if (!read_flag && dtp->u.p.current_unit->flags.action == ACTION_READ)
    {
      generate_error (&dtp->common, LIBERROR_BAD_ACTION,
		      "Cannot write to file opened for READ");
      return;
    }

  dtp->u.p.first_item = 1;

  /* Check the format.  */

  if ((cf & IOPARM_DT_HAS_FORMAT) != 0)
    parse_format (dtp);

  if (dtp->u.p.current_unit->flags.form == FORM_UNFORMATTED
      && (cf & (IOPARM_DT_HAS_FORMAT | IOPARM_DT_LIST_FORMAT))
	 != 0)
    {
      generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
		      "Format present for UNFORMATTED data transfer");
      return;
    }

  if ((cf & IOPARM_DT_HAS_NAMELIST_NAME) != 0 && dtp->u.p.ionml != NULL)
     {
	if ((cf & IOPARM_DT_HAS_FORMAT) != 0)
	  {
	    generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			"A format cannot be specified with a namelist");
	    return;
	  }
     }
  else if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED &&
	   !(cf & (IOPARM_DT_HAS_FORMAT | IOPARM_DT_LIST_FORMAT)))
    {
      generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
		      "Missing format for FORMATTED data transfer");
      return;
    }

  if (is_internal_unit (dtp)
      && dtp->u.p.current_unit->flags.form == FORM_UNFORMATTED)
    {
      generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
		      "Internal file cannot be accessed by UNFORMATTED "
		      "data transfer");
      return;
    }

  /* Check the record or position number.  */

  if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT
      && (cf & IOPARM_DT_HAS_REC) == 0)
    {
      generate_error (&dtp->common, LIBERROR_MISSING_OPTION,
		      "Direct access data transfer requires record number");
      return;
    }

  if (dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
    {
      if ((cf & IOPARM_DT_HAS_REC) != 0)
	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			"Record number not allowed for sequential access "
			"data transfer");
	  return;
	}

      if (compile_options.warn_std &&
	  dtp->u.p.current_unit->endfile == AFTER_ENDFILE)
      	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			"Sequential READ or WRITE not allowed after "
			"EOF marker, possibly use REWIND or BACKSPACE");
	  return;
	}
    }

  /* Process the ADVANCE option.  */

  dtp->u.p.advance_status
    = !(cf & IOPARM_DT_HAS_ADVANCE) ? ADVANCE_UNSPECIFIED :
      find_option (&dtp->common, dtp->advance, dtp->advance_len, advance_opt,
		   "Bad ADVANCE parameter in data transfer statement");

  if (dtp->u.p.advance_status != ADVANCE_UNSPECIFIED)
    {
      if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT)
	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			  "ADVANCE specification conflicts with sequential "
			  "access");
	  return;
	}

      if (is_internal_unit (dtp))
	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			  "ADVANCE specification conflicts with internal file");
	  return;
	}

      if ((cf & (IOPARM_DT_HAS_FORMAT | IOPARM_DT_LIST_FORMAT))
	  != IOPARM_DT_HAS_FORMAT)
	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			  "ADVANCE specification requires an explicit format");
	  return;
	}
    }

  /* Child IO is non-advancing and any ADVANCE= specifier is ignored.
     F2008 9.6.2.4  */
  if (dtp->u.p.current_unit->child_dtio  > 0)
    dtp->u.p.advance_status = ADVANCE_NO;

  if (read_flag)
    {
      dtp->u.p.current_unit->previous_nonadvancing_write = 0;

      if ((cf & IOPARM_EOR) != 0 && dtp->u.p.advance_status != ADVANCE_NO)
	{
	  generate_error (&dtp->common, LIBERROR_MISSING_OPTION,
			  "EOR specification requires an ADVANCE specification "
			  "of NO");
	  return;
	}

      if ((cf & IOPARM_DT_HAS_SIZE) != 0
	  && dtp->u.p.advance_status != ADVANCE_NO)
	{
	  generate_error (&dtp->common, LIBERROR_MISSING_OPTION,
			  "SIZE specification requires an ADVANCE "
			  "specification of NO");
	  return;
	}
    }
  else
    {				/* Write constraints.  */
      if ((cf & IOPARM_END) != 0)
	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			  "END specification cannot appear in a write "
			  "statement");
	  return;
	}

      if ((cf & IOPARM_EOR) != 0)
	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			  "EOR specification cannot appear in a write "
			  "statement");
	  return;
	}

      if ((cf & IOPARM_DT_HAS_SIZE) != 0)
	{
	  generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			  "SIZE specification cannot appear in a write "
			  "statement");
	  return;
	}
    }

  if (dtp->u.p.advance_status == ADVANCE_UNSPECIFIED)
    dtp->u.p.advance_status = ADVANCE_YES;

  /* Check the decimal mode.  */
  dtp->u.p.current_unit->decimal_status
	= !(cf & IOPARM_DT_HAS_DECIMAL) ? DECIMAL_UNSPECIFIED :
	  find_option (&dtp->common, dtp->decimal, dtp->decimal_len,
			decimal_opt, "Bad DECIMAL parameter in data transfer "
			"statement");

  if (dtp->u.p.current_unit->decimal_status == DECIMAL_UNSPECIFIED)
	dtp->u.p.current_unit->decimal_status = dtp->u.p.current_unit->flags.decimal;

  /* Check the round mode.  */
  dtp->u.p.current_unit->round_status
	= !(cf & IOPARM_DT_HAS_ROUND) ? ROUND_UNSPECIFIED :
	  find_option (&dtp->common, dtp->round, dtp->round_len,
			round_opt, "Bad ROUND parameter in data transfer "
			"statement");

  if (dtp->u.p.current_unit->round_status == ROUND_UNSPECIFIED)
	dtp->u.p.current_unit->round_status = dtp->u.p.current_unit->flags.round;

  /* Check the sign mode. */
  dtp->u.p.sign_status
	= !(cf & IOPARM_DT_HAS_SIGN) ? SIGN_UNSPECIFIED :
	  find_option (&dtp->common, dtp->sign, dtp->sign_len, sign_opt,
			"Bad SIGN parameter in data transfer statement");

  if (dtp->u.p.sign_status == SIGN_UNSPECIFIED)
	dtp->u.p.sign_status = dtp->u.p.current_unit->flags.sign;

  /* Check the blank mode.  */
  dtp->u.p.blank_status
	= !(cf & IOPARM_DT_HAS_BLANK) ? BLANK_UNSPECIFIED :
	  find_option (&dtp->common, dtp->blank, dtp->blank_len,
			blank_opt,
			"Bad BLANK parameter in data transfer statement");

  if (dtp->u.p.blank_status == BLANK_UNSPECIFIED)
	dtp->u.p.blank_status = dtp->u.p.current_unit->flags.blank;

  /* Check the delim mode.  */
  dtp->u.p.current_unit->delim_status
	= !(cf & IOPARM_DT_HAS_DELIM) ? DELIM_UNSPECIFIED :
	  find_option (&dtp->common, dtp->delim, dtp->delim_len,
	  delim_opt, "Bad DELIM parameter in data transfer statement");

  if (dtp->u.p.current_unit->delim_status == DELIM_UNSPECIFIED)
    {
      if (ionml && dtp->u.p.current_unit->flags.delim == DELIM_UNSPECIFIED)
	dtp->u.p.current_unit->delim_status = DELIM_QUOTE;
      else
	dtp->u.p.current_unit->delim_status = dtp->u.p.current_unit->flags.delim;
    }

  /* Check the pad mode.  */
  dtp->u.p.current_unit->pad_status
	= !(cf & IOPARM_DT_HAS_PAD) ? PAD_UNSPECIFIED :
	  find_option (&dtp->common, dtp->pad, dtp->pad_len, pad_opt,
			"Bad PAD parameter in data transfer statement");

  if (dtp->u.p.current_unit->pad_status == PAD_UNSPECIFIED)
	dtp->u.p.current_unit->pad_status = dtp->u.p.current_unit->flags.pad;

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

  if (au && dtp->u.p.async)
    {
      NOTE ("enqueue_data_transfer");
      enqueue_data_transfer_init (au, dtp, read_flag);
    }
  else
    {
      NOTE ("invoking data_transfer_init_worker");
      data_transfer_init_worker (dtp, read_flag);
    }
}

void
data_transfer_init_worker (st_parameter_dt *dtp, int read_flag)
{
  GFC_INTEGER_4 cf = dtp->common.flags;

  NOTE ("starting worker...");

  if (read_flag && dtp->u.p.current_unit->flags.form != FORM_UNFORMATTED
      && ((cf & IOPARM_DT_LIST_FORMAT) != 0)
      && dtp->u.p.current_unit->child_dtio  == 0)
    dtp->u.p.current_unit->last_char = EOF - 1;

  /* Check to see if we might be reading what we wrote before  */

  if (dtp->u.p.mode != dtp->u.p.current_unit->mode
      && !is_internal_unit (dtp))
    {
      int pos = fbuf_reset (dtp->u.p.current_unit);
      if (pos != 0)
        sseek (dtp->u.p.current_unit->s, pos, SEEK_CUR);
      sflush(dtp->u.p.current_unit->s);
    }

  /* Check the POS= specifier: that it is in range and that it is used with a
     unit that has been connected for STREAM access. F2003 9.5.1.10.  */

  if (((cf & IOPARM_DT_HAS_POS) != 0))
    {
      if (is_stream_io (dtp))
        {

          if (dtp->pos <= 0)
            {
              generate_error (&dtp->common, LIBERROR_BAD_OPTION,
                              "POS=specifier must be positive");
              return;
            }

          if (dtp->pos >= dtp->u.p.current_unit->maxrec)
            {
              generate_error (&dtp->common, LIBERROR_BAD_OPTION,
                              "POS=specifier too large");
              return;
            }

          dtp->rec = dtp->pos;

          if (dtp->u.p.mode == READING)
            {
              /* Reset the endfile flag; if we hit EOF during reading
                 we'll set the flag and generate an error at that point
                 rather than worrying about it here.  */
              dtp->u.p.current_unit->endfile = NO_ENDFILE;
            }

          if (dtp->pos != dtp->u.p.current_unit->strm_pos)
            {
	      fbuf_reset (dtp->u.p.current_unit);
	      if (sseek (dtp->u.p.current_unit->s, dtp->pos - 1,
			 SEEK_SET) < 0)
                {
                  generate_error (&dtp->common, LIBERROR_OS, NULL);
                  return;
                }
              dtp->u.p.current_unit->strm_pos = dtp->pos;
            }
        }
      else
        {
          generate_error (&dtp->common, LIBERROR_BAD_OPTION,
                          "POS=specifier not allowed, "
                          "Try OPEN with ACCESS='stream'");
          return;
        }
    }


  /* Sanity checks on the record number.  */
  if ((cf & IOPARM_DT_HAS_REC) != 0)
    {
      if (dtp->rec <= 0)
	{
	  generate_error (&dtp->common, LIBERROR_BAD_OPTION,
			  "Record number must be positive");
	  return;
	}

      if (dtp->rec >= dtp->u.p.current_unit->maxrec)
	{
	  generate_error (&dtp->common, LIBERROR_BAD_OPTION,
			  "Record number too large");
	  return;
	}

      /* Make sure format buffer is reset.  */
      if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED)
        fbuf_reset (dtp->u.p.current_unit);


      /* Check whether the record exists to be read.  Only
	 a partial record needs to exist.  */

      if (dtp->u.p.mode == READING && (dtp->rec - 1)
	  * dtp->u.p.current_unit->recl >= ssize (dtp->u.p.current_unit->s))
	{
	  generate_error (&dtp->common, LIBERROR_BAD_OPTION,
			  "Non-existing record number");
	  return;
	}

      /* Position the file.  */
      if (sseek (dtp->u.p.current_unit->s, (gfc_offset) (dtp->rec - 1)
		 * dtp->u.p.current_unit->recl, SEEK_SET) < 0)
	{
	  generate_error (&dtp->common, LIBERROR_OS, NULL);
	  return;
	}

      if (dtp->u.p.current_unit->flags.access == ACCESS_STREAM)
       {
         generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
                     "Record number not allowed for stream access "
                     "data transfer");
         return;
       }
    }

  /* Bugware for badly written mixed C-Fortran I/O.  */
  if (!is_internal_unit (dtp))
    flush_if_preconnected(dtp->u.p.current_unit->s);

  dtp->u.p.current_unit->mode = dtp->u.p.mode;

  /* Set the maximum position reached from the previous I/O operation.  This
     could be greater than zero from a previous non-advancing write.  */
  dtp->u.p.max_pos = dtp->u.p.current_unit->saved_pos;

  pre_position (dtp);

  /* Make sure that we don't do a read after a nonadvancing write.  */

  if (read_flag)
    {
      if (dtp->u.p.current_unit->read_bad && !is_stream_io (dtp))
	{
	  generate_error (&dtp->common, LIBERROR_BAD_OPTION,
			  "Cannot READ after a nonadvancing WRITE");
	  return;
	}
    }
  else
    {
      if (dtp->u.p.advance_status == ADVANCE_YES && !dtp->u.p.seen_dollar)
	dtp->u.p.current_unit->read_bad = 1;
    }

  if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED)
    {
#ifdef HAVE_POSIX_2008_LOCALE
      dtp->u.p.old_locale = uselocale (c_locale);
#else
      __gthread_mutex_lock (&old_locale_lock);
      if (!old_locale_ctr++)
	{
	  old_locale = setlocale (LC_NUMERIC, NULL);
	  setlocale (LC_NUMERIC, "C");
	}
      __gthread_mutex_unlock (&old_locale_lock);
#endif
      /* Start the data transfer if we are doing a formatted transfer.  */
      if ((cf & (IOPARM_DT_LIST_FORMAT | IOPARM_DT_HAS_NAMELIST_NAME)) == 0
	&& dtp->u.p.ionml == NULL)
	formatted_transfer (dtp, 0, NULL, 0, 0, 1);
    }
}


/* Initialize an array_loop_spec given the array descriptor.  The function
   returns the index of the last element of the array, and also returns
   starting record, where the first I/O goes to (necessary in case of
   negative strides).  */

gfc_offset
init_loop_spec (gfc_array_char *desc, array_loop_spec *ls,
		gfc_offset *start_record)
{
  int rank = GFC_DESCRIPTOR_RANK(desc);
  int i;
  gfc_offset index;
  int empty;

  empty = 0;
  index = 1;
  *start_record = 0;

  for (i=0; i<rank; i++)
    {
      ls[i].idx = GFC_DESCRIPTOR_LBOUND(desc,i);
      ls[i].start = GFC_DESCRIPTOR_LBOUND(desc,i);
      ls[i].end = GFC_DESCRIPTOR_UBOUND(desc,i);
      ls[i].step = GFC_DESCRIPTOR_STRIDE(desc,i);
      empty = empty || (GFC_DESCRIPTOR_UBOUND(desc,i)
			< GFC_DESCRIPTOR_LBOUND(desc,i));

      if (GFC_DESCRIPTOR_STRIDE(desc,i) > 0)
	{
	  index += (GFC_DESCRIPTOR_EXTENT(desc,i) - 1)
	    * GFC_DESCRIPTOR_STRIDE(desc,i);
	}
      else
	{
	  index -= (GFC_DESCRIPTOR_EXTENT(desc,i) - 1)
	    * GFC_DESCRIPTOR_STRIDE(desc,i);
	  *start_record -= (GFC_DESCRIPTOR_EXTENT(desc,i) - 1)
	    * GFC_DESCRIPTOR_STRIDE(desc,i);
	}
    }

  if (empty)
    return 0;
  else
    return index;
}

/* Determine the index to the next record in an internal unit array by
   by incrementing through the array_loop_spec.  */

gfc_offset
next_array_record (st_parameter_dt *dtp, array_loop_spec *ls, int *finished)
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
      index = index + (ls[i].idx - ls[i].start) * ls[i].step;
    }

  *finished = carry;

  return index;
}



/* Skip to the end of the current record, taking care of an optional
   record marker of size bytes.  If the file is not seekable, we
   read chunks of size MAX_READ until we get to the right
   position.  */

static void
skip_record (st_parameter_dt *dtp, gfc_offset bytes)
{
  ssize_t rlength, readb;
#define MAX_READ 4096
  char p[MAX_READ];

  dtp->u.p.current_unit->bytes_left_subrecord += bytes;
  if (dtp->u.p.current_unit->bytes_left_subrecord == 0)
    return;

  /* Direct access files do not generate END conditions,
     only I/O errors.  */
  if (sseek (dtp->u.p.current_unit->s,
	     dtp->u.p.current_unit->bytes_left_subrecord, SEEK_CUR) < 0)
    {
      /* Seeking failed, fall back to seeking by reading data.  */
      while (dtp->u.p.current_unit->bytes_left_subrecord > 0)
	{
	  rlength =
	    (MAX_READ < dtp->u.p.current_unit->bytes_left_subrecord) ?
	    MAX_READ : dtp->u.p.current_unit->bytes_left_subrecord;

	  readb = sread (dtp->u.p.current_unit->s, p, rlength);
	  if (readb < 0)
	    {
	      generate_error (&dtp->common, LIBERROR_OS, NULL);
	      return;
	    }

	  dtp->u.p.current_unit->bytes_left_subrecord -= readb;
	}
      return;
    }
  dtp->u.p.current_unit->bytes_left_subrecord = 0;
}


/* Advance to the next record reading unformatted files, taking
   care of subrecords.  If complete_record is nonzero, we loop
   until all subrecords are cleared.  */

static void
next_record_r_unf (st_parameter_dt *dtp, int complete_record)
{
  size_t bytes;

  bytes =  compile_options.record_marker == 0 ?
    sizeof (GFC_INTEGER_4) : compile_options.record_marker;

  while(1)
    {

      /* Skip over tail */

      skip_record (dtp, bytes);

      if ( ! (complete_record && dtp->u.p.current_unit->continued))
	return;

      us_read (dtp, 1);
    }
}


static gfc_offset
min_off (gfc_offset a, gfc_offset b)
{
  return (a < b ? a : b);
}


/* Space to the next record for read mode.  */

static void
next_record_r (st_parameter_dt *dtp, int done)
{
  gfc_offset record;
  char p;
  int cc;

  switch (current_mode (dtp))
    {
    /* No records in unformatted STREAM I/O.  */
    case UNFORMATTED_STREAM:
      return;

    case UNFORMATTED_SEQUENTIAL:
      next_record_r_unf (dtp, 1);
      dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
      break;

    case FORMATTED_DIRECT:
    case UNFORMATTED_DIRECT:
      skip_record (dtp, dtp->u.p.current_unit->bytes_left);
      break;

    case FORMATTED_STREAM:
    case FORMATTED_SEQUENTIAL:
      /* read_sf has already terminated input because of an '\n', or
         we have hit EOF.  */
      if (dtp->u.p.sf_seen_eor)
	{
	  dtp->u.p.sf_seen_eor = 0;
	  break;
	}

      if (is_internal_unit (dtp))
	{
	  if (is_array_io (dtp))
	    {
	      int finished;

	      record = next_array_record (dtp, dtp->u.p.current_unit->ls,
					  &finished);
	      if (!done && finished)
		hit_eof (dtp);

	      /* Now seek to this record.  */
	      record = record * dtp->u.p.current_unit->recl;
	      if (sseek (dtp->u.p.current_unit->s, record, SEEK_SET) < 0)
		{
		  generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
		  break;
		}
	      dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	    }
	  else
	    {
	      gfc_offset bytes_left = dtp->u.p.current_unit->bytes_left;
	      bytes_left = min_off (bytes_left,
		      ssize (dtp->u.p.current_unit->s)
		      - stell (dtp->u.p.current_unit->s));
	      if (sseek (dtp->u.p.current_unit->s,
			 bytes_left, SEEK_CUR) < 0)
	        {
		  generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
		  break;
		}
	      dtp->u.p.current_unit->bytes_left
		= dtp->u.p.current_unit->recl;
	    }
	  break;
	}
      else if (dtp->u.p.current_unit->flags.cc != CC_NONE)
	{
	  do
	    {
              errno = 0;
              cc = fbuf_getc (dtp->u.p.current_unit);
	      if (cc == EOF)
		{
                  if (errno != 0)
                    generate_error (&dtp->common, LIBERROR_OS, NULL);
		  else
		    {
		      if (is_stream_io (dtp)
			  || dtp->u.p.current_unit->pad_status == PAD_NO
			  || dtp->u.p.current_unit->bytes_left
			     == dtp->u.p.current_unit->recl)
			hit_eof (dtp);
		    }
		  break;
                }

	      if (is_stream_io (dtp))
		dtp->u.p.current_unit->strm_pos++;

              p = (char) cc;
	    }
	  while (p != '\n');
	}
      break;
    case FORMATTED_UNSPECIFIED:
      gcc_unreachable ();
    }
}


/* Small utility function to write a record marker, taking care of
   byte swapping and of choosing the correct size.  */

static int
write_us_marker (st_parameter_dt *dtp, const gfc_offset buf)
{
  size_t len;
  GFC_INTEGER_4 buf4;
  GFC_INTEGER_8 buf8;

  if (compile_options.record_marker == 0)
    len = sizeof (GFC_INTEGER_4);
  else
    len = compile_options.record_marker;

  int convert = dtp->u.p.current_unit->flags.convert;
#ifdef HAVE_GFC_REAL_17
  convert &= ~(GFC_CONVERT_R16_IEEE | GFC_CONVERT_R16_IBM);
#endif
  /* Only GFC_CONVERT_NATIVE and GFC_CONVERT_SWAP are valid here.  */
  if (likely (convert == GFC_CONVERT_NATIVE))
    {
      switch (len)
	{
	case sizeof (GFC_INTEGER_4):
	  buf4 = buf;
	  return swrite (dtp->u.p.current_unit->s, &buf4, len);
	  break;

	case sizeof (GFC_INTEGER_8):
	  buf8 = buf;
	  return swrite (dtp->u.p.current_unit->s, &buf8, len);
	  break;

	default:
	  runtime_error ("Illegal value for record marker");
	  break;
	}
    }
  else
    {
      uint32_t u32;
      uint64_t u64;
      switch (len)
	{
	case sizeof (GFC_INTEGER_4):
	  buf4 = buf;
	  memcpy (&u32, &buf4, sizeof (u32));
	  u32 = __builtin_bswap32 (u32);
	  return swrite (dtp->u.p.current_unit->s, &u32, len);
	  break;

	case sizeof (GFC_INTEGER_8):
	  buf8 = buf;
	  memcpy (&u64, &buf8, sizeof (u64));
	  u64 = __builtin_bswap64 (u64);
	  return swrite (dtp->u.p.current_unit->s, &u64, len);
	  break;

	default:
	  runtime_error ("Illegal value for record marker");
	  break;
	}
    }

}

/* Position to the next (sub)record in write mode for
   unformatted sequential files.  */

static void
next_record_w_unf (st_parameter_dt *dtp, int next_subrecord)
{
  gfc_offset m, m_write, record_marker;

  /* Bytes written.  */
  m = dtp->u.p.current_unit->recl_subrecord
    - dtp->u.p.current_unit->bytes_left_subrecord;

  if (compile_options.record_marker == 0)
    record_marker = sizeof (GFC_INTEGER_4);
  else
    record_marker = compile_options.record_marker;

  /* Seek to the head and overwrite the bogus length with the real
     length.  */

  if (unlikely (sseek (dtp->u.p.current_unit->s, - m - record_marker,
		       SEEK_CUR) < 0))
    goto io_error;

  if (next_subrecord)
    m_write = -m;
  else
    m_write = m;

  if (unlikely (write_us_marker (dtp, m_write) < 0))
    goto io_error;

  /* Seek past the end of the current record.  */

  if (unlikely (sseek (dtp->u.p.current_unit->s, m, SEEK_CUR) < 0))
    goto io_error;

  /* Write the length tail.  If we finish a record containing
     subrecords, we write out the negative length.  */

  if (dtp->u.p.current_unit->continued)
    m_write = -m;
  else
    m_write = m;

  if (unlikely (write_us_marker (dtp, m_write) < 0))
    goto io_error;

  return;

 io_error:
  generate_error (&dtp->common, LIBERROR_OS, NULL);
  return;

}


/* Utility function like memset() but operating on streams. Return
   value is same as for POSIX write().  */

static gfc_offset
sset (stream *s, int c, gfc_offset nbyte)
{
#define WRITE_CHUNK 256
  char p[WRITE_CHUNK];
  gfc_offset bytes_left;
  ssize_t trans;

  if (nbyte < WRITE_CHUNK)
    memset (p, c, nbyte);
  else
    memset (p, c, WRITE_CHUNK);

  bytes_left = nbyte;
  while (bytes_left > 0)
    {
      trans = (bytes_left < WRITE_CHUNK) ? bytes_left : WRITE_CHUNK;
      trans = swrite (s, p, trans);
      if (trans <= 0)
	return trans;
      bytes_left -= trans;
    }

  return nbyte - bytes_left;
}


/* Finish up a record according to the legacy carriagecontrol type, based
   on the first character in the record.  */

static void
next_record_cc (st_parameter_dt *dtp)
{
  /* Only valid with CARRIAGECONTROL=FORTRAN.  */
  if (dtp->u.p.current_unit->flags.cc != CC_FORTRAN)
    return;

  fbuf_seek (dtp->u.p.current_unit, 0, SEEK_END);
  if (dtp->u.p.cc.len > 0)
    {
      char *p = fbuf_alloc (dtp->u.p.current_unit, dtp->u.p.cc.len);
      if (!p)
	generate_error (&dtp->common, LIBERROR_OS, NULL);

      /* Output CR for the first character with default CC setting.  */
      *(p++) = dtp->u.p.cc.u.end;
      if (dtp->u.p.cc.len > 1)
	*p = dtp->u.p.cc.u.end;
    }
}

/* Position to the next record in write mode.  */

static void
next_record_w (st_parameter_dt *dtp, int done)
{
  gfc_offset max_pos_off;

  /* Zero counters for X- and T-editing.  */
  max_pos_off = dtp->u.p.max_pos;
  dtp->u.p.max_pos = dtp->u.p.skips = dtp->u.p.pending_spaces = 0;

  switch (current_mode (dtp))
    {
    /* No records in unformatted STREAM I/O.  */
    case UNFORMATTED_STREAM:
      return;

    case FORMATTED_DIRECT:
      if (dtp->u.p.current_unit->bytes_left == 0)
	break;

      fbuf_seek (dtp->u.p.current_unit, 0, SEEK_END);
      fbuf_flush (dtp->u.p.current_unit, WRITING);
      if (sset (dtp->u.p.current_unit->s, ' ',
		dtp->u.p.current_unit->bytes_left)
	  != dtp->u.p.current_unit->bytes_left)
	goto io_error;

      break;

    case UNFORMATTED_DIRECT:
      if (dtp->u.p.current_unit->bytes_left > 0)
	{
	  gfc_offset length = dtp->u.p.current_unit->bytes_left;
	  if (sset (dtp->u.p.current_unit->s, 0, length) != length)
	    goto io_error;
	}
      break;

    case UNFORMATTED_SEQUENTIAL:
      next_record_w_unf (dtp, 0);
      dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
      break;

    case FORMATTED_STREAM:
    case FORMATTED_SEQUENTIAL:

      if (is_internal_unit (dtp))
	{
	  char *p;
	  /* Internal unit, so must fit in memory.  */
	  size_t length, m;
	  size_t max_pos = max_pos_off;
	  if (is_array_io (dtp))
	    {
	      int finished;

	      length = dtp->u.p.current_unit->bytes_left;

	      /* If the farthest position reached is greater than current
	      position, adjust the position and set length to pad out
	      whats left.  Otherwise just pad whats left.
	      (for character array unit) */
	      m = dtp->u.p.current_unit->recl
			- dtp->u.p.current_unit->bytes_left;
	      if (max_pos > m)
		{
		  length = (max_pos - m);
		  if (sseek (dtp->u.p.current_unit->s,
			     length, SEEK_CUR) < 0)
		    {
		      generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
		      return;
		    }
		  length = ((size_t) dtp->u.p.current_unit->recl - max_pos);
		}

	      p = write_block (dtp, length);
	      if (p == NULL)
		return;

	      if (unlikely (is_char4_unit (dtp)))
	        {
		  gfc_char4_t *p4 = (gfc_char4_t *) p;
		  memset4 (p4, ' ', length);
		}
	      else
		memset (p, ' ', length);

	      /* Now that the current record has been padded out,
		 determine where the next record in the array is.
		 Note that this can return a negative value, so it
		 needs to be assigned to a signed value.  */
	      gfc_offset record = next_array_record
		(dtp, dtp->u.p.current_unit->ls, &finished);
	      if (finished)
		dtp->u.p.current_unit->endfile = AT_ENDFILE;

	      /* Now seek to this record */
	      record = record * dtp->u.p.current_unit->recl;

	      if (sseek (dtp->u.p.current_unit->s, record, SEEK_SET) < 0)
		{
		  generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
		  return;
		}

	      dtp->u.p.current_unit->bytes_left = dtp->u.p.current_unit->recl;
	    }
	  else
	    {
	      length = 1;

	      /* If this is the last call to next_record move to the farthest
		 position reached and set length to pad out the remainder
		 of the record. (for character scaler unit) */
	      if (done)
		{
		  m = dtp->u.p.current_unit->recl
			- dtp->u.p.current_unit->bytes_left;
		  if (max_pos > m)
		    {
		      length = max_pos - m;
		      if (sseek (dtp->u.p.current_unit->s,
				 length, SEEK_CUR) < 0)
		        {
			  generate_error (&dtp->common, LIBERROR_INTERNAL_UNIT, NULL);
			  return;
			}
		      length = (size_t) dtp->u.p.current_unit->recl
			- max_pos;
		    }
		  else
		    length = dtp->u.p.current_unit->bytes_left;
		}
	      if (length > 0)
		{
		  p = write_block (dtp, length);
		  if (p == NULL)
		    return;

		  if (unlikely (is_char4_unit (dtp)))
		    {
		      gfc_char4_t *p4 = (gfc_char4_t *) p;
		      memset4 (p4, (gfc_char4_t) ' ', length);
		    }
		  else
		    memset (p, ' ', length);
		}
	    }
	}
      else if (dtp->u.p.seen_dollar == 1)
	break;
      /* Handle legacy CARRIAGECONTROL line endings.  */
      else if (dtp->u.p.current_unit->flags.cc == CC_FORTRAN)
	next_record_cc (dtp);
      else
	{
	  /* Skip newlines for CC=CC_NONE.  */
	  const int len = (dtp->u.p.current_unit->flags.cc == CC_NONE)
	    ? 0
#ifdef HAVE_CRLF
	    : 2;
#else
	    : 1;
#endif
	  fbuf_seek (dtp->u.p.current_unit, 0, SEEK_END);
	  if (dtp->u.p.current_unit->flags.cc != CC_NONE)
	    {
	      char *p = fbuf_alloc (dtp->u.p.current_unit, len);
	      if (!p)
		goto io_error;
#ifdef HAVE_CRLF
	      *(p++) = '\r';
#endif
	      *p = '\n';
	    }
	  if (is_stream_io (dtp))
	    {
	      dtp->u.p.current_unit->strm_pos += len;
	      if (dtp->u.p.current_unit->strm_pos
		  < ssize (dtp->u.p.current_unit->s))
		unit_truncate (dtp->u.p.current_unit,
                               dtp->u.p.current_unit->strm_pos - 1,
                               &dtp->common);
	    }
	}

      break;
    case FORMATTED_UNSPECIFIED:
      gcc_unreachable ();

    io_error:
      generate_error (&dtp->common, LIBERROR_OS, NULL);
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
    next_record_r (dtp, done);
  else
    next_record_w (dtp, done);

  fbuf_flush (dtp->u.p.current_unit, dtp->u.p.mode);

  if (!is_stream_io (dtp))
    {
      /* Since we have changed the position, set it to unspecified so
	 that INQUIRE(POSITION=) knows it needs to look into it.  */
      if (done)
	dtp->u.p.current_unit->flags.position = POSITION_UNSPECIFIED;

      dtp->u.p.current_unit->current_record = 0;
      if (dtp->u.p.current_unit->flags.access == ACCESS_DIRECT)
	{
	  fp = stell (dtp->u.p.current_unit->s);
	  /* Calculate next record, rounding up partial records.  */
	  dtp->u.p.current_unit->last_record =
	    (fp + dtp->u.p.current_unit->recl) /
	      dtp->u.p.current_unit->recl - 1;
	}
      else
	dtp->u.p.current_unit->last_record++;
    }

  if (!done)
    pre_position (dtp);

  smarkeor (dtp->u.p.current_unit->s);
}


/* Finalize the current data transfer.  For a nonadvancing transfer,
   this means advancing to the next record.  For internal units close the
   stream associated with the unit.  */

static void
finalize_transfer (st_parameter_dt *dtp)
{
  GFC_INTEGER_4 cf = dtp->common.flags;

  if ((dtp->u.p.ionml != NULL)
      && (cf & IOPARM_DT_HAS_NAMELIST_NAME) != 0)
    {
       if (dtp->u.p.current_unit->flags.form == FORM_UNFORMATTED)
	 {
	   generate_error (&dtp->common, LIBERROR_OPTION_CONFLICT,
			   "Namelist formatting for unit connected "
			   "with FORM='UNFORMATTED'");
	   return;
	 }

       dtp->u.p.namelist_mode = 1;
       if ((cf & IOPARM_DT_NAMELIST_READ_MODE) != 0)
	 namelist_read (dtp);
       else
	 namelist_write (dtp);
    }

  if ((dtp->common.flags & IOPARM_DT_HAS_SIZE) != 0)
    *dtp->size = dtp->u.p.current_unit->size_used;

  if (dtp->u.p.eor_condition)
    {
      generate_error (&dtp->common, LIBERROR_EOR, NULL);
      goto done;
    }

  if (dtp->u.p.current_unit && (dtp->u.p.current_unit->child_dtio  > 0))
    {
      if (cf & IOPARM_DT_HAS_FORMAT)
        {
	  free (dtp->u.p.fmt);
	  free (dtp->format);
	}
      return;
    }

  if ((dtp->common.flags & IOPARM_LIBRETURN_MASK) != IOPARM_LIBRETURN_OK)
    {
      if (dtp->u.p.current_unit && current_mode (dtp) == UNFORMATTED_SEQUENTIAL)
	dtp->u.p.current_unit->current_record = 0;
      goto done;
    }

  dtp->u.p.transfer = NULL;
  if (dtp->u.p.current_unit == NULL)
    goto done;

  if ((cf & IOPARM_DT_LIST_FORMAT) != 0 && dtp->u.p.mode == READING)
    {
      finish_list_read (dtp);
      goto done;
    }

  if (dtp->u.p.mode == WRITING)
    dtp->u.p.current_unit->previous_nonadvancing_write
      = dtp->u.p.advance_status == ADVANCE_NO;

  if (is_stream_io (dtp))
    {
      if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED
	  && dtp->u.p.advance_status != ADVANCE_NO)
	next_record (dtp, 1);

      goto done;
    }

  dtp->u.p.current_unit->current_record = 0;

  if (!is_internal_unit (dtp) && dtp->u.p.seen_dollar)
    {
      fbuf_flush (dtp->u.p.current_unit, dtp->u.p.mode);
      dtp->u.p.seen_dollar = 0;
      goto done;
    }

  /* For non-advancing I/O, save the current maximum position for use in the
     next I/O operation if needed.  */
  if (dtp->u.p.advance_status == ADVANCE_NO)
    {
      if (dtp->u.p.skips > 0)
	{
	  int tmp;
	  write_x (dtp, dtp->u.p.skips, dtp->u.p.pending_spaces);
	  tmp = (int)(dtp->u.p.current_unit->recl
		      - dtp->u.p.current_unit->bytes_left);
	  dtp->u.p.max_pos =
	    dtp->u.p.max_pos > tmp ? dtp->u.p.max_pos : tmp;
	  dtp->u.p.skips = 0;
	}
      int bytes_written = (int) (dtp->u.p.current_unit->recl
	- dtp->u.p.current_unit->bytes_left);
      dtp->u.p.current_unit->saved_pos =
	dtp->u.p.max_pos > 0 ? dtp->u.p.max_pos - bytes_written : 0;
      fbuf_flush (dtp->u.p.current_unit, dtp->u.p.mode);
      goto done;
    }
  else if (dtp->u.p.current_unit->flags.form == FORM_FORMATTED
           && dtp->u.p.mode == WRITING && !is_internal_unit (dtp))
      fbuf_seek (dtp->u.p.current_unit, 0, SEEK_END);

  dtp->u.p.current_unit->saved_pos = 0;
  dtp->u.p.current_unit->last_char = EOF - 1;
  next_record (dtp, 1);

 done:

  if (dtp->u.p.unit_is_internal)
    {
      /* The unit structure may be reused later so clear the
	 internal unit kind.  */
      dtp->u.p.current_unit->internal_unit_kind = 0;

      fbuf_destroy (dtp->u.p.current_unit);
      if (dtp->u.p.current_unit
	  && (dtp->u.p.current_unit->child_dtio  == 0)
	  && dtp->u.p.current_unit->s)
	{
	  sclose (dtp->u.p.current_unit->s);
	  dtp->u.p.current_unit->s = NULL;
	}
    }

#ifdef HAVE_POSIX_2008_LOCALE
  if (dtp->u.p.old_locale != (locale_t) 0)
    {
      uselocale (dtp->u.p.old_locale);
      dtp->u.p.old_locale = (locale_t) 0;
    }
#else
  __gthread_mutex_lock (&old_locale_lock);
  if (!--old_locale_ctr)
    {
      setlocale (LC_NUMERIC, old_locale);
      old_locale = NULL;
    }
  __gthread_mutex_unlock (&old_locale_lock);
#endif
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
    *dtp->iolength += (GFC_IO_INT) (size * nelems);
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
}

extern void st_read_done (st_parameter_dt *);
export_proto(st_read_done);

void
st_read_done_worker (st_parameter_dt *dtp, bool unlock)
{
  bool free_newunit = false;
  finalize_transfer (dtp);

  free_ionml (dtp);

  /* If this is a parent READ statement we do not need to retain the
     internal unit structure for child use.  */
  if (dtp->u.p.current_unit != NULL
      && dtp->u.p.current_unit->child_dtio == 0)
    {
      if (dtp->u.p.unit_is_internal)
	{
	  if ((dtp->common.flags & IOPARM_DT_HAS_UDTIO) == 0)
	    {
	      free (dtp->u.p.current_unit->filename);
	      dtp->u.p.current_unit->filename = NULL;
	      if (dtp->u.p.current_unit->ls)
		free (dtp->u.p.current_unit->ls);
	      dtp->u.p.current_unit->ls = NULL;
	    }
	  free_newunit = true;
	}
      if (dtp->u.p.unit_is_internal || dtp->u.p.format_not_saved)
	{
	  free_format_data (dtp->u.p.fmt);
	  free_format (dtp);
	}
    }
   if (unlock)
     unlock_unit (dtp->u.p.current_unit);
   if (free_newunit)
     {
       /* Avoid inverse lock issues by placing after unlock_unit.  */
       LOCK (&unit_lock);
       newunit_free (dtp->common.unit);
       UNLOCK (&unit_lock);
     }
}

void
st_read_done (st_parameter_dt *dtp)
{
  if (dtp->u.p.current_unit)
    {
      if (dtp->u.p.current_unit->au)
	{
	  if (dtp->common.flags & IOPARM_DT_HAS_ID)
	    *dtp->id = enqueue_done_id (dtp->u.p.current_unit->au, AIO_READ_DONE);  
	  else
	    {
	      if (dtp->u.p.async)
		enqueue_done (dtp->u.p.current_unit->au, AIO_READ_DONE);
	    }
	  unlock_unit (dtp->u.p.current_unit);
	}
      else
	st_read_done_worker (dtp, true);  /* Calls unlock_unit.  */
    }

  library_end ();
}

extern void st_write (st_parameter_dt *);
export_proto (st_write);

void
st_write (st_parameter_dt *dtp)
{
  library_start (&dtp->common);
  data_transfer_init (dtp, 0);
}


void
st_write_done_worker (st_parameter_dt *dtp, bool unlock)
{
  bool free_newunit = false;
  finalize_transfer (dtp);

  if (dtp->u.p.current_unit != NULL
      && dtp->u.p.current_unit->child_dtio == 0)
    {
      /* Deal with endfile conditions associated with sequential files.  */
      if (dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
	switch (dtp->u.p.current_unit->endfile)
	  {
	  case AT_ENDFILE:		/* Remain at the endfile record.  */
	    break;

	  case AFTER_ENDFILE:
	    dtp->u.p.current_unit->endfile = AT_ENDFILE; /* Just at it now.  */
	    break;

	  case NO_ENDFILE:
	    /* Get rid of whatever is after this record.  */
	    if (!is_internal_unit (dtp))
	      unit_truncate (dtp->u.p.current_unit,
			     stell (dtp->u.p.current_unit->s),
			     &dtp->common);
	    dtp->u.p.current_unit->endfile = AT_ENDFILE;
	    break;
	  }

      free_ionml (dtp);

      /* If this is a parent WRITE statement we do not need to retain the
	 internal unit structure for child use.  */
      if (dtp->u.p.unit_is_internal)
	{
	  if ((dtp->common.flags & IOPARM_DT_HAS_UDTIO) == 0)
	    {
	      free (dtp->u.p.current_unit->filename);
	      dtp->u.p.current_unit->filename = NULL;
	      if (dtp->u.p.current_unit->ls)
		free (dtp->u.p.current_unit->ls);
	      dtp->u.p.current_unit->ls = NULL;
	    }
	  free_newunit = true;
	}
      if (dtp->u.p.unit_is_internal || dtp->u.p.format_not_saved)
	{
	  free_format_data (dtp->u.p.fmt);
	  free_format (dtp);
	}
    }
   if (unlock)
     unlock_unit (dtp->u.p.current_unit);
   if (free_newunit)
     {
       /* Avoid inverse lock issues by placing after unlock_unit.  */
       LOCK (&unit_lock);
       newunit_free (dtp->common.unit);
       UNLOCK (&unit_lock);
     }
}

extern void st_write_done (st_parameter_dt *);
export_proto(st_write_done);

void
st_write_done (st_parameter_dt *dtp)
{
  if (dtp->u.p.current_unit)
    {
      if (dtp->u.p.current_unit->au && dtp->u.p.async)
	{
	  if (dtp->common.flags & IOPARM_DT_HAS_ID)
	    *dtp->id = enqueue_done_id (dtp->u.p.current_unit->au,
					AIO_WRITE_DONE);
	  else
	    {
	      /* We perform synchronous I/O on an asynchronous unit, so no need
		 to enqueue AIO_READ_DONE.  */
	      if (dtp->u.p.async)
		enqueue_done (dtp->u.p.current_unit->au, AIO_WRITE_DONE);
	    }
	  unlock_unit (dtp->u.p.current_unit);
	}
      else
	st_write_done_worker (dtp, true);  /* Calls unlock_unit.  */
    }

  library_end ();
}

/* Wait operation.  We need to keep around the do-nothing version
 of st_wait for compatibility with previous versions, which had marked
 the argument as unused (and thus liable to be removed).

 TODO: remove at next bump in version number.  */

void
st_wait (st_parameter_wait *wtp __attribute__((unused)))
{
  return;
}

void
st_wait_async (st_parameter_wait *wtp)
{
  gfc_unit *u = find_unit (wtp->common.unit);
  if (ASYNC_IO && u && u->au)
    {
      if (wtp->common.flags & IOPARM_WAIT_HAS_ID)
	async_wait_id (&(wtp->common), u->au, *wtp->id);
      else
	async_wait (&(wtp->common), u->au);
    }

  unlock_unit (u);
}


/* Receives the scalar information for namelist objects and stores it
   in a linked list of namelist_info types.  */

static void
set_nml_var (st_parameter_dt *dtp, void *var_addr, char *var_name,
	     GFC_INTEGER_4 len, gfc_charlen_type string_length,
	     dtype_type dtype, void *dtio_sub, void *vtable)
{
  namelist_info *t1 = NULL;
  namelist_info *nml;
  size_t var_name_len = strlen (var_name);

  nml = (namelist_info*) xmalloc (sizeof (namelist_info));

  nml->mem_pos = var_addr;
  nml->dtio_sub = dtio_sub;
  nml->vtable = vtable;

  nml->var_name = (char*) xmalloc (var_name_len + 1);
  memcpy (nml->var_name, var_name, var_name_len);
  nml->var_name[var_name_len] = '\0';

  nml->len = (int) len;
  nml->string_length = (index_type) string_length;

  nml->var_rank = (int) (dtype.rank);
  nml->size = (index_type) (dtype.elem_len);
  nml->type = (bt) (dtype.type);

  if (nml->var_rank > 0)
    {
      nml->dim = (descriptor_dimension*)
	xmallocarray (nml->var_rank, sizeof (descriptor_dimension));
      nml->ls = (array_loop_spec*)
	xmallocarray (nml->var_rank, sizeof (array_loop_spec));
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

extern void st_set_nml_var (st_parameter_dt *dtp, void *, char *,
			    GFC_INTEGER_4, gfc_charlen_type, dtype_type);
export_proto(st_set_nml_var);

void
st_set_nml_var (st_parameter_dt *dtp, void *var_addr, char *var_name,
		GFC_INTEGER_4 len, gfc_charlen_type string_length,
		dtype_type dtype)
{
  set_nml_var (dtp, var_addr, var_name, len, string_length,
	       dtype, NULL, NULL);
}


/* Essentially the same as previous but carrying the dtio procedure
   and the vtable as additional arguments.  */
extern void st_set_nml_dtio_var (st_parameter_dt *dtp, void *, char *,
				 GFC_INTEGER_4, gfc_charlen_type, dtype_type,
				 void *, void *);
export_proto(st_set_nml_dtio_var);


void
st_set_nml_dtio_var (st_parameter_dt *dtp, void *var_addr, char *var_name,
		     GFC_INTEGER_4 len, gfc_charlen_type string_length,
		     dtype_type dtype, void *dtio_sub, void *vtable)
{
  set_nml_var (dtp, var_addr, var_name, len, string_length,
	       dtype, dtio_sub, vtable);
}

/* Store the dimensional information for the namelist object.  */
extern void st_set_nml_var_dim (st_parameter_dt *, GFC_INTEGER_4,
				index_type, index_type,
				index_type);
export_proto(st_set_nml_var_dim);

void
st_set_nml_var_dim (st_parameter_dt *dtp, GFC_INTEGER_4 n_dim,
		    index_type stride, index_type lbound,
		    index_type ubound)
{
  namelist_info *nml;
  int n;

  n = (int)n_dim;

  for (nml = dtp->u.p.ionml; nml->next; nml = nml->next);

  GFC_DIMENSION_SET(nml->dim[n],lbound,ubound,stride);
}


/* Once upon a time, a poor innocent Fortran program was reading a
   file, when suddenly it hit the end-of-file (EOF).  Unfortunately
   the OS doesn't tell whether we're at the EOF or whether we already
   went past it.  Luckily our hero, libgfortran, keeps track of this.
   Call this function when you detect an EOF condition.  See Section
   9.10.2 in F2003.  */

void
hit_eof (st_parameter_dt *dtp)
{
  dtp->u.p.current_unit->flags.position = POSITION_APPEND;

  if (dtp->u.p.current_unit->flags.access == ACCESS_SEQUENTIAL)
    switch (dtp->u.p.current_unit->endfile)
      {
      case NO_ENDFILE:
      case AT_ENDFILE:
        generate_error (&dtp->common, LIBERROR_END, NULL);
	if (!is_internal_unit (dtp) && !dtp->u.p.namelist_mode)
	  {
	    dtp->u.p.current_unit->endfile = AFTER_ENDFILE;
	    dtp->u.p.current_unit->current_record = 0;
	  }
        else
          dtp->u.p.current_unit->endfile = AT_ENDFILE;
	break;

      case AFTER_ENDFILE:
	generate_error (&dtp->common, LIBERROR_ENDFILE, NULL);
	dtp->u.p.current_unit->current_record = 0;
	break;
      }
  else
    {
      /* Non-sequential files don't have an ENDFILE record, so we
         can't be at AFTER_ENDFILE.  */
      dtp->u.p.current_unit->endfile = AT_ENDFILE;
      generate_error (&dtp->common, LIBERROR_END, NULL);
      dtp->u.p.current_unit->current_record = 0;
    }
}
