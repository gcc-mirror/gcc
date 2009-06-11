/* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Andy Vaught
   F2003 I/O support contributed by Jerry DeLisle

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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

#ifndef GFOR_IO_H
#define GFOR_IO_H

/* IO library include.  */

#include "libgfortran.h"

#include <setjmp.h>
#include <gthr.h>

/* Basic types used in data transfers.  */

typedef enum
{ BT_NULL, BT_INTEGER, BT_LOGICAL, BT_CHARACTER, BT_REAL,
  BT_COMPLEX
}
bt;

struct st_parameter_dt;

typedef struct stream
{
  ssize_t (*read) (struct stream *, void *, ssize_t);
  ssize_t (*write) (struct stream *, const void *, ssize_t);
  off_t (*seek) (struct stream *, off_t, int);
  off_t (*tell) (struct stream *);
  /* Avoid keyword truncate due to AIX namespace collision.  */
  int (*trunc) (struct stream *, off_t);
  int (*flush) (struct stream *);
  int (*close) (struct stream *);
}
stream;

/* Inline functions for doing file I/O given a stream.  */
static inline ssize_t
sread (stream * s, void * buf, ssize_t nbyte)
{
  return s->read (s, buf, nbyte);
}

static inline ssize_t
swrite (stream * s, const void * buf, ssize_t nbyte)
{
  return s->write (s, buf, nbyte);
}

static inline off_t
sseek (stream * s, off_t offset, int whence)
{
  return s->seek (s, offset, whence);
}

static inline off_t
stell (stream * s)
{
  return s->tell (s);
}

static inline int
struncate (stream * s, off_t length)
{
  return s->trunc (s, length);
}

static inline int
sflush (stream * s)
{
  return s->flush (s);
}

static inline int
sclose (stream * s)
{
  return s->close (s);
}


/* Macros for testing what kinds of I/O we are doing.  */

#define is_array_io(dtp) ((dtp)->internal_unit_desc)

#define is_internal_unit(dtp) ((dtp)->u.p.unit_is_internal)

#define is_stream_io(dtp) ((dtp)->u.p.current_unit->flags.access == ACCESS_STREAM)

/* The array_loop_spec contains the variables for the loops over index ranges
   that are encountered.  Since the variables can be negative, ssize_t
   is used.  */

typedef struct array_loop_spec
{
  /* Index counter for this dimension.  */
  ssize_t idx;

  /* Start for the index counter.  */
  ssize_t start;

  /* End for the index counter.  */
  ssize_t end;

  /* Step for the index counter.  */
  ssize_t step;
}
array_loop_spec;

/* Representation of a namelist object in libgfortran

   Namelist Records
      &GROUPNAME  OBJECT=value[s] [,OBJECT=value[s]].../
     or
      &GROUPNAME  OBJECT=value[s] [,OBJECT=value[s]]...&END

   The object can be a fully qualified, compound name for an intrinsic
   type, derived types or derived type components.  So, a substring
   a(:)%b(4)%ch(2:4)(1:7) has to be treated correctly in namelist
   read. Hence full information about the structure of the object has
   to be available to list_read.c and write.

   These requirements are met by the following data structures.

   namelist_info type contains all the scalar information about the
   object and arrays of descriptor_dimension and array_loop_spec types for
   arrays.  */

typedef struct namelist_type
{
  /* Object type, stored as GFC_DTYPE_xxxx.  */
  bt type;

  /* Object name.  */
  char * var_name;

  /* Address for the start of the object's data.  */
  void * mem_pos;

  /* Flag to show that a read is to be attempted for this node.  */
  int touched;

  /* Length of intrinsic type in bytes.  */
  int len;

  /* Rank of the object.  */
  int var_rank;

  /* Overall size of the object in bytes.  */
  index_type size;

  /* Length of character string.  */
  index_type string_length;

  descriptor_dimension * dim;
  array_loop_spec * ls;
  struct namelist_type * next;
}
namelist_info;

/* Options for the OPEN statement.  */

typedef enum
{ ACCESS_SEQUENTIAL, ACCESS_DIRECT, ACCESS_APPEND, ACCESS_STREAM,
  ACCESS_UNSPECIFIED
}
unit_access;

typedef enum
{ ACTION_READ, ACTION_WRITE, ACTION_READWRITE,
  ACTION_UNSPECIFIED
}
unit_action;

typedef enum
{ BLANK_NULL, BLANK_ZERO, BLANK_UNSPECIFIED }
unit_blank;

typedef enum
{ DELIM_NONE, DELIM_APOSTROPHE, DELIM_QUOTE,
  DELIM_UNSPECIFIED
}
unit_delim;

typedef enum
{ FORM_FORMATTED, FORM_UNFORMATTED, FORM_UNSPECIFIED }
unit_form;

typedef enum
{ POSITION_ASIS, POSITION_REWIND, POSITION_APPEND,
  POSITION_UNSPECIFIED
}
unit_position;

typedef enum
{ STATUS_UNKNOWN, STATUS_OLD, STATUS_NEW, STATUS_SCRATCH,
  STATUS_REPLACE, STATUS_UNSPECIFIED
}
unit_status;

typedef enum
{ PAD_YES, PAD_NO, PAD_UNSPECIFIED }
unit_pad;

typedef enum
{ DECIMAL_POINT, DECIMAL_COMMA, DECIMAL_UNSPECIFIED }
unit_decimal;

typedef enum
{ ENCODING_UTF8, ENCODING_DEFAULT, ENCODING_UNSPECIFIED }
unit_encoding;

typedef enum
{ ROUND_UP, ROUND_DOWN, ROUND_ZERO, ROUND_NEAREST, ROUND_COMPATIBLE,
  ROUND_PROCDEFINED, ROUND_UNSPECIFIED }
unit_round;

/* NOTE: unit_sign must correspond with the sign_status enumerator in
   st_parameter_dt to not break the ABI.  */
typedef enum
{ SIGN_PROCDEFINED, SIGN_SUPPRESS, SIGN_PLUS, SIGN_UNSPECIFIED }
unit_sign;

typedef enum
{ ADVANCE_YES, ADVANCE_NO, ADVANCE_UNSPECIFIED }
unit_advance;

typedef enum
{READING, WRITING}
unit_mode;

typedef enum
{ ASYNC_YES, ASYNC_NO, ASYNC_UNSPECIFIED }
unit_async;

typedef enum
{ SIGN_S, SIGN_SS, SIGN_SP }
unit_sign_s;

#define CHARACTER1(name) \
	      char * name; \
	      gfc_charlen_type name ## _len
#define CHARACTER2(name) \
	      gfc_charlen_type name ## _len; \
	      char * name

typedef struct
{
  st_parameter_common common;
  GFC_INTEGER_4 recl_in;
  CHARACTER2 (file);
  CHARACTER1 (status);
  CHARACTER2 (access);
  CHARACTER1 (form);
  CHARACTER2 (blank);
  CHARACTER1 (position);
  CHARACTER2 (action);
  CHARACTER1 (delim);
  CHARACTER2 (pad);
  CHARACTER1 (convert);
  CHARACTER2 (decimal);
  CHARACTER1 (encoding);
  CHARACTER2 (round);
  CHARACTER1 (sign);
  CHARACTER2 (asynchronous);
}
st_parameter_open;

#define IOPARM_CLOSE_HAS_STATUS		(1 << 7)

typedef struct
{
  st_parameter_common common;
  CHARACTER1 (status);
}
st_parameter_close;

typedef struct
{
  st_parameter_common common;
}
st_parameter_filepos;

#define IOPARM_INQUIRE_HAS_EXIST	(1 << 7)
#define IOPARM_INQUIRE_HAS_OPENED	(1 << 8)
#define IOPARM_INQUIRE_HAS_NUMBER	(1 << 9)
#define IOPARM_INQUIRE_HAS_NAMED	(1 << 10)
#define IOPARM_INQUIRE_HAS_NEXTREC	(1 << 11)
#define IOPARM_INQUIRE_HAS_RECL_OUT	(1 << 12)
#define IOPARM_INQUIRE_HAS_STRM_POS_OUT (1 << 13)
#define IOPARM_INQUIRE_HAS_FILE		(1 << 14)
#define IOPARM_INQUIRE_HAS_ACCESS	(1 << 15)
#define IOPARM_INQUIRE_HAS_FORM		(1 << 16)
#define IOPARM_INQUIRE_HAS_BLANK	(1 << 17)
#define IOPARM_INQUIRE_HAS_POSITION	(1 << 18)
#define IOPARM_INQUIRE_HAS_ACTION	(1 << 19)
#define IOPARM_INQUIRE_HAS_DELIM	(1 << 20)
#define IOPARM_INQUIRE_HAS_PAD		(1 << 21)
#define IOPARM_INQUIRE_HAS_NAME		(1 << 22)
#define IOPARM_INQUIRE_HAS_SEQUENTIAL	(1 << 23)
#define IOPARM_INQUIRE_HAS_DIRECT	(1 << 24)
#define IOPARM_INQUIRE_HAS_FORMATTED	(1 << 25)
#define IOPARM_INQUIRE_HAS_UNFORMATTED	(1 << 26)
#define IOPARM_INQUIRE_HAS_READ		(1 << 27)
#define IOPARM_INQUIRE_HAS_WRITE	(1 << 28)
#define IOPARM_INQUIRE_HAS_READWRITE	(1 << 29)
#define IOPARM_INQUIRE_HAS_CONVERT	(1 << 30)
#define IOPARM_INQUIRE_HAS_FLAGS2	(1 << 31)

#define IOPARM_INQUIRE_HAS_ASYNCHRONOUS	(1 << 0)
#define IOPARM_INQUIRE_HAS_DECIMAL	(1 << 1)
#define IOPARM_INQUIRE_HAS_ENCODING	(1 << 2)
#define IOPARM_INQUIRE_HAS_ROUND	(1 << 3)
#define IOPARM_INQUIRE_HAS_SIGN		(1 << 4)
#define IOPARM_INQUIRE_HAS_PENDING	(1 << 5)
#define IOPARM_INQUIRE_HAS_SIZE		(1 << 6)
#define IOPARM_INQUIRE_HAS_ID		(1 << 7)

typedef struct
{
  st_parameter_common common;
  GFC_INTEGER_4 *exist, *opened, *number, *named;
  GFC_INTEGER_4 *nextrec, *recl_out;
  GFC_IO_INT *strm_pos_out;
  CHARACTER1 (file);
  CHARACTER2 (access);
  CHARACTER1 (form);
  CHARACTER2 (blank);
  CHARACTER1 (position);
  CHARACTER2 (action);
  CHARACTER1 (delim);
  CHARACTER2 (pad);
  CHARACTER1 (name);
  CHARACTER2 (sequential);
  CHARACTER1 (direct);
  CHARACTER2 (formatted);
  CHARACTER1 (unformatted);
  CHARACTER2 (read);
  CHARACTER1 (write);
  CHARACTER2 (readwrite);
  CHARACTER1 (convert);
  GFC_INTEGER_4 flags2;
  CHARACTER1 (asynchronous);
  CHARACTER2 (decimal);
  CHARACTER1 (encoding);
  CHARACTER2 (round);
  CHARACTER1 (sign);
  GFC_INTEGER_4 *pending;
  GFC_INTEGER_4 *size;
  GFC_INTEGER_4 *id;
}
st_parameter_inquire;

struct gfc_unit;
struct format_data;

#define IOPARM_DT_LIST_FORMAT			(1 << 7)
#define IOPARM_DT_NAMELIST_READ_MODE		(1 << 8)
#define IOPARM_DT_HAS_REC			(1 << 9)
#define IOPARM_DT_HAS_SIZE			(1 << 10)
#define IOPARM_DT_HAS_IOLENGTH			(1 << 11)
#define IOPARM_DT_HAS_FORMAT			(1 << 12)
#define IOPARM_DT_HAS_ADVANCE			(1 << 13)
#define IOPARM_DT_HAS_INTERNAL_UNIT		(1 << 14)
#define IOPARM_DT_HAS_NAMELIST_NAME		(1 << 15)
#define IOPARM_DT_HAS_ID			(1 << 16)
#define IOPARM_DT_HAS_POS			(1 << 17)
#define IOPARM_DT_HAS_ASYNCHRONOUS		(1 << 18)
#define IOPARM_DT_HAS_BLANK			(1 << 19)
#define IOPARM_DT_HAS_DECIMAL			(1 << 20)
#define IOPARM_DT_HAS_DELIM			(1 << 21)
#define IOPARM_DT_HAS_PAD			(1 << 22)
#define IOPARM_DT_HAS_ROUND			(1 << 23)
#define IOPARM_DT_HAS_SIGN			(1 << 24)
#define IOPARM_DT_HAS_F2003                     (1 << 25)
/* Internal use bit.  */
#define IOPARM_DT_IONML_SET			(1 << 31)


typedef struct st_parameter_dt
{
  st_parameter_common common;
  GFC_IO_INT rec;
  GFC_IO_INT *size, *iolength;
  gfc_array_char *internal_unit_desc;
  CHARACTER1 (format);
  CHARACTER2 (advance);
  CHARACTER1 (internal_unit);
  CHARACTER2 (namelist_name);
  /* Private part of the structure.  The compiler just needs
     to reserve enough space.  */
  union
    {
      struct
	{
	  void (*transfer) (struct st_parameter_dt *, bt, void *, int,
			    size_t, size_t);
	  struct gfc_unit *current_unit;
	  /* Item number in a formatted data transfer.  Also used in namelist
	     read_logical as an index into line_buffer.  */
	  int item_count;
	  unit_mode mode;
	  unit_blank blank_status;
	  unit_sign sign_status;
	  int scale_factor;
	  int max_pos; /* Maximum righthand column written to.  */
	  /* Number of skips + spaces to be done for T and X-editing.  */
	  int skips;
	  /* Number of spaces to be done for T and X-editing.  */
	  int pending_spaces;
	  /* Whether an EOR condition was encountered. Value is:
	       0 if no EOR was encountered
	       1 if an EOR was encountered due to a 1-byte marker (LF)
	       2 if an EOR was encountered due to a 2-bytes marker (CRLF) */
	  int sf_seen_eor;
	  unit_advance advance_status;
	  unsigned reversion_flag : 1; /* Format reversion has occurred.  */
	  unsigned first_item : 1;
	  unsigned seen_dollar : 1;
	  unsigned eor_condition : 1;
	  unsigned no_leading_blank : 1;
	  unsigned char_flag : 1;
	  unsigned input_complete : 1;
	  unsigned at_eol : 1;
	  unsigned comma_flag : 1;
	  /* A namelist specific flag used in the list directed library
	     to flag that calls are being made from namelist read (eg. to
	     ignore comments or to treat '/' as a terminator)  */
	  unsigned namelist_mode : 1;
	  /* A namelist specific flag used in the list directed library
	     to flag read errors and return, so that an attempt can be
	     made to read a new object name.  */
	  unsigned nml_read_error : 1;
	  /* A sequential formatted read specific flag used to signal that a
	     character string is being read so don't use commas to shorten a
	     formatted field width.  */
	  unsigned sf_read_comma : 1;
	  /* A namelist specific flag used to enable reading input from 
	     line_buffer for logical reads.  */
	  unsigned line_buffer_enabled : 1;
	  /* An internal unit specific flag used to identify that the associated
	     unit is internal.  */
	  unsigned unit_is_internal : 1;
	  /* An internal unit specific flag to signify an EOF condition for list
	     directed read.  */
	  unsigned at_eof : 1;
	  /* Used for g0 floating point output.  */
	  unsigned g0_no_blanks : 1;
	  /* 15 unused bits.  */

	  char last_char;
	  char nml_delim;

	  int repeat_count;
	  int saved_length;
	  int saved_used;
	  bt saved_type;
	  char *saved_string;
	  char *scratch;
	  char *line_buffer;
	  struct format_data *fmt;
	  jmp_buf *eof_jump;
	  namelist_info *ionml;
	  /* A flag used to identify when a non-standard expanded namelist read
	     has occurred.  */
	  int expanded_read;
	  /* Storage area for values except for strings.  Must be
	     large enough to hold a complex value (two reals) of the
	     largest kind.  */
	  char value[32];
	  GFC_IO_INT size_used;
	} p;
      /* This pad size must be equal to the pad_size declared in
	 trans-io.c (gfc_build_io_library_fndecls).  The above structure
	 must be smaller or equal to this array.  */
      char pad[16 * sizeof (char *) + 32 * sizeof (int)];
    } u;
  GFC_INTEGER_4 *id;
  GFC_IO_INT pos;
  CHARACTER1 (asynchronous);
  CHARACTER2 (blank);
  CHARACTER1 (decimal);
  CHARACTER2 (delim);
  CHARACTER1 (pad);
  CHARACTER2 (round);
  CHARACTER1 (sign);
}
st_parameter_dt;

/* Ensure st_parameter_dt's u.pad is bigger or equal to u.p.  */
extern char check_st_parameter_dt[sizeof (((st_parameter_dt *) 0)->u.pad)
				  >= sizeof (((st_parameter_dt *) 0)->u.p)
				  ? 1 : -1];

#define IOPARM_WAIT_HAS_ID		(1 << 7)

typedef struct
{
  st_parameter_common common;
  CHARACTER1 (id);
}
st_parameter_wait;


#undef CHARACTER1
#undef CHARACTER2

typedef struct
{
  unit_access access;
  unit_action action;
  unit_blank blank;
  unit_delim delim;
  unit_form form;
  int is_notpadded;
  unit_position position;
  unit_status status;
  unit_pad pad;
  unit_convert convert;
  int has_recl;
  unit_decimal decimal;
  unit_encoding encoding;
  unit_round round;
  unit_sign sign;
  unit_async async;
}
unit_flags;


/* Formatting buffer. This is a temporary scratch buffer. Currently used only
   by formatted writes. After every
   formatted write statement, this buffer is flushed. This buffer is needed since
   not all devices are seekable, and T or TL edit descriptors require 
   moving backwards in the record.  However, advance='no' complicates the
   situation, so the buffer must only be partially flushed from the end of the
   last flush until the current position in the record. */

typedef struct fbuf
{
  char *buf;			/* Start of buffer.  */
  int len;			/* Length of buffer.  */
  int act;			/* Active bytes in buffer.  */
  int pos;			/* Current position in buffer.  */
}
fbuf;


typedef struct gfc_unit
{
  int unit_number;
  stream *s;
  
  /* Treap links.  */
  struct gfc_unit *left, *right;
  int priority;

  int read_bad, current_record, saved_pos, previous_nonadvancing_write;

  enum
  { NO_ENDFILE, AT_ENDFILE, AFTER_ENDFILE }
  endfile;

  unit_mode mode;
  unit_flags flags;
  unit_pad pad_status;
  unit_decimal decimal_status;
  unit_delim delim_status;

  /* recl                 -- Record length of the file.
     last_record          -- Last record number read or written
     maxrec               -- Maximum record number in a direct access file
     bytes_left           -- Bytes left in current record.
     strm_pos             -- Current position in file for STREAM I/O.
     recl_subrecord       -- Maximum length for subrecord.
     bytes_left_subrecord -- Bytes left in current subrecord.  */
  gfc_offset recl, last_record, maxrec, bytes_left, strm_pos,
    recl_subrecord, bytes_left_subrecord;

  /* Set to 1 if we have read a subrecord.  */

  int continued;

  __gthread_mutex_t lock;
  /* Number of threads waiting to acquire this unit's lock.
     When non-zero, close_unit doesn't only removes the unit
     from the UNIT_ROOT tree, but doesn't free it and the
     last of the waiting threads will do that.
     This must be either atomically increased/decreased, or
     always guarded by UNIT_LOCK.  */
  int waiting;
  /* Flag set by close_unit if the unit as been closed.
     Must be manipulated under unit's lock.  */
  int closed;

  /* For traversing arrays */
  array_loop_spec *ls;
  int rank;

  int file_len;
  char *file;

  /* Formatting buffer.  */
  struct fbuf *fbuf;
}
gfc_unit;

/* Format tokens.  Only about half of these can be stored in the
   format nodes.  */

typedef enum
{
  FMT_NONE = 0, FMT_UNKNOWN, FMT_SIGNED_INT, FMT_ZERO, FMT_POSINT, FMT_PERIOD,
  FMT_COMMA, FMT_COLON, FMT_SLASH, FMT_DOLLAR, FMT_T, FMT_TR, FMT_TL,
  FMT_LPAREN, FMT_RPAREN, FMT_X, FMT_S, FMT_SS, FMT_SP, FMT_STRING,
  FMT_BADSTRING, FMT_P, FMT_I, FMT_B, FMT_BN, FMT_BZ, FMT_O, FMT_Z, FMT_F,
  FMT_E, FMT_EN, FMT_ES, FMT_G, FMT_L, FMT_A, FMT_D, FMT_H, FMT_END, FMT_DC,
  FMT_DP
}
format_token;


/* Format nodes.  A format string is converted into a tree of these
   structures, which is traversed as part of a data transfer statement.  */

typedef struct fnode
{
  format_token format;
  int repeat;
  struct fnode *next;
  char *source;

  union
  {
    struct
    {
      int w, d, e;
    }
    real;

    struct
    {
      int length;
      char *p;
    }
    string;

    struct
    {
      int w, m;
    }
    integer;

    int w;
    int k;
    int r;
    int n;

    struct fnode *child;
  }
  u;

  /* Members for traversing the tree during data transfer.  */

  int count;
  struct fnode *current;

}
fnode;


/* unix.c */

extern int compare_files (stream *, stream *);
internal_proto(compare_files);

extern stream *open_external (st_parameter_open *, unit_flags *);
internal_proto(open_external);

extern stream *open_internal (char *, int, gfc_offset);
internal_proto(open_internal);

extern char * mem_alloc_w (stream *, int *);
internal_proto(mem_alloc_w);

extern char * mem_alloc_r (stream *, int *);
internal_proto(mem_alloc_w);

extern stream *input_stream (void);
internal_proto(input_stream);

extern stream *output_stream (void);
internal_proto(output_stream);

extern stream *error_stream (void);
internal_proto(error_stream);

extern int compare_file_filename (gfc_unit *, const char *, int);
internal_proto(compare_file_filename);

extern gfc_unit *find_file (const char *file, gfc_charlen_type file_len);
internal_proto(find_file);

extern int delete_file (gfc_unit *);
internal_proto(delete_file);

extern int file_exists (const char *file, gfc_charlen_type file_len);
internal_proto(file_exists);

extern const char *inquire_sequential (const char *, int);
internal_proto(inquire_sequential);

extern const char *inquire_direct (const char *, int);
internal_proto(inquire_direct);

extern const char *inquire_formatted (const char *, int);
internal_proto(inquire_formatted);

extern const char *inquire_unformatted (const char *, int);
internal_proto(inquire_unformatted);

extern const char *inquire_read (const char *, int);
internal_proto(inquire_read);

extern const char *inquire_write (const char *, int);
internal_proto(inquire_write);

extern const char *inquire_readwrite (const char *, int);
internal_proto(inquire_readwrite);

extern gfc_offset file_length (stream *);
internal_proto(file_length);

extern int is_seekable (stream *);
internal_proto(is_seekable);

extern int is_special (stream *);
internal_proto(is_special);

extern void flush_if_preconnected (stream *);
internal_proto(flush_if_preconnected);

extern void empty_internal_buffer(stream *);
internal_proto(empty_internal_buffer);

extern int stream_isatty (stream *);
internal_proto(stream_isatty);

extern char * stream_ttyname (stream *);
internal_proto(stream_ttyname);

extern int unpack_filename (char *, const char *, int);
internal_proto(unpack_filename);

/* unit.c */

/* Maximum file offset, computed at library initialization time.  */
extern gfc_offset max_offset;
internal_proto(max_offset);

/* Unit tree root.  */
extern gfc_unit *unit_root;
internal_proto(unit_root);

extern __gthread_mutex_t unit_lock;
internal_proto(unit_lock);

extern int close_unit (gfc_unit *);
internal_proto(close_unit);

extern gfc_unit *get_internal_unit (st_parameter_dt *);
internal_proto(get_internal_unit);

extern void free_internal_unit (st_parameter_dt *);
internal_proto(free_internal_unit);

extern gfc_unit *find_unit (int);
internal_proto(find_unit);

extern gfc_unit *find_or_create_unit (int);
internal_proto(find_or_create_unit);

extern gfc_unit *get_unit (st_parameter_dt *, int);
internal_proto(get_unit);

extern void unlock_unit (gfc_unit *);
internal_proto(unlock_unit);

extern void update_position (gfc_unit *);
internal_proto(update_position);

extern void finish_last_advance_record (gfc_unit *u);
internal_proto (finish_last_advance_record);

extern int unit_truncate (gfc_unit *, gfc_offset, st_parameter_common *);
internal_proto (unit_truncate);

/* open.c */

extern gfc_unit *new_unit (st_parameter_open *, gfc_unit *, unit_flags *);
internal_proto(new_unit);

/* format.c */

extern void parse_format (st_parameter_dt *);
internal_proto(parse_format);

extern const fnode *next_format (st_parameter_dt *);
internal_proto(next_format);

extern void unget_format (st_parameter_dt *, const fnode *);
internal_proto(unget_format);

extern void format_error (st_parameter_dt *, const fnode *, const char *);
internal_proto(format_error);

extern void free_format_data (st_parameter_dt *);
internal_proto(free_format_data);

/* transfer.c */

#define SCRATCH_SIZE 300

extern const char *type_name (bt);
internal_proto(type_name);

extern void * read_block_form (st_parameter_dt *, int *);
internal_proto(read_block_form);

extern char *read_sf (st_parameter_dt *, int *, int);
internal_proto(read_sf);

extern void *write_block (st_parameter_dt *, int);
internal_proto(write_block);

extern gfc_offset next_array_record (st_parameter_dt *, array_loop_spec *,
				     int*);
internal_proto(next_array_record);

extern gfc_offset init_loop_spec (gfc_array_char *, array_loop_spec *,
				  gfc_offset *);
internal_proto(init_loop_spec);

extern void next_record (st_parameter_dt *, int);
internal_proto(next_record);

extern void reverse_memcpy (void *, const void *, size_t);
internal_proto (reverse_memcpy);

extern void st_wait (st_parameter_wait *);
export_proto(st_wait);

extern void hit_eof (st_parameter_dt *);
internal_proto(hit_eof);

/* read.c */

extern void set_integer (void *, GFC_INTEGER_LARGEST, int);
internal_proto(set_integer);

extern GFC_UINTEGER_LARGEST max_value (int, int);
internal_proto(max_value);

extern int convert_real (st_parameter_dt *, void *, const char *, int);
internal_proto(convert_real);

extern void read_a (st_parameter_dt *, const fnode *, char *, int);
internal_proto(read_a);

extern void read_a_char4 (st_parameter_dt *, const fnode *, char *, int);
internal_proto(read_a);

extern void read_f (st_parameter_dt *, const fnode *, char *, int);
internal_proto(read_f);

extern void read_l (st_parameter_dt *, const fnode *, char *, int);
internal_proto(read_l);

extern void read_x (st_parameter_dt *, int);
internal_proto(read_x);

extern void read_radix (st_parameter_dt *, const fnode *, char *, int, int);
internal_proto(read_radix);

extern void read_decimal (st_parameter_dt *, const fnode *, char *, int);
internal_proto(read_decimal);

/* list_read.c */

extern void list_formatted_read (st_parameter_dt *, bt, void *, int, size_t,
				 size_t);
internal_proto(list_formatted_read);

extern void finish_list_read (st_parameter_dt *);
internal_proto(finish_list_read);

extern void namelist_read (st_parameter_dt *);
internal_proto(namelist_read);

extern void namelist_write (st_parameter_dt *);
internal_proto(namelist_write);

/* write.c */

extern void write_a (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_a);

extern void write_a_char4 (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_a_char4);

extern void write_b (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_b);

extern void write_d (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_d);

extern void write_e (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_e);

extern void write_en (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_en);

extern void write_es (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_es);

extern void write_f (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_f);

extern void write_i (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_i);

extern void write_l (st_parameter_dt *, const fnode *, char *, int);
internal_proto(write_l);

extern void write_o (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_o);

extern void write_real (st_parameter_dt *, const char *, int);
internal_proto(write_real);

extern void write_real_g0 (st_parameter_dt *, const char *, int, int);
internal_proto(write_real_g0);

extern void write_x (st_parameter_dt *, int, int);
internal_proto(write_x);

extern void write_z (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_z);

extern void list_formatted_write (st_parameter_dt *, bt, void *, int, size_t,
				  size_t);
internal_proto(list_formatted_write);

/* size_from_kind.c */
extern size_t size_from_real_kind (int);
internal_proto(size_from_real_kind);

extern size_t size_from_complex_kind (int);
internal_proto(size_from_complex_kind);

/* fbuf.c */
extern void fbuf_init (gfc_unit *, int);
internal_proto(fbuf_init);

extern void fbuf_destroy (gfc_unit *);
internal_proto(fbuf_destroy);

extern int fbuf_reset (gfc_unit *);
internal_proto(fbuf_reset);

extern char * fbuf_alloc (gfc_unit *, int);
internal_proto(fbuf_alloc);

extern int fbuf_flush (gfc_unit *, unit_mode);
internal_proto(fbuf_flush);

extern int fbuf_seek (gfc_unit *, int, int);
internal_proto(fbuf_seek);

extern char * fbuf_read (gfc_unit *, int *);
internal_proto(fbuf_read);

/* Never call this function, only use fbuf_getc().  */
extern int fbuf_getc_refill (gfc_unit *);
internal_proto(fbuf_getc_refill);

static inline int
fbuf_getc (gfc_unit * u)
{
  if (u->fbuf->pos < u->fbuf->act)
    return (unsigned char) u->fbuf->buf[u->fbuf->pos++];
  return fbuf_getc_refill (u);
}

/* lock.c */
extern void free_ionml (st_parameter_dt *);
internal_proto(free_ionml);

static inline void
inc_waiting_locked (gfc_unit *u)
{
#ifdef HAVE_SYNC_FETCH_AND_ADD
  (void) __sync_fetch_and_add (&u->waiting, 1);
#else
  u->waiting++;
#endif
}

static inline int
predec_waiting_locked (gfc_unit *u)
{
#ifdef HAVE_SYNC_FETCH_AND_ADD
  return __sync_add_and_fetch (&u->waiting, -1);
#else
  return --u->waiting;
#endif
}

static inline void
dec_waiting_unlocked (gfc_unit *u)
{
#ifdef HAVE_SYNC_FETCH_AND_ADD
  (void) __sync_fetch_and_add (&u->waiting, -1);
#else
  __gthread_mutex_lock (&unit_lock);
  u->waiting--;
  __gthread_mutex_unlock (&unit_lock);
#endif
}

#endif

