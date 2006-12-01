/* Copyright (C) 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef GFOR_IO_H
#define GFOR_IO_H

/* IO library include.  */

#include <setjmp.h>
#include "libgfortran.h"

#include <gthr.h>

#define DEFAULT_TEMPDIR "/tmp"

/* Basic types used in data transfers.  */

typedef enum
{ BT_NULL, BT_INTEGER, BT_LOGICAL, BT_CHARACTER, BT_REAL,
  BT_COMPLEX
}
bt;


struct st_parameter_dt;

typedef struct stream
{
  char *(*alloc_w_at) (struct stream *, int *, gfc_offset);
  char *(*alloc_r_at) (struct stream *, int *, gfc_offset);
  try (*sfree) (struct stream *);
  try (*close) (struct stream *);
  try (*seek) (struct stream *, gfc_offset);
  try (*truncate) (struct stream *);
  int (*read) (struct stream *, void *, size_t *);
  int (*write) (struct stream *, const void *, size_t *);
  try (*set) (struct stream *, int, size_t);
}
stream;


/* Macros for doing file I/O given a stream.  */

#define sfree(s) ((s)->sfree)(s)
#define sclose(s) ((s)->close)(s)

#define salloc_r(s, len) ((s)->alloc_r_at)(s, len, -1)
#define salloc_w(s, len) ((s)->alloc_w_at)(s, len, -1)

#define salloc_r_at(s, len, where) ((s)->alloc_r_at)(s, len, where)
#define salloc_w_at(s, len, where) ((s)->alloc_w_at)(s, len, where)

#define sseek(s, pos) ((s)->seek)(s, pos)
#define struncate(s) ((s)->truncate)(s)
#define sread(s, buf, nbytes) ((s)->read)(s, buf, nbytes)
#define swrite(s, buf, nbytes) ((s)->write)(s, buf, nbytes)

#define sset(s, c, n) ((s)->set)(s, c, n)

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
{ ADVANCE_YES, ADVANCE_NO, ADVANCE_UNSPECIFIED }
unit_advance;

typedef enum
{READING, WRITING}
unit_mode;

typedef enum
{ CONVERT_NONE=-1, CONVERT_NATIVE, CONVERT_SWAP, CONVERT_BIG, CONVERT_LITTLE }
unit_convert;

#define CHARACTER1(name) \
	      char * name; \
	      gfc_charlen_type name ## _len
#define CHARACTER2(name) \
	      gfc_charlen_type name ## _len; \
	      char * name

#define IOPARM_LIBRETURN_MASK		(3 << 0)
#define IOPARM_LIBRETURN_OK		(0 << 0)
#define IOPARM_LIBRETURN_ERROR		(1 << 0)
#define IOPARM_LIBRETURN_END		(2 << 0)
#define IOPARM_LIBRETURN_EOR		(3 << 0)
#define IOPARM_ERR			(1 << 2)
#define IOPARM_END			(1 << 3)
#define IOPARM_EOR			(1 << 4)
#define IOPARM_HAS_IOSTAT		(1 << 5)
#define IOPARM_HAS_IOMSG		(1 << 6)

#define IOPARM_COMMON_MASK		((1 << 7) - 1)

typedef struct st_parameter_common
{
  GFC_INTEGER_4 flags;
  GFC_INTEGER_4 unit;
  const char *filename;
  GFC_INTEGER_4 line;
  CHARACTER2 (iomsg);
  GFC_INTEGER_4 *iostat;
}
st_parameter_common;

#define IOPARM_OPEN_HAS_RECL_IN		(1 << 7)
#define IOPARM_OPEN_HAS_FILE		(1 << 8)
#define IOPARM_OPEN_HAS_STATUS		(1 << 9)
#define IOPARM_OPEN_HAS_ACCESS		(1 << 10)
#define IOPARM_OPEN_HAS_FORM		(1 << 11)
#define IOPARM_OPEN_HAS_BLANK		(1 << 12)
#define IOPARM_OPEN_HAS_POSITION	(1 << 13)
#define IOPARM_OPEN_HAS_ACTION		(1 << 14)
#define IOPARM_OPEN_HAS_DELIM		(1 << 15)
#define IOPARM_OPEN_HAS_PAD		(1 << 16)
#define IOPARM_OPEN_HAS_CONVERT		(1 << 17)

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
/* Internal use bit.  */
#define IOPARM_DT_IONML_SET			(1 << 31)

typedef struct st_parameter_dt
{
  st_parameter_common common;
  GFC_IO_INT rec;
  GFC_INTEGER_4 *size, *iolength;
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
	  enum {SIGN_S, SIGN_SS, SIGN_SP} sign_status;
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
	  /* 16 unused bits.  */

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
	  /* Storage area for values except for strings.  Must be large
	     enough to hold a complex value (two reals) of the largest
	     kind.  */
	  char value[32];
	  gfc_offset size_used;
	} p;
      /* This pad size must be equal to the pad_size declared in
	 trans-io.c (gfc_build_io_library_fndecls).  The above structure
	 must be smaller or equal to this array.  */
      char pad[16 * sizeof (char *) + 32 * sizeof (int)];
    } u;
}
st_parameter_dt;

/* Ensure st_parameter_dt's u.pad is bigger or equal to u.p.  */
extern char check_st_parameter_dt[sizeof (((st_parameter_dt *) 0)->u.pad)
				  >= sizeof (((st_parameter_dt *) 0)->u.p)
				  ? 1 : -1];

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
}
unit_flags;


/* The default value of record length for preconnected units is defined
   here. This value can be overriden by an environment variable.
   Default value is 1 Gb.  */

#define DEFAULT_RECL 1073741824


typedef struct gfc_unit
{
  int unit_number;
  stream *s;
  
  /* Treap links.  */
  struct gfc_unit *left, *right;
  int priority;

  int read_bad, current_record;
  enum
  { NO_ENDFILE, AT_ENDFILE, AFTER_ENDFILE }
  endfile;

  unit_mode mode;
  unit_flags flags;

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
  FMT_E, FMT_EN, FMT_ES, FMT_G, FMT_L, FMT_A, FMT_D, FMT_H, FMT_END
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

extern int move_pos_offset (stream *, int);
internal_proto(move_pos_offset);

extern int compare_files (stream *, stream *);
internal_proto(compare_files);

extern stream *open_external (st_parameter_open *, unit_flags *);
internal_proto(open_external);

extern stream *open_internal (char *, int);
internal_proto(open_internal);

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

extern void flush_all_units (void);
internal_proto(flush_all_units);

extern int stream_at_bof (stream *);
internal_proto(stream_at_bof);

extern int stream_at_eof (stream *);
internal_proto(stream_at_eof);

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

extern gfc_offset file_position (stream *);
internal_proto(file_position);

extern int is_seekable (stream *);
internal_proto(is_seekable);

extern int is_preconnected (stream *);
internal_proto(is_preconnected);

extern void flush_if_preconnected (stream *);
internal_proto(flush_if_preconnected);

extern void empty_internal_buffer(stream *);
internal_proto(empty_internal_buffer);

extern try flush (stream *);
internal_proto(flush);

extern int stream_isatty (stream *);
internal_proto(stream_isatty);

extern char * stream_ttyname (stream *);
internal_proto(stream_ttyname);

extern gfc_offset stream_offset (stream *s);
internal_proto(stream_offset);

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

extern int is_internal_unit (st_parameter_dt *);
internal_proto(is_internal_unit);

extern int is_array_io (st_parameter_dt *);
internal_proto(is_array_io);

extern int is_stream_io (st_parameter_dt *);
internal_proto(is_stream_io);

extern gfc_unit *find_unit (int);
internal_proto(find_unit);

extern gfc_unit *find_or_create_unit (int);
internal_proto(find_or_create_unit);

extern gfc_unit *get_unit (st_parameter_dt *, int);
internal_proto(get_unit);

extern void unlock_unit (gfc_unit *);
internal_proto(unlock_unit);

/* open.c */

extern void test_endfile (gfc_unit *);
internal_proto(test_endfile);

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

extern void *read_block (st_parameter_dt *, int *);
internal_proto(read_block);

extern char *read_sf (st_parameter_dt *, int *, int);
internal_proto(read_sf);

extern void *write_block (st_parameter_dt *, int);
internal_proto(write_block);

extern gfc_offset next_array_record (st_parameter_dt *, array_loop_spec *);
internal_proto(next_array_record);

extern gfc_offset init_loop_spec (gfc_array_char *, array_loop_spec *);
internal_proto(init_loop_spec);

extern void next_record (st_parameter_dt *, int);
internal_proto(next_record);

extern void reverse_memcpy (void *, const void *, size_t);
internal_proto (reverse_memcpy);

/* read.c */

extern void set_integer (void *, GFC_INTEGER_LARGEST, int);
internal_proto(set_integer);

extern GFC_UINTEGER_LARGEST max_value (int, int);
internal_proto(max_value);

extern int convert_real (st_parameter_dt *, void *, const char *, int);
internal_proto(convert_real);

extern void read_a (st_parameter_dt *, const fnode *, char *, int);
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

extern void write_x (st_parameter_dt *, int, int);
internal_proto(write_x);

extern void write_z (st_parameter_dt *, const fnode *, const char *, int);
internal_proto(write_z);

extern void list_formatted_write (st_parameter_dt *, bt, void *, int, size_t,
				  size_t);
internal_proto(list_formatted_write);

/* error.c */
extern notification notification_std(int);
internal_proto(notification_std);

/* size_from_kind.c */
extern size_t size_from_real_kind (int);
internal_proto(size_from_real_kind);

extern size_t size_from_complex_kind (int);
internal_proto(size_from_complex_kind);

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

/* ../runtime/environ.c  This is here because we return unit_convert.  */

unit_convert get_unformatted_convert (int);
internal_proto(get_unformatted_convert);
