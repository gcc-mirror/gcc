/* Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

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

#define DEFAULT_TEMPDIR "/tmp"

/* Basic types used in data transfers.  */

typedef enum
{ BT_NULL, BT_INTEGER, BT_LOGICAL, BT_CHARACTER, BT_REAL,
  BT_COMPLEX
}
bt;


typedef enum
{ SUCCESS = 1, FAILURE }
try;

typedef struct stream
{
  char *(*alloc_w_at) (struct stream *, int *, gfc_offset);
  char *(*alloc_r_at) (struct stream *, int *, gfc_offset);
  try (*sfree) (struct stream *);
  try (*close) (struct stream *);
  try (*seek) (struct stream *, gfc_offset);
  try (*truncate) (struct stream *);
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

/* Representation of a namelist object in libgfortran

   Namelist Records
      &GROUPNAME  OBJECT=value[s] [,OBJECT=value[s]].../
     or
      &GROUPNAME  OBJECT=value[s] [,OBJECT=value[s]]...&END

   The object can be a fully qualified, compound name for an instrinsic
   type, derived types or derived type components.  So, a substring
   a(:)%b(4)%ch(2:4)(1:7) has to be treated correctly in namelist
   read. Hence full information about the structure of the object has
   to be available to list_read.c and write.

   These requirements are met by the following data structures.

   nml_loop_spec contains the variables for the loops over index ranges
   that are encountered.  Since the variables can be negative, ssize_t
   is used.  */

typedef struct nml_loop_spec
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
nml_loop_spec;

/* namelist_info type contains all the scalar information about the
   object and arrays of descriptor_dimension and nml_loop_spec types for
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
  nml_loop_spec * ls;
  struct namelist_type * next;
}
namelist_info;

/* Options for the OPEN statement.  */

typedef enum
{ ACCESS_SEQUENTIAL, ACCESS_DIRECT,
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

/* Statement parameters.  These are all the things that can appear in
   an I/O statement.  Some are inputs and some are outputs, but none
   are both.  All of these values are initially zeroed and are zeroed
   at the end of a library statement.  The relevant values need to be
   set before entry to an I/O statement.  This structure needs to be
   duplicated by the back end.  */

typedef struct
{
  GFC_INTEGER_4 unit;
  GFC_INTEGER_4 err, end, eor, list_format; /* These are flags, not values.  */

/* Return values from library statements.  These are returned only if
   the labels are specified in the statement itself and the condition
   occurs.  In most cases, none of the labels are specified and the
   return value does not have to be checked.  Must be consistent with
   the front end.  */

  enum
  {
    LIBRARY_OK = 0,
    LIBRARY_ERROR,
    LIBRARY_END,
    LIBRARY_EOR
  }
  library_return;

  GFC_INTEGER_4 *iostat, *exist, *opened, *number, *named;
  GFC_INTEGER_4 rec;
  GFC_INTEGER_4 *nextrec, *size;

  GFC_INTEGER_4 recl_in; 
  GFC_INTEGER_4 *recl_out;

  GFC_INTEGER_4 *iolength;

#define CHARACTER(name) \
              char * name; \
              gfc_charlen_type name ## _len
  CHARACTER (file);
  CHARACTER (status);
  CHARACTER (access);
  CHARACTER (form);
  CHARACTER (blank);
  CHARACTER (position);
  CHARACTER (action);
  CHARACTER (delim);
  CHARACTER (pad);
  CHARACTER (format);
  CHARACTER (advance);
  CHARACTER (name);
  CHARACTER (internal_unit);
  CHARACTER (sequential);
  CHARACTER (direct);
  CHARACTER (formatted);
  CHARACTER (unformatted);
  CHARACTER (read);
  CHARACTER (write);
  CHARACTER (readwrite);

/* namelist related data */
  CHARACTER (namelist_name);
  GFC_INTEGER_4 namelist_read_mode;

#undef CHARACTER
}
st_parameter;

extern st_parameter ioparm;
iexport_data_proto(ioparm);

extern namelist_info * ionml;
internal_proto(ionml);

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

  struct gfc_unit *left, *right;	/* Treap links.  */
  int priority;

  int read_bad, current_record;
  enum
  { NO_ENDFILE, AT_ENDFILE, AFTER_ENDFILE }
  endfile;

  unit_mode  mode;
  unit_flags flags;
  gfc_offset recl, last_record, maxrec, bytes_left;

  /* recl           -- Record length of the file.
     last_record    -- Last record number read or written
     maxrec         -- Maximum record number in a direct access file
     bytes_left     -- Bytes left in current record.  */

  int file_len;
  char file[1];	      /* Filename is allocated at the end of the structure.  */
}
gfc_unit;

/* Global variables.  Putting these in a structure makes it easier to
   maintain, particularly with the constraint of a prefix.  */

typedef struct
{
  int in_library;       /* Nonzero if a library call is being processed.  */
  int size;	/* Bytes processed by the current data-transfer statement.  */
  gfc_offset max_offset;	/* Maximum file offset.  */
  int item_count;	/* Item number in a formatted data transfer.  */
  int reversion_flag;	/* Format reversion has occurred.  */
  int first_item;

  gfc_unit *unit_root;
  int seen_dollar;

  unit_mode  mode;

  unit_blank blank_status;
  enum {SIGN_S, SIGN_SS, SIGN_SP} sign_status;
  int scale_factor;
  jmp_buf eof_jump;  
}
global_t;

extern global_t g;
internal_proto(g);

extern gfc_unit *current_unit;
internal_proto(current_unit);

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

extern stream *init_error_stream (void);
internal_proto(init_error_stream);

extern stream *open_external (unit_flags *);
internal_proto(open_external);

extern stream *open_internal (char *, int);
internal_proto(open_internal);

extern stream *input_stream (void);
internal_proto(input_stream);

extern stream *output_stream (void);
internal_proto(output_stream);

extern stream *error_stream (void);
internal_proto(error_stream);

extern int compare_file_filename (stream *, const char *, int);
internal_proto(compare_file_filename);

extern gfc_unit *find_file (void);
internal_proto(find_file);

extern int stream_at_bof (stream *);
internal_proto(stream_at_bof);

extern int stream_at_eof (stream *);
internal_proto(stream_at_eof);

extern int delete_file (gfc_unit *);
internal_proto(delete_file);

extern int file_exists (void);
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

extern int unit_to_fd (int);
internal_proto(unit_to_fd);

extern int unpack_filename (char *, const char *, int);
internal_proto(unpack_filename);

/* unit.c */

extern void insert_unit (gfc_unit *);
internal_proto(insert_unit);

extern int close_unit (gfc_unit *);
internal_proto(close_unit);

extern int is_internal_unit (void);
internal_proto(is_internal_unit);

extern gfc_unit *find_unit (int);
internal_proto(find_unit);

extern gfc_unit *get_unit (int);
internal_proto(get_unit);

/* open.c */

extern void test_endfile (gfc_unit *);
internal_proto(test_endfile);

extern void new_unit (unit_flags *);
internal_proto(new_unit);

/* format.c */

extern void parse_format (void);
internal_proto(parse_format);

extern fnode *next_format (void);
internal_proto(next_format);

extern void unget_format (fnode *);
internal_proto(unget_format);

extern void format_error (fnode *, const char *);
internal_proto(format_error);

extern void free_fnodes (void);
internal_proto(free_fnodes);

/* transfer.c */

#define SCRATCH_SIZE 300

extern char scratch[];
internal_proto(scratch);

extern const char *type_name (bt);
internal_proto(type_name);

extern void *read_block (int *);
internal_proto(read_block);

extern void *write_block (int);
internal_proto(write_block);

extern void next_record (int);
internal_proto(next_record);

/* read.c */

extern void set_integer (void *, int64_t, int);
internal_proto(set_integer);

extern uint64_t max_value (int, int);
internal_proto(max_value);

extern int convert_real (void *, const char *, int);
internal_proto(convert_real);

extern void read_a (fnode *, char *, int);
internal_proto(read_a);

extern void read_f (fnode *, char *, int);
internal_proto(read_f);

extern void read_l (fnode *, char *, int);
internal_proto(read_l);

extern void read_x (int);
internal_proto(read_x);

extern void read_radix (fnode *, char *, int, int);
internal_proto(read_radix);

extern void read_decimal (fnode *, char *, int);
internal_proto(read_decimal);

/* list_read.c */

extern void list_formatted_read (bt, void *, int);
internal_proto(list_formatted_read);

extern void finish_list_read (void);
internal_proto(finish_list_read);

extern void init_at_eol();
internal_proto(init_at_eol);

extern void namelist_read();
internal_proto(namelist_read);

extern void namelist_write();
internal_proto(namelist_write);

/* write.c */

extern void write_a (fnode *, const char *, int);
internal_proto(write_a);

extern void write_b (fnode *, const char *, int);
internal_proto(write_b);

extern void write_d (fnode *, const char *, int);
internal_proto(write_d);

extern void write_e (fnode *, const char *, int);
internal_proto(write_e);

extern void write_en (fnode *, const char *, int);
internal_proto(write_en);

extern void write_es (fnode *, const char *, int);
internal_proto(write_es);

extern void write_f (fnode *, const char *, int);
internal_proto(write_f);

extern void write_i (fnode *, const char *, int);
internal_proto(write_i);

extern void write_l (fnode *, char *, int);
internal_proto(write_l);

extern void write_o (fnode *, const char *, int);
internal_proto(write_o);

extern void write_x (int, int);
internal_proto(write_x);

extern void write_z (fnode *, const char *, int);
internal_proto(write_z);

extern void list_formatted_write (bt, void *, int);
internal_proto(list_formatted_write);

#endif
