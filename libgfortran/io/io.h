/* Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
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

#ifndef GFOR_IO_H
#define GFOR_IO_H

/* IO library include.  */

#include <setjmp.h>
#include "libgfortran.h"
#define DEFAULT_TEMPDIR "/var/tmp"

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

/* Namelist represent object */
/*
   Namelist Records
       &groupname  object=value [,object=value].../
     or
       &groupname  object=value [,object=value]...&groupname

  Even more complex, during the execution of a program containing a
  namelist READ statement, you can specify a question mark character(?)
  or a question mark character preceded by an equal sign(=?) to get
  the information of the namelist group. By '?', the name of variables
  in the namelist will be displayed, by '=?', the name and value of
  variables will be displayed.

  All these requirements need a new data structure to record all info
  about the namelist.
*/

typedef struct namelist_type
{
  char * var_name;
  void * mem_pos;
  int  value_acquired;
  int len;
  int string_length;
  bt type;
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
  int unit;
  int err, end, eor, list_format;	/* These are flags, not values.  */

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

  int *iostat, *exist, *opened, *number, *named, rec, *nextrec, *size;

  int recl_in; 
  int *recl_out;

  int *iolength;

  char *file;
  int file_len;
  char *status;
  int status_len;
  char *access;
  int access_len;
  char *form;
  int form_len;
  char *blank;
  int blank_len;
  char *position;
  int position_len;
  char *action;
  int action_len;
  char *delim;
  int delim_len;
  char *pad;
  int pad_len;
  char *format;
  int format_len;
  char *advance;
  int advance_len;
  char *name;
  int name_len;
  char *internal_unit;
  int internal_unit_len;
  char *sequential;
  int sequential_len;
  char *direct;
  int direct_len;
  char *formatted;
  int formatted_len;
  char *unformatted;
  int unformatted_len;
  char *read;
  int read_len;
  char *write;
  int write_len;
  char *readwrite;
  int readwrite_len;

/* namelist related data */
  char * namelist_name;
  int namelist_name_len;
  int namelist_read_mode;
}
st_parameter;



#define ioparm prefix(ioparm)
extern st_parameter ioparm;

#define ionml prefix(ionml)
extern namelist_info * ionml;

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


/* The default value of record length is defined here.  This value can
   be overriden by the OPEN statement or by an environment variable.  */

#define DEFAULT_RECL 10000


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


#define g prefix(g)
extern global_t g;


#define current_unit prefix(current_unit)
extern gfc_unit *current_unit;

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

#define sys_exit prefix(sys_exit)
void sys_exit (int) __attribute__ ((noreturn));

#define move_pos_offset prefix(move_pos_offset)
int move_pos_offset (stream *, int);

#define get_oserror prefix(get_oserror)
const char *get_oserror (void);

#define compare_files prefix(compare_files)
int compare_files (stream *, stream *);

#define init_error_stream prefix(init_error_stream)
stream *init_error_stream (void);

#define open_external prefix(open_external)
stream *open_external (unit_action, unit_status);

#define open_internal prefix(open_internal)
stream *open_internal (char *, int);

#define input_stream prefix(input_stream)
stream *input_stream (void);

#define output_stream prefix(output_stream)
stream *output_stream (void);

#define compare_file_filename prefix(compare_file_filename)
int compare_file_filename (stream *, const char *, int);

#define find_file prefix(find_file)
gfc_unit *find_file (void);

#define stream_at_bof prefix(stream_at_bof)
int stream_at_bof (stream *);

#define stream_at_eof prefix(stream_at_eof)
int stream_at_eof (stream *);

#define delete_file prefix(delete_file)
int delete_file (gfc_unit *);

#define file_exists prefix(file_exists)
int file_exists (void);

#define inquire_sequential prefix(inquire_sequential)
const char *inquire_sequential (const char *, int);

#define inquire_direct prefix(inquire_direct)
const char *inquire_direct (const char *, int);

#define inquire_formatted prefix(inquire_formatted)
const char *inquire_formatted (const char *, int);

#define inquire_unformatted prefix(inquire_unformatted)
const char *inquire_unformatted (const char *, int);

#define inquire_read prefix(inquire_read)
const char *inquire_read (const char *, int);

#define inquire_write prefix(inquire_write)
const char *inquire_write (const char *, int);

#define inquire_readwrite prefix(inquire_readwrite)
const char *inquire_readwrite (const char *, int);

#define file_length prefix(file_length)
gfc_offset file_length (stream *);

#define file_position prefix(file_position)
gfc_offset file_position (stream *);

#define is_seekable prefix(is_seekable)
int is_seekable (stream *);

#define empty_internal_buffer prefix(empty_internal_buffer)
void empty_internal_buffer(stream *);

#define flush prefix(flush)
try flush (stream *);


/* unit.c */

#define insert_unit prefix(insert_unix)
void insert_unit (gfc_unit *);

#define close_unit prefix(close_unit)
int close_unit (gfc_unit *);

#define is_internal_unit prefix(is_internal_unit)
int is_internal_unit (void);

#define find_unit prefix(find_unit)
gfc_unit *find_unit (int);

#define get_unit prefix(get_unit)
gfc_unit *get_unit (int);

/* open.c */

#define test_endfile prefix(test_endfile)
void test_endfile (gfc_unit *);

#define new_unit prefix(new_unit)
void new_unit (unit_flags *);

/* format.c */

#define parse_format prefix(parse_format)
void parse_format (void);

#define next_format prefix(next_format)
fnode *next_format (void);

#define unget_format prefix(unget_format)
void unget_format (fnode *);

#define format_error prefix(format_error)
void format_error (fnode *, const char *);

#define free_fnodes prefix(free_fnodes)
void free_fnodes (void);

/* transfer.c */

#define SCRATCH_SIZE 300

#define scratch prefix(scratch)
extern char scratch[];

#define type_name prefix(type_name)
const char *type_name (bt);

#define read_block prefix(read_block)
void *read_block (int *);

#define write_block prefix(write_block)
void *write_block (int);

#define transfer_integer prefix(transfer_integer)
void transfer_integer (void *, int);

#define transfer_real prefix(transfer_real)
void transfer_real (void *, int);

#define transfer_logical prefix(transfer_logical)
void transfer_logical (void *, int);

#define transfer_character prefix(transfer_character)
void transfer_character (void *, int);

#define transfer_complex prefix(transfer_complex)
void transfer_complex (void *, int);

#define next_record prefix(next_record)
void next_record (int);

#define st_set_nml_var_int prefix(st_set_nml_var_int)
void st_set_nml_var_int (void * , char * , int , int );

#define st_set_nml_var_float prefix(st_set_nml_var_float)
void st_set_nml_var_float (void * , char * , int , int );

#define st_set_nml_var_char prefix(st_set_nml_var_char)
void st_set_nml_var_char (void * , char * , int , int, gfc_charlen_type);

#define st_set_nml_var_complex prefix(st_set_nml_var_complex)
void st_set_nml_var_complex (void * , char * , int , int );

#define st_set_nml_var_log prefix(st_set_nml_var_log)
void st_set_nml_var_log (void * , char * , int , int );

/* read.c */

#define set_integer prefix(set_integer)
void set_integer (void *, int64_t, int);

#define max_value prefix(max_value)
uint64_t max_value (int, int);

#define convert_real prefix(convert_real)
int convert_real (void *, const char *, int);

#define read_a prefix(read_a)
void read_a (fnode *, char *, int);

#define read_f prefix(read_f)
void read_f (fnode *, char *, int);

#define read_l prefix(read_l)
void read_l (fnode *, char *, int);

#define read_x prefix(read_x)
void read_x (fnode *);

#define read_radix prefix(read_radix)
void read_radix (fnode *, char *, int, int);

#define read_decimal prefix(read_decimal)
void read_decimal (fnode *, char *, int);

/* list_read.c */

#define list_formatted_read prefix(list_formatted_read)
void list_formatted_read (bt, void *, int);

#define finish_list_read prefix(finish_list_read)
void finish_list_read (void);

#define init_at_eol prefix(init_at_eol)
void init_at_eol();

#define namelist_read prefix(namelist_read)
void namelist_read();

#define namelist_write prefix(namelist_write)
void namelist_write();

/* write.c */

#define write_a prefix(write_a)
void write_a (fnode *, const char *, int);

#define write_b prefix(write_b)
void write_b (fnode *, const char *, int);

#define write_d prefix(write_d)
void write_d (fnode *, const char *, int);

#define write_e prefix(write_e)
void write_e (fnode *, const char *, int);

#define write_en prefix(write_en)
void write_en (fnode *, const char *, int);

#define write_es prefix(write_es)
void write_es (fnode *, const char *, int);

#define write_f prefix(write_f)
void write_f (fnode *, const char *, int);

#define write_i prefix(write_i)
void write_i (fnode *, const char *, int);

#define write_l prefix(write_l)
void write_l (fnode *, char *, int);

#define write_o prefix(write_o)
void write_o (fnode *, const char *, int);

#define write_x prefix(write_x)
void write_x (fnode *);

#define write_z prefix(write_z)
void write_z (fnode *, const char *, int);

#define list_formatted_write prefix(list_formatted_write)
void list_formatted_write (bt, void *, int);


#define st_open prefix(st_open)
#define st_close prefix(st_close)
#define st_inquire prefix(st_inquire)
#define st_iolength prefix(st_iolength)
#define st_iolength_done prefix(st_iolength_done)
#define st_rewind prefix(st_rewind)
#define st_read prefix(st_read)
#define st_read_done prefix(st_read_done)
#define st_write prefix(st_write)
#define st_write_done prefix(st_write_done)
#define st_backspace prefix(st_backspace)
#define st_endfile prefix(st_endfile)


void __MAIN (void);

#endif
