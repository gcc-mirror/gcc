/* Copyright (C) 2002-2017 Free Software Foundation, Inc.
   Contributed by Andy Vaught
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

#ifndef GFOR_IO_H
#define GFOR_IO_H

/* IO library include.  */

#include "libgfortran.h"

#include <gthr.h>


/* POSIX 2008 specifies that the extended locale stuff is found in
   locale.h, but some systems have them in xlocale.h.  */

#include <locale.h>

#ifdef HAVE_XLOCALE_H
#include <xlocale.h>
#endif


/* Forward declarations.  */
struct st_parameter_dt;
typedef struct stream stream;
struct fbuf;
struct format_data;
typedef struct fnode fnode;
struct gfc_unit;

#ifdef HAVE_NEWLOCALE
/* We have POSIX 2008 extended locale stuff.  */
extern locale_t c_locale;
internal_proto(c_locale);
#else
extern char* old_locale;
internal_proto(old_locale);
extern int old_locale_ctr;
internal_proto(old_locale_ctr);
extern __gthread_mutex_t old_locale_lock;
internal_proto(old_locale_lock);
#endif


/* Macros for testing what kinds of I/O we are doing.  */

#define is_array_io(dtp) ((dtp)->internal_unit_desc)

#define is_internal_unit(dtp) ((dtp)->u.p.current_unit->internal_unit_kind)

#define is_stream_io(dtp) ((dtp)->u.p.current_unit->flags.access == ACCESS_STREAM)

#define is_char4_unit(dtp) ((dtp)->u.p.current_unit->internal_unit_kind == 4)

/* The array_loop_spec contains the variables for the loops over index ranges
   that are encountered.  */

typedef struct array_loop_spec
{
  /* Index counter for this dimension.  */
  index_type idx;

  /* Start for the index counter.  */
  index_type start;

  /* End for the index counter.  */
  index_type end;

  /* Step for the index counter.  */
  index_type step;
}
array_loop_spec;

/* User defined input/output iomsg length. */

#define IOMSG_LEN 256

/* Subroutine formatted_dtio (struct, unit, iotype, v_list, iostat,
			      iomsg, (_iotype), (_iomsg))  */
typedef void (*formatted_dtio)(void *, GFC_INTEGER_4 *, char *, gfc_array_i4 *,
			       GFC_INTEGER_4 *, char *,
			       gfc_charlen_type, gfc_charlen_type);

/* Subroutine unformatted_dtio (struct, unit, iostat, iomsg, (_iomsg))  */
typedef void (*unformatted_dtio)(void *, GFC_INTEGER_4 *, GFC_INTEGER_4 *,
				 char *, gfc_charlen_type);

/* The dtio calls for namelist require a CLASS object to be built.  */
typedef struct gfc_class
{
  void *data;
  void *vptr;
  index_type len;
}
gfc_class;


/* A structure to build a hash table for format data.  */

#define FORMAT_HASH_SIZE 16

typedef struct format_hash_entry
{
  char *key;
  gfc_charlen_type key_len;
  struct format_data *hashed_fmt;
}
format_hash_entry;

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
  /* Object type.  */
  bt type;

  /* Object name.  */
  char * var_name;

  /* Address for the start of the object's data.  */
  void * mem_pos;

  /* Address of specific DTIO subroutine.  */
  void * dtio_sub;

  /* Address of vtable if dtio_sub non-null.  */
  void * vtable;

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
{ ROUND_UP = GFC_FPE_UPWARD,
  ROUND_DOWN = GFC_FPE_DOWNWARD,
  ROUND_ZERO = GFC_FPE_TOWARDZERO,
  ROUND_NEAREST = GFC_FPE_TONEAREST,
  ROUND_COMPATIBLE = 10, /* round away from zero.  */
  ROUND_PROCDEFINED, /* Here as ROUND_NEAREST. */
  ROUND_UNSPECIFIED /* Should never occur. */
}
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
{READING, WRITING, LIST_READING, LIST_WRITING}
unit_mode;

typedef enum
{ ASYNC_YES, ASYNC_NO, ASYNC_UNSPECIFIED }
unit_async;

typedef enum
{ SHARE_DENYRW, SHARE_DENYNONE,
  SHARE_UNSPECIFIED
}
unit_share;

typedef enum
{ CC_LIST, CC_FORTRAN, CC_NONE,
  CC_UNSPECIFIED
}
unit_cc;

/* End-of-record types for CC_FORTRAN.  */
typedef enum
{ CCF_DEFAULT=0x0,
  CCF_OVERPRINT=0x1,
  CCF_ONE_LF=0x2,
  CCF_TWO_LF=0x4,
  CCF_PAGE_FEED=0x8,
  CCF_PROMPT=0x10,
  CCF_OVERPRINT_NOA=0x20,
} /* 6 bits */
cc_fortran;

typedef enum
{ SIGN_S, SIGN_SS, SIGN_SP }
unit_sign_s;

/* Make sure to keep st_parameter_* in sync with gcc/fortran/ioparm.def.  */

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
  GFC_INTEGER_4 *newunit;
  GFC_INTEGER_4 readonly;
  CHARACTER2 (cc);
  CHARACTER1 (share);
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
#define IOPARM_INQUIRE_HAS_FLAGS2	(1u << 31)

#define IOPARM_INQUIRE_HAS_ASYNCHRONOUS	(1 << 0)
#define IOPARM_INQUIRE_HAS_DECIMAL	(1 << 1)
#define IOPARM_INQUIRE_HAS_ENCODING	(1 << 2)
#define IOPARM_INQUIRE_HAS_ROUND	(1 << 3)
#define IOPARM_INQUIRE_HAS_SIGN		(1 << 4)
#define IOPARM_INQUIRE_HAS_PENDING	(1 << 5)
#define IOPARM_INQUIRE_HAS_SIZE		(1 << 6)
#define IOPARM_INQUIRE_HAS_ID		(1 << 7)
#define IOPARM_INQUIRE_HAS_IQSTREAM	(1 << 8)
#define IOPARM_INQUIRE_HAS_SHARE	(1 << 9)
#define IOPARM_INQUIRE_HAS_CC		(1 << 10)

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
  GFC_IO_INT *size;
  GFC_INTEGER_4 *id;
  CHARACTER1 (iqstream);
  CHARACTER2 (share);
  CHARACTER1 (cc);
}
st_parameter_inquire;


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
#define IOPARM_DT_HAS_UDTIO                     (1 << 26)
#define IOPARM_DT_DEFAULT_EXP			(1 << 27)
/* Internal use bit.  */
#define IOPARM_DT_IONML_SET			(1u << 31)


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
  GFC_INTEGER_4 *id;
  GFC_IO_INT pos;
  CHARACTER1 (asynchronous);
  CHARACTER2 (blank);
  CHARACTER1 (decimal);
  CHARACTER2 (delim);
  CHARACTER1 (pad);
  CHARACTER2 (round);
  CHARACTER1 (sign);
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
	  /* Maximum righthand column written to.  */
	  int max_pos;
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
	     to flag that calls are being made from namelist read (e.g. to
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
	  /* Used to signal use of free_format_data.  */
	  unsigned format_not_saved : 1;
	  /* A flag used to identify when a non-standard expanded namelist read
	     has occurred.  */
	  unsigned expanded_read : 1;
	  /* 13 unused bits.  */

	  int child_saved_iostat;
	  int nml_delim;
	  int repeat_count;
	  int saved_length;
	  int saved_used;
	  bt saved_type;
	  char *saved_string;
	  char *scratch;
	  char *line_buffer;
	  struct format_data *fmt;
	  namelist_info *ionml;
#ifdef HAVE_NEWLOCALE
	  locale_t old_locale;
#endif
	  /* Current position within the look-ahead line buffer.  */
	  int line_buffer_pos;
	  /* Storage area for values except for strings.  Must be
	     large enough to hold a complex value (two reals) of the
	     largest kind.  */
	  char value[32];
	  GFC_IO_INT not_used; /* Needed for alignment. */
	  formatted_dtio fdtio_ptr;
	  unformatted_dtio ufdtio_ptr;
	  /* With CC_FORTRAN, the first character of a record determines the
	     style of record end (and start) to use. We must mark down the type
	     when we write first in write_a so we remember the end type later in
	     next_record_w.  */
	  struct
	    {
	      unsigned type : 6; /* See enum cc_fortran.  */
	      unsigned len  : 2; /* Always 0, 1, or 2.  */
	      /* The union is updated after start-of-record is written.  */
	      union
		{
		  char start; /* Output character for start of record.  */
		  char end;   /* Output character for end of record.  */
		} u;
	    } cc;
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
  unit_share share;
  unit_cc cc;
  int readonly;
}
unit_flags;


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
  unit_round round_status;

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

  /* Name of the file at the time OPEN was executed, as a
     null-terminated C string.  */
  char *filename;

  /* The format hash table.  */
  struct format_hash_entry format_hash_table[FORMAT_HASH_SIZE];
  
  /* Formatting buffer.  */
  struct fbuf *fbuf;
  
  /* Function pointer, points to list_read worker functions.  */
  int (*next_char_fn_ptr) (st_parameter_dt *);
  void (*push_char_fn_ptr) (st_parameter_dt *, int);

  /* Internal unit char string data.  */
  char * internal_unit;
  gfc_charlen_type internal_unit_len;
  gfc_array_char *string_unit_desc;
  int internal_unit_kind;

  /* DTIO Parent/Child procedure, 0 = parent, >0 = child level.  */
  int child_dtio;

  /* Used for ungetc() style functionality. Possible values
     are an unsigned char, EOF, or EOF - 1 used to mark the
     field as not valid.  */
  int last_char;
  bool has_size;
  GFC_IO_INT size_used;
}
gfc_unit;

typedef struct gfc_saved_unit
{
  GFC_INTEGER_4 unit_number;
  gfc_unit *unit;
}
gfc_saved_unit;

/* TEMP_FAILURE_RETRY macro from glibc.  */

#ifndef TEMP_FAILURE_RETRY
/* Evaluate EXPRESSION, and repeat as long as it returns -1 with `errno'
   set to EINTR.  */

# define TEMP_FAILURE_RETRY(expression) \
  (__extension__                                                              \
    ({ long int __result;                                                     \
       do __result = (long int) (expression);                                 \
       while (__result == -1L && errno == EINTR);                             \
       __result; }))
#endif


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

extern gfc_unit *set_internal_unit (st_parameter_dt *, gfc_unit *, int);
internal_proto(set_internal_unit);

extern void stash_internal_unit (st_parameter_dt *);
internal_proto(stash_internal_unit);

extern gfc_unit *find_unit (int);
internal_proto(find_unit);

extern gfc_unit *find_or_create_unit (int);
internal_proto(find_or_create_unit);

extern gfc_unit *get_unit (st_parameter_dt *, int);
internal_proto(get_unit);

extern void unlock_unit (gfc_unit *);
internal_proto(unlock_unit);

extern void finish_last_advance_record (gfc_unit *u);
internal_proto (finish_last_advance_record);

extern int unit_truncate (gfc_unit *, gfc_offset, st_parameter_common *);
internal_proto (unit_truncate);

extern int newunit_alloc (void);
internal_proto(newunit_alloc);


/* open.c */

extern gfc_unit *new_unit (st_parameter_open *, gfc_unit *, unit_flags *);
internal_proto(new_unit);


/* transfer.c */

#define SCRATCH_SIZE 300

extern const char *type_name (bt);
internal_proto(type_name);

extern void * read_block_form (st_parameter_dt *, int *);
internal_proto(read_block_form);

extern void * read_block_form4 (st_parameter_dt *, int *);
internal_proto(read_block_form4);

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

extern void st_wait (st_parameter_wait *);
export_proto(st_wait);

extern void hit_eof (st_parameter_dt *);
internal_proto(hit_eof);

/* read.c */

extern void set_integer (void *, GFC_INTEGER_LARGEST, int);
internal_proto(set_integer);

extern GFC_UINTEGER_LARGEST si_max (int);
internal_proto(si_max);

extern int convert_real (st_parameter_dt *, void *, const char *, int);
internal_proto(convert_real);

extern int convert_infnan (st_parameter_dt *, void *, const char *, int);
internal_proto(convert_infnan);

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

extern void read_user_defined (st_parameter_dt *, void *);
internal_proto(read_user_defined);

extern void read_user_defined (st_parameter_dt *, void *);
internal_proto(read_user_defined);

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

extern void write_user_defined (st_parameter_dt *, void *);
internal_proto(write_user_defined);

extern void write_user_defined (st_parameter_dt *, void *);
internal_proto(write_user_defined);

extern void list_formatted_write (st_parameter_dt *, bt, void *, int, size_t,
				  size_t);
internal_proto(list_formatted_write);

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


static inline void
memset4 (gfc_char4_t *p, gfc_char4_t c, int k)
{
  int j;
  for (j = 0; j < k; j++)
    *p++ = c;
}

#endif

