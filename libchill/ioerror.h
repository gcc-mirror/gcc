/* Implement Input/Output runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser, et al

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef _ioerror_h_
#define _ioerror_h_

#include <setjmp.h>

/* Note: numbers must be in the same order as
   strings in ioerror.c */
typedef enum 
{ NOTASSOCIATED = 1,
  ASSOCIATEFAIL,
  CREATEFAIL, 
  DELETEFAIL,
  MODIFYFAIL,
  CONNECTFAIL,
  NOTCONNECTED,
  EMPTY,
  RANGEFAIL,
  SPACEFAIL,
  READFAIL,
  WRITEFAIL,
  TEXTFAIL
} io_exceptions_t;

#ifndef FIRST_IO_ERROR_NUMBER
#define FIRST_IO_ERROR_NUMBER 0
#endif

typedef enum {
  FIRST_AND_UNUSED = FIRST_IO_ERROR_NUMBER,
  INTERNAL_ERROR,
  INVALID_IO_LIST,
  REPFAC_OVERFLOW,
  CLAUSE_WIDTH_OVERFLOW,
  UNMATCHED_CLOSING_PAREN,
  UNMATCHED_OPENING_PAREN,
  BAD_FORMAT_SPEC_CHAR,
  NO_PAD_CHAR,
  IO_CONTROL_NOT_VALID,
  DUPLICATE_QUALIFIER,
  NO_FRACTION_WIDTH,
  NO_EXPONENT_WIDTH,
  FRACTION_WIDTH_OVERFLOW,
  EXPONENT_WIDTH_OVERFLOW,
  NO_FRACTION,
  NO_EXPONENT,
  NEGATIVE_FIELD_WIDTH,
  TEXT_LOC_OVERFLOW,
  IOLIST_EXHAUSTED,
  CONVCODE_MODE_MISFIT,
  SET_CONVERSION_ERROR,
  BOOL_CONVERSION_ERROR,
  NON_INT_FIELD_WIDTH,
  EXCESS_IOLIST_ELEMENTS,
  NOT_ENOUGH_CHARS,
  NO_CHARS_FOR_INT,
  NO_CHARS_FOR_FLOAT,
  NO_EXPONENT_VAL,
  INT_VAL_OVERFLOW,
  REAL_OVERFLOW,
  NO_DIGITS_FOR_INT,
  NO_DIGITS_FOR_FLOAT,
  NO_CHARS_FOR_SET,
  NO_CHARS_FOR_CHAR,
  NO_CHARS_FOR_BOOLS,
  NO_CHARS_FOR_CHARS,
  NO_CHARS_FOR_TEXT,
  NO_CHARS_FOR_EDIT,
  NO_SPACE_TO_SKIP,
  FORMAT_TEXT_MISMATCH,
  INTEGER_RANGE_ERROR,
  SET_RANGE_ERROR,
  CHAR_RANGE_ERROR,
  INVALID_CHAR,
/* end of formatting errors */
  NULL_ASSOCIATION,
  NULL_ACCESS,
  NULL_TEXT,
  IS_NOT_ASSOCIATED,
  IS_ASSOCIATED,
  GETCWD_FAILS,
  INVALID_ASSOCIATION_MODE,
  FILE_EXISTING,
  CREATE_FAILS,
  DELETE_FAILS,
  RENAME_FAILS,
  IMPL_RESTRICTION,
  NOT_EXISTING,
  NOT_READABLE,
  NOT_WRITEABLE,
  NOT_INDEXABLE,
  NOT_SEQUENCIBLE,
  NO_CURRENT_POS,
  NOT_VARIABLE,
  NOT_FIXED,
  NOT_INDEXED, 
  LENGTH_CHANGE,
  LSEEK_FAILS,
  BUFFER_ALLOC,
  OPEN_FAILS,
  NO_ACCESS_SUBLOCATION, 
  BAD_INDEX,
  IS_NOT_CONNECTED,
  NO_PATH_NAME,
  PATHNAME_ALLOC,
  BAD_USAGE,
  OUT_OF_FILE,
  NULL_STORE_LOC,
  STORE_LOC_ALLOC,
  OS_IO_ERROR,
  RECORD_TOO_LONG,
  RECORD_TOO_SHORT,
  BAD_TEXTINDEX,
  NULL_TEXTREC
} io_info_word_t;


extern
char* io_info_text [];

extern
char* exc_text [];
 
extern 
jmp_buf __io_exception;

extern 
jmp_buf __rw_exception;

void __cause_exception (char *ex, char* f, int line, int info);
extern char * __IO_exception_names[];

#define IOEXCEPTION(EXC,INFO) \
    longjmp( __io_exception, (EXC<<16) + INFO )

#define RWEXCEPTION(EXC,INFO) \
    longjmp( __rw_exception, (EXC<<16) + INFO )

#define CHILLEXCEPTION(FILE,LINE,EXC,INFO) \
    __cause_exception (__IO_exception_names[EXC], FILE, LINE, INFO);

#endif
