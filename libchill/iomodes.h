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

#ifndef _iomodes_h_
#define _iomodes_h_

#include "auxtypes.h"

typedef enum { ReadOnly, WriteOnly, ReadWrite
} Usage_Mode;

typedef enum { First, Same, Last
} Where_Mode;

typedef enum { None, Fixed, VaryingChars
} Record_t;

/* association flags */
#define IO_ISASSOCIATED 0x00000001
#define IO_EXISTING     0x00000002
#define IO_READABLE     0x00000004
#define IO_WRITEABLE    0x00000008
#define IO_INDEXABLE    0x00000010
#define IO_SEQUENCIBLE  0x00000020
#define IO_VARIABLE     0x00000040
#define IO_FIRSTLINE    0x00000100
#define IO_FORCE_PAGE   0x00000200

struct Access_Mode;

#define READBUFLEN 512
typedef struct
{
  unsigned long len;
  unsigned long cur;
  char buf[READBUFLEN];
} readbuf_t;

typedef struct Association_Mode {
  unsigned long       flags;      /* INIT = 0 */
  char*               pathname;
  struct Access_Mode* access;
  int                 handle;
  readbuf_t*          bufptr;
  long                syserrno;
  char                usage;
  char                ctl_pre;
  char                ctl_post;
} Association_Mode;

/*
   rectype   indexed   max. reclength    act. reclength
   ---------------------------------------------------
   None        T/F        0
   Fixed       T/F     SIZE(recmode)  =  SIZE(recmode)
   Varying       F     SIZE(recmode) >=  length
*/

/* access/text flags */
#define IO_TEXTLOCATION 0x80000000
#define IO_INDEXED      0x00000001
#define IO_TEXTIO       0x00000002
#define IO_OUTOFFILE    0x00010000

typedef struct Access_Mode {
  unsigned long     flags;     /* INIT */   
  unsigned long     reclength; /* INIT */
  signed long       lowindex;  /* INIT */
  signed long       highindex; /* INIT */
  Association_Mode* association;
  unsigned long     base;
  char*             store_loc;
  Record_t          rectype;   /* INIT */
} Access_Mode;

typedef struct Text_Mode {
  unsigned long flags;         /* INIT */
  VarString*    text_record;   /* INIT */
  Access_Mode*  access_sub;    /* INIT */
  unsigned long actual_index;
} Text_Mode;

typedef enum
{
    __IO_UNUSED,

    __IO_ByteVal,
    __IO_UByteVal,
    __IO_IntVal,
    __IO_UIntVal,
    __IO_LongVal,
    __IO_ULongVal,

    __IO_ByteLoc,
    __IO_UByteLoc,
    __IO_IntLoc,
    __IO_UIntLoc,
    __IO_LongLoc,
    __IO_ULongLoc,

    __IO_ByteRangeLoc,
    __IO_UByteRangeLoc,
    __IO_IntRangeLoc,
    __IO_UIntRangeLoc,
    __IO_LongRangeLoc,
    __IO_ULongRangeLoc,

    __IO_BoolVal,
    __IO_BoolLoc,
    __IO_BoolRangeLoc,

    __IO_SetVal,
    __IO_SetLoc,
    __IO_SetRangeLoc,

    __IO_CharVal,
    __IO_CharLoc,
    __IO_CharRangeLoc,

    __IO_CharStrLoc,

    __IO_CharVaryingLoc,

    __IO_BitStrLoc,

    __IO_RealVal,
    __IO_RealLoc,
    __IO_LongRealVal,
    __IO_LongRealLoc
} __tmp_IO_enum;

typedef struct
{
    long        value;
    char*       name;
} __tmp_IO_enum_table_type;

typedef struct
{
    long                      value;
    __tmp_IO_enum_table_type* name_table;
} __tmp_WIO_set;

typedef struct
{
    char*       ptr;
    long        lower;
    long        upper;
} __tmp_IO_charrange;

typedef union
{
      signed long  slong;
    unsigned long  ulong;
}  __tmp_IO_long;

typedef struct
{
    void*         ptr;
    __tmp_IO_long lower;
    __tmp_IO_long upper;
} __tmp_IO_intrange;

typedef struct
{
    void*           ptr;
    unsigned long   lower;
    unsigned long   upper;
} __tmp_RIO_boolrange;

typedef struct
{
    void*                     ptr;
    long                      length;
    __tmp_IO_enum_table_type* name_table;
} __tmp_RIO_set;

typedef struct
{
    void*                      ptr;
    long                       length;
    __tmp_IO_enum_table_type*  name_table;
    unsigned long              lower;
    unsigned long              upper;
} __tmp_RIO_setrange;

typedef struct
{
    char*       string;
    long        string_length;
} __tmp_IO_charstring;

typedef union
{
    char                     __valbyte;
    unsigned char            __valubyte;
    short                    __valint;
    unsigned short           __valuint;
    long                     __vallong;
    unsigned long            __valulong;
    void*                    __locint;
    __tmp_IO_intrange        __locintrange;

    unsigned char            __valbool;
    unsigned char*           __locbool;
    __tmp_RIO_boolrange      __locboolrange;

    __tmp_WIO_set            __valset;
    __tmp_RIO_set            __locset;
    __tmp_RIO_setrange       __locsetrange;

    unsigned char            __valchar;
    unsigned char*           __locchar;
    __tmp_IO_charrange       __loccharrange;

    __tmp_IO_charstring      __loccharstring;

    float                    __valreal;
    float*                   __locreal;
    double                   __vallongreal;
    double*                  __loclongreal;
} __tmp_IO_union;

/*
 * CAUTION: The longest variant of __tmp_IO_union is 5 words long.
 * Together with __descr this caters for double alignment where required.
 */
typedef struct
{
    __tmp_IO_union    __t;
    __tmp_IO_enum     __descr;
} __tmp_IO_list;

#endif
