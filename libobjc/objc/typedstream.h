/* GNU Objective-C Typed Streams interface.
   Copyright (C) 1993, 1995, 2004, 2009 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#ifndef __typedstream_INCLUDE_GNU
#define __typedstream_INCLUDE_GNU

#include "objc.h"
#include "hash.h"

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef int (*objc_typed_read_func)(void*, char*, int);
typedef int (*objc_typed_write_func)(void*, const char*, int);
typedef int (*objc_typed_flush_func)(void*);
typedef int (*objc_typed_eof_func)(void*);

#define OBJC_READONLY   0x01
#define OBJC_WRITEONLY  0x02

#define OBJC_MANAGED_STREAM  0x01
#define OBJC_FILE_STREAM     0x02
#define OBJC_MEMORY_STREAM   0x04

#define OBJC_TYPED_STREAM_VERSION 0x01

typedef struct objc_typed_stream {
  void* physical;
  cache_ptr object_table;	/* read/written objects */
  cache_ptr stream_table;	/* other read/written but shared things.. */
  cache_ptr class_table;	/* class version mapping */
  cache_ptr object_refs;	/* forward references */
  int mode;			/* OBJC_READONLY or OBJC_WRITEONLY */
  int type;			/* MANAGED, FILE, MEMORY etc bit string */
  int version;			/* version used when writing */
  int writing_root_p;
  objc_typed_read_func read;
  objc_typed_write_func write;
  objc_typed_eof_func eof;
  objc_typed_flush_func flush;
} TypedStream;

/* opcode masks */
#define _B_VALUE   0x1fU
#define _B_CODE    0xe0U
#define _B_SIGN    0x10U
#define _B_NUMBER  0x0fU

/* standard opcodes */
#define _B_INVALID 0x00U
#define _B_SINT    0x20U
#define _B_NINT    0x40U
#define _B_SSTR    0x60U
#define _B_NSTR    0x80U
#define _B_RCOMM   0xa0U
#define _B_UCOMM   0xc0U
#define _B_EXT     0xe0U

/* eXtension opcodes */
#define _BX_OBJECT  0x00U
#define _BX_CLASS   0x01U
#define _BX_SEL     0x02U
#define _BX_OBJREF  0x03U
#define _BX_OBJROOT 0x04U
#define _BX_EXT     0x1fU

/*
** Read and write objects as specified by TYPE.  All the `last'
** arguments are pointers to the objects to read/write.  
*/

int objc_write_type (TypedStream* stream, const char* type, const void* data);
int objc_read_type (TypedStream* stream, const char* type, void* data);

int objc_write_types (TypedStream* stream, const char* type, ...);
int objc_read_types (TypedStream* stream, const char* type, ...);

int objc_write_object_reference (TypedStream* stream, id object);
int objc_write_root_object (TypedStream* stream, id object);

long objc_get_stream_class_version (TypedStream* stream, Class class_type);


/*
** Convenience functions
*/

int objc_write_array (TypedStream* stream, const char* type,
		      int count, const void* data);
int objc_read_array (TypedStream* stream, const char* type,
		     int count, void* data);

int objc_write_object (TypedStream* stream, id object);
int objc_read_object (TypedStream* stream, id* object);



/*
** Open a typed stream for reading or writing.  MODE may be either of
** OBJC_READONLY or OBJC_WRITEONLY.  
*/

TypedStream* objc_open_typed_stream (FILE* physical, int mode);
TypedStream* objc_open_typed_stream_for_file (const char* file_name, int mode);

void objc_close_typed_stream (TypedStream* stream);

BOOL objc_end_of_typed_stream (TypedStream* stream);
void objc_flush_typed_stream (TypedStream* stream);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* not __typedstream_INCLUDE_GNU */
