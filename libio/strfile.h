/* Copyright (C) 1993, 1997 Free Software Foundation, Inc.
   This file is part of the GNU IO Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#include <libio.h>
#ifdef TODO
Merge into  libio.h ?
#endif

typedef void *(*_IO_alloc_type) __P ((_IO_size_t));
typedef void (*_IO_free_type) __P ((void*));

struct _IO_str_fields
{
  _IO_alloc_type _allocate_buffer;
  _IO_free_type _free_buffer;
};

/* This is needed for the Irix6 N32 ABI, which has a 64 bit off_t type,
   but a 32 bit pointer type.  In this case, we get 4 bytes of padding
   after the vtable pointer.  Putting them in a structure together solves
   this problem.  */

struct _IO_streambuf
{
  struct _IO_FILE _f;
  const void *_vtable;
};

typedef struct _IO_strfile_
{
  struct _IO_streambuf _sbf;
  struct _IO_str_fields _s;
} _IO_strfile;

/* dynamic: set when the array object is allocated (or reallocated)  as
   necessary to hold a character sequence that can change in length. */
#define _IO_STR_DYNAMIC(FP) ((FP)->_s._allocate_buffer != (_IO_alloc_type)0)

/* frozen: set when the program has requested that the array object not
   be altered, reallocated, or freed. */
#define _IO_STR_FROZEN(FP) ((FP)->_f._IO_file_flags & _IO_USER_BUF)
