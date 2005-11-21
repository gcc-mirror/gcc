/* Copyright (C) 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Unix stream I/O module */

#define BUFFER_SIZE 8192

typedef struct
{
  stream st;

  int fd;
  gfc_offset buffer_offset;	/* File offset of the start of the buffer */
  gfc_offset physical_offset;	/* Current physical file offset */
  gfc_offset logical_offset;	/* Current logical file offset */
  gfc_offset dirty_offset;	/* Start of modified bytes in buffer */
  gfc_offset file_length;	/* Length of the file, -1 if not seekable. */

  char *buffer;
  int len;			/* Physical length of the current buffer */
  int active;			/* Length of valid bytes in the buffer */

  int prot;
  int ndirty;			/* Dirty bytes starting at dirty_offset */

  int special_file;		/* =1 if the fd refers to a special file */

  unsigned unbuffered:1;

  char small_buffer[BUFFER_SIZE];

}
unix_stream;

extern stream *init_error_stream (unix_stream *);
internal_proto(init_error_stream);
