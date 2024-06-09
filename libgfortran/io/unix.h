/* Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by Janne Blomqvist

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

#ifndef GFOR_UNIX_H
#define GFOR_UNIX_H

#include "io.h"

struct stream_vtable
{
  ssize_t (* const read) (struct stream *, void *, ssize_t);
  ssize_t (* const write) (struct stream *, const void *, ssize_t);
  gfc_offset (* const seek) (struct stream *, gfc_offset, int);
  gfc_offset (* const tell) (struct stream *);
  gfc_offset (* const size) (struct stream *);
  /* Avoid keyword truncate due to AIX namespace collision.  */
  int (* const trunc) (struct stream *, gfc_offset);
  int (* const flush) (struct stream *);
  int (* const close) (struct stream *);
  int (* const markeor) (struct stream *);
};

struct stream
{
  const struct stream_vtable *vptr;
};

/* Inline functions for doing file I/O given a stream.  */
static inline ssize_t
sread (stream *s, void *buf, ssize_t nbyte)
{
  return s->vptr->read (s, buf, nbyte);
}

static inline ssize_t
swrite (stream *s, const void *buf, ssize_t nbyte)
{
  return s->vptr->write (s, buf, nbyte);
}

static inline gfc_offset
sseek (stream *s, gfc_offset offset, int whence)
{
  return s->vptr->seek (s, offset, whence);
}

static inline gfc_offset
stell (stream *s)
{
  return s->vptr->tell (s);
}

static inline gfc_offset
ssize (stream *s)
{
  return s->vptr->size (s);
}

static inline int
struncate (stream *s, gfc_offset length)
{
  return s->vptr->trunc (s, length);
}

static inline int
sflush (stream *s)
{
  return s->vptr->flush (s);
}

static inline int
sclose (stream *s)
{
  return s->vptr->close (s);
}

static inline int
smarkeor (stream *s)
{
  return s->vptr->markeor (s);
}


extern int compare_files (stream *, stream *);
internal_proto(compare_files);

extern stream *open_external (st_parameter_open *, unit_flags *);
internal_proto(open_external);

extern stream *open_internal (char *, size_t, gfc_offset);
internal_proto(open_internal);

extern stream *open_internal4 (char *, size_t, gfc_offset);
internal_proto(open_internal4);

extern char *mem_alloc_w (stream *, size_t *);
internal_proto(mem_alloc_w);

extern char *mem_alloc_r (stream *, size_t *);
internal_proto(mem_alloc_r);

extern gfc_char4_t *mem_alloc_w4 (stream *, size_t *);
internal_proto(mem_alloc_w4);

extern char *mem_alloc_r4 (stream *, size_t *);
internal_proto(mem_alloc_r4);

extern stream *input_stream (void);
internal_proto(input_stream);

extern stream *output_stream (void);
internal_proto(output_stream);

extern stream *error_stream (void);
internal_proto(error_stream);

extern int compare_file_filename (gfc_unit *, const char *, gfc_charlen_type);
internal_proto(compare_file_filename);

extern gfc_unit *find_file (const char *file, gfc_charlen_type file_len);
internal_proto(find_file);

extern int close_share (gfc_unit *);
internal_proto(close_share);

extern int file_exists (const char *file, gfc_charlen_type file_len);
internal_proto(file_exists);

extern GFC_IO_INT file_size (const char *file, gfc_charlen_type file_len);
internal_proto(file_size);

extern const char *inquire_sequential (const char *, gfc_charlen_type);
internal_proto(inquire_sequential);

extern const char *inquire_direct (const char *, gfc_charlen_type);
internal_proto(inquire_direct);

extern const char *inquire_formatted (const char *, gfc_charlen_type);
internal_proto(inquire_formatted);

extern const char *inquire_unformatted (const char *, gfc_charlen_type);
internal_proto(inquire_unformatted);

extern const char *inquire_read (const char *, gfc_charlen_type);
internal_proto(inquire_read);

extern const char *inquire_write (const char *, gfc_charlen_type);
internal_proto(inquire_write);

extern const char *inquire_readwrite (const char *, gfc_charlen_type);
internal_proto(inquire_readwrite);

extern void flush_if_preconnected (stream *);
internal_proto(flush_if_preconnected);

extern int stream_isatty (stream *);
internal_proto(stream_isatty);

#ifndef TTY_NAME_MAX
#ifdef _POSIX_TTY_NAME_MAX
#define TTY_NAME_MAX _POSIX_TTY_NAME_MAX
#else
/* sysconf(_SC_TTY_NAME_MAX) = 32 which should be enough.  */
#define TTY_NAME_MAX 32
#endif
#endif

extern int stream_ttyname (stream *, char *, size_t);
internal_proto(stream_ttyname);

#endif
