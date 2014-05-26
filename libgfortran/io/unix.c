/* Copyright (C) 2002-2014 Free Software Foundation, Inc.
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

/* Unix stream I/O module */

#include "io.h"
#include "unix.h"
#include <stdlib.h>
#include <limits.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>

#include <string.h>
#include <errno.h>


/* For mingw, we don't identify files by their inode number, but by a
   64-bit identifier created from a BY_HANDLE_FILE_INFORMATION. */
#ifdef __MINGW32__

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#if !defined(_FILE_OFFSET_BITS) || _FILE_OFFSET_BITS != 64
#undef lseek
#define lseek _lseeki64
#undef fstat
#define fstat _fstati64
#undef stat
#define stat _stati64
#endif

#ifndef HAVE_WORKING_STAT
static uint64_t
id_from_handle (HANDLE hFile)
{
  BY_HANDLE_FILE_INFORMATION FileInformation;

  if (hFile == INVALID_HANDLE_VALUE)
      return 0;

  memset (&FileInformation, 0, sizeof(FileInformation));
  if (!GetFileInformationByHandle (hFile, &FileInformation))
    return 0;

  return ((uint64_t) FileInformation.nFileIndexLow)
	 | (((uint64_t) FileInformation.nFileIndexHigh) << 32);
}


static uint64_t
id_from_path (const char *path)
{
  HANDLE hFile;
  uint64_t res;

  if (!path || !*path || access (path, F_OK))
    return (uint64_t) -1;

  hFile = CreateFile (path, 0, 0, NULL, OPEN_EXISTING,
		      FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_READONLY,
		      NULL);
  res = id_from_handle (hFile);
  CloseHandle (hFile);
  return res;
}


static uint64_t
id_from_fd (const int fd)
{
  return id_from_handle ((HANDLE) _get_osfhandle (fd));
}

#endif /* HAVE_WORKING_STAT */
#endif /* __MINGW32__ */


/* min macro that evaluates its arguments only once.  */
#ifdef min
#undef min
#endif

#define min(a,b)		\
  ({ typeof (a) _a = (a);	\
    typeof (b) _b = (b);	\
    _a < _b ? _a : _b; })


/* These flags aren't defined on all targets (mingw32), so provide them
   here.  */
#ifndef S_IRGRP
#define S_IRGRP 0
#endif

#ifndef S_IWGRP
#define S_IWGRP 0
#endif

#ifndef S_IROTH
#define S_IROTH 0
#endif

#ifndef S_IWOTH
#define S_IWOTH 0
#endif


#ifndef HAVE_ACCESS

#ifndef W_OK
#define W_OK 2
#endif

#ifndef R_OK
#define R_OK 4
#endif

#ifndef F_OK
#define F_OK 0
#endif

/* Fallback implementation of access() on systems that don't have it.
   Only modes R_OK, W_OK and F_OK are used in this file.  */

static int
fallback_access (const char *path, int mode)
{
  int fd;

  if ((mode & R_OK) && (fd = open (path, O_RDONLY)) < 0)
    return -1;
  close (fd);

  if ((mode & W_OK) && (fd = open (path, O_WRONLY)) < 0)
    return -1;
  close (fd);

  if (mode == F_OK)
    {
      struct stat st;
      return stat (path, &st);
    }

  return 0;
}

#undef access
#define access fallback_access
#endif


/* Fallback directory for creating temporary files.  P_tmpdir is
   defined on many POSIX platforms.  */
#ifndef P_tmpdir
#ifdef _P_tmpdir
#define P_tmpdir _P_tmpdir  /* MinGW */
#else
#define P_tmpdir "/tmp"
#endif
#endif


/* Unix and internal stream I/O module */

static const int BUFFER_SIZE = 8192;

typedef struct
{
  stream st;

  gfc_offset buffer_offset;	/* File offset of the start of the buffer */
  gfc_offset physical_offset;	/* Current physical file offset */
  gfc_offset logical_offset;	/* Current logical file offset */
  gfc_offset file_length;	/* Length of the file. */

  char *buffer;                 /* Pointer to the buffer.  */
  int fd;                       /* The POSIX file descriptor.  */

  int active;			/* Length of valid bytes in the buffer */

  int ndirty;			/* Dirty bytes starting at buffer_offset */

  /* Cached stat(2) values.  */
  dev_t st_dev;
  ino_t st_ino;

  bool unbuffered;  /* Buffer should be flushed after each I/O statement.  */
}
unix_stream;


/* fix_fd()-- Given a file descriptor, make sure it is not one of the
 * standard descriptors, returning a non-standard descriptor.  If the
 * user specifies that system errors should go to standard output,
 * then closes standard output, we don't want the system errors to a
 * file that has been given file descriptor 1 or 0.  We want to send
 * the error to the invalid descriptor. */

static int
fix_fd (int fd)
{
#ifdef HAVE_DUP
  int input, output, error;

  input = output = error = 0;

  /* Unix allocates the lowest descriptors first, so a loop is not
     required, but this order is. */
  if (fd == STDIN_FILENO)
    {
      fd = dup (fd);
      input = 1;
    }
  if (fd == STDOUT_FILENO)
    {
      fd = dup (fd);
      output = 1;
    }
  if (fd == STDERR_FILENO)
    {
      fd = dup (fd);
      error = 1;
    }

  if (input)
    close (STDIN_FILENO);
  if (output)
    close (STDOUT_FILENO);
  if (error)
    close (STDERR_FILENO);
#endif

  return fd;
}


/* If the stream corresponds to a preconnected unit, we flush the
   corresponding C stream.  This is bugware for mixed C-Fortran codes
   where the C code doesn't flush I/O before returning.  */
void
flush_if_preconnected (stream * s)
{
  int fd;

  fd = ((unix_stream *) s)->fd;
  if (fd == STDIN_FILENO)
    fflush (stdin);
  else if (fd == STDOUT_FILENO)
    fflush (stdout);
  else if (fd == STDERR_FILENO)
    fflush (stderr);
}


/********************************************************************
Raw I/O functions (read, write, seek, tell, truncate, close).

These functions wrap the basic POSIX I/O syscalls. Any deviation in
semantics is a bug, except the following: write restarts in case
of being interrupted by a signal, and as the first argument the
functions take the unix_stream struct rather than an integer file
descriptor. Also, for POSIX read() and write() a nbyte argument larger
than SSIZE_MAX is undefined; here the type of nbyte is ssize_t rather
than size_t as for POSIX read/write.
*********************************************************************/

static int
raw_flush (unix_stream * s  __attribute__ ((unused)))
{
  return 0;
}

static ssize_t
raw_read (unix_stream * s, void * buf, ssize_t nbyte)
{
  /* For read we can't do I/O in a loop like raw_write does, because
     that will break applications that wait for interactive I/O.  */
  return read (s->fd, buf, nbyte);
}

static ssize_t
raw_write (unix_stream * s, const void * buf, ssize_t nbyte)
{
  ssize_t trans, bytes_left;
  char *buf_st;

  bytes_left = nbyte;
  buf_st = (char *) buf;

  /* We must write in a loop since some systems don't restart system
     calls in case of a signal.  */
  while (bytes_left > 0)
    {
      trans = write (s->fd, buf_st, bytes_left);
      if (trans < 0)
	{
	  if (errno == EINTR)
	    continue;
	  else
	    return trans;
	}
      buf_st += trans;
      bytes_left -= trans;
    }

  return nbyte - bytes_left;
}

static gfc_offset
raw_seek (unix_stream * s, gfc_offset offset, int whence)
{
  return lseek (s->fd, offset, whence);
}

static gfc_offset
raw_tell (unix_stream * s)
{
  return lseek (s->fd, 0, SEEK_CUR);
}

static gfc_offset
raw_size (unix_stream * s)
{
  struct stat statbuf;
  int ret = fstat (s->fd, &statbuf);
  if (ret == -1)
    return ret;
  if (S_ISREG (statbuf.st_mode))
    return statbuf.st_size;
  else
    return 0;
}

static int
raw_truncate (unix_stream * s, gfc_offset length)
{
#ifdef __MINGW32__
  HANDLE h;
  gfc_offset cur;

  if (isatty (s->fd))
    {
      errno = EBADF;
      return -1;
    }
  h = (HANDLE) _get_osfhandle (s->fd);
  if (h == INVALID_HANDLE_VALUE)
    {
      errno = EBADF;
      return -1;
    }
  cur = lseek (s->fd, 0, SEEK_CUR);
  if (cur == -1)
    return -1;
  if (lseek (s->fd, length, SEEK_SET) == -1)
    goto error;
  if (!SetEndOfFile (h))
    {
      errno = EBADF;
      goto error;
    }
  if (lseek (s->fd, cur, SEEK_SET) == -1)
    return -1;
  return 0;
 error:
  lseek (s->fd, cur, SEEK_SET);
  return -1;
#elif defined HAVE_FTRUNCATE
  return ftruncate (s->fd, length);
#elif defined HAVE_CHSIZE
  return chsize (s->fd, length);
#else
  runtime_error ("required ftruncate or chsize support not present");
  return -1;
#endif
}

static int
raw_close (unix_stream * s)
{
  int retval;
  
  if (s->fd == -1)
    retval = -1;
  else if (s->fd != STDOUT_FILENO
      && s->fd != STDERR_FILENO
      && s->fd != STDIN_FILENO)
    retval = close (s->fd);
  else
    retval = 0;
  free (s);
  return retval;
}

static const struct stream_vtable raw_vtable = {
  .read = (void *) raw_read,
  .write = (void *) raw_write,
  .seek = (void *) raw_seek,
  .tell = (void *) raw_tell,
  .size = (void *) raw_size,
  .trunc = (void *) raw_truncate,
  .close = (void *) raw_close,
  .flush = (void *) raw_flush 
};

static int
raw_init (unix_stream * s)
{
  s->st.vptr = &raw_vtable;

  s->buffer = NULL;
  return 0;
}


/*********************************************************************
Buffered I/O functions. These functions have the same semantics as the
raw I/O functions above, except that they are buffered in order to
improve performance. The buffer must be flushed when switching from
reading to writing and vice versa.
*********************************************************************/

static int
buf_flush (unix_stream * s)
{
  int writelen;

  /* Flushing in read mode means discarding read bytes.  */
  s->active = 0;

  if (s->ndirty == 0)
    return 0;
  
  if (s->physical_offset != s->buffer_offset
      && lseek (s->fd, s->buffer_offset, SEEK_SET) < 0)
    return -1;

  writelen = raw_write (s, s->buffer, s->ndirty);

  s->physical_offset = s->buffer_offset + writelen;

  if (s->physical_offset > s->file_length)
      s->file_length = s->physical_offset;

  s->ndirty -= writelen;
  if (s->ndirty != 0)
    return -1;

  return 0;
}

static ssize_t
buf_read (unix_stream * s, void * buf, ssize_t nbyte)
{
  if (s->active == 0)
    s->buffer_offset = s->logical_offset;

  /* Is the data we want in the buffer?  */
  if (s->logical_offset + nbyte <= s->buffer_offset + s->active
      && s->buffer_offset <= s->logical_offset)
    memcpy (buf, s->buffer + (s->logical_offset - s->buffer_offset), nbyte);
  else
    {
      /* First copy the active bytes if applicable, then read the rest
         either directly or filling the buffer.  */
      char *p;
      int nread = 0;
      ssize_t to_read, did_read;
      gfc_offset new_logical;
      
      p = (char *) buf;
      if (s->logical_offset >= s->buffer_offset 
          && s->buffer_offset + s->active >= s->logical_offset)
        {
          nread = s->active - (s->logical_offset - s->buffer_offset);
          memcpy (buf, s->buffer + (s->logical_offset - s->buffer_offset), 
                  nread);
          p += nread;
        }
      /* At this point we consider all bytes in the buffer discarded.  */
      to_read = nbyte - nread;
      new_logical = s->logical_offset + nread;
      if (s->physical_offset != new_logical
          && lseek (s->fd, new_logical, SEEK_SET) < 0)
        return -1;
      s->buffer_offset = s->physical_offset = new_logical;
      if (to_read <= BUFFER_SIZE/2)
        {
          did_read = raw_read (s, s->buffer, BUFFER_SIZE);
          s->physical_offset += did_read;
          s->active = did_read;
          did_read = (did_read > to_read) ? to_read : did_read;
          memcpy (p, s->buffer, did_read);
        }
      else
        {
          did_read = raw_read (s, p, to_read);
          s->physical_offset += did_read;
          s->active = 0;
        }
      nbyte = did_read + nread;
    }
  s->logical_offset += nbyte;
  return nbyte;
}

static ssize_t
buf_write (unix_stream * s, const void * buf, ssize_t nbyte)
{
  if (s->ndirty == 0)
    s->buffer_offset = s->logical_offset;

  /* Does the data fit into the buffer?  As a special case, if the
     buffer is empty and the request is bigger than BUFFER_SIZE/2,
     write directly. This avoids the case where the buffer would have
     to be flushed at every write.  */
  if (!(s->ndirty == 0 && nbyte > BUFFER_SIZE/2)
      && s->logical_offset + nbyte <= s->buffer_offset + BUFFER_SIZE
      && s->buffer_offset <= s->logical_offset
      && s->buffer_offset + s->ndirty >= s->logical_offset)
    {
      memcpy (s->buffer + (s->logical_offset - s->buffer_offset), buf, nbyte);
      int nd = (s->logical_offset - s->buffer_offset) + nbyte;
      if (nd > s->ndirty)
        s->ndirty = nd;
    }
  else
    {
      /* Flush, and either fill the buffer with the new data, or if
         the request is bigger than the buffer size, write directly
         bypassing the buffer.  */
      buf_flush (s);
      if (nbyte <= BUFFER_SIZE/2)
        {
          memcpy (s->buffer, buf, nbyte);
          s->buffer_offset = s->logical_offset;
          s->ndirty += nbyte;
        }
      else
	{
	  if (s->physical_offset != s->logical_offset)
	    {
	      if (lseek (s->fd, s->logical_offset, SEEK_SET) < 0)
		return -1;
	      s->physical_offset = s->logical_offset;
	    }

	  nbyte = raw_write (s, buf, nbyte);
	  s->physical_offset += nbyte;
	}
    }
  s->logical_offset += nbyte;
  if (s->logical_offset > s->file_length)
    s->file_length = s->logical_offset;
  return nbyte;
}

static gfc_offset
buf_seek (unix_stream * s, gfc_offset offset, int whence)
{
  switch (whence)
    {
    case SEEK_SET:
      break;
    case SEEK_CUR:
      offset += s->logical_offset;
      break;
    case SEEK_END:
      offset += s->file_length;
      break;
    default:
      return -1;
    }
  if (offset < 0)
    {
      errno = EINVAL;
      return -1;
    }
  s->logical_offset = offset;
  return offset;
}

static gfc_offset
buf_tell (unix_stream * s)
{
  return buf_seek (s, 0, SEEK_CUR);
}

static gfc_offset
buf_size (unix_stream * s)
{
  return s->file_length;
}

static int
buf_truncate (unix_stream * s, gfc_offset length)
{
  int r;

  if (buf_flush (s) != 0)
    return -1;
  r = raw_truncate (s, length);
  if (r == 0)
    s->file_length = length;
  return r;
}

static int
buf_close (unix_stream * s)
{
  if (buf_flush (s) != 0)
    return -1;
  free (s->buffer);
  return raw_close (s);
}

static const struct stream_vtable buf_vtable = {
  .read = (void *) buf_read,
  .write = (void *) buf_write,
  .seek = (void *) buf_seek,
  .tell = (void *) buf_tell,
  .size = (void *) buf_size,
  .trunc = (void *) buf_truncate,
  .close = (void *) buf_close,
  .flush = (void *) buf_flush 
};

static int
buf_init (unix_stream * s)
{
  s->st.vptr = &buf_vtable;

  s->buffer = xmalloc (BUFFER_SIZE);
  return 0;
}


/*********************************************************************
  memory stream functions - These are used for internal files

  The idea here is that a single stream structure is created and all
  requests must be satisfied from it.  The location and size of the
  buffer is the character variable supplied to the READ or WRITE
  statement.

*********************************************************************/

char *
mem_alloc_r (stream * strm, int * len)
{
  unix_stream * s = (unix_stream *) strm;
  gfc_offset n;
  gfc_offset where = s->logical_offset;

  if (where < s->buffer_offset || where > s->buffer_offset + s->active)
    return NULL;

  n = s->buffer_offset + s->active - where;
  if (*len > n)
    *len = n;

  s->logical_offset = where + *len;

  return s->buffer + (where - s->buffer_offset);
}


char *
mem_alloc_r4 (stream * strm, int * len)
{
  unix_stream * s = (unix_stream *) strm;
  gfc_offset n;
  gfc_offset where = s->logical_offset;

  if (where < s->buffer_offset || where > s->buffer_offset + s->active)
    return NULL;

  n = s->buffer_offset + s->active - where;
  if (*len > n)
    *len = n;

  s->logical_offset = where + *len;

  return s->buffer + (where - s->buffer_offset) * 4;
}


char *
mem_alloc_w (stream * strm, int * len)
{
  unix_stream * s = (unix_stream *) strm;
  gfc_offset m;
  gfc_offset where = s->logical_offset;

  m = where + *len;

  if (where < s->buffer_offset)
    return NULL;

  if (m > s->file_length)
    return NULL;

  s->logical_offset = m;

  return s->buffer + (where - s->buffer_offset);
}


gfc_char4_t *
mem_alloc_w4 (stream * strm, int * len)
{
  unix_stream * s = (unix_stream *) strm;
  gfc_offset m;
  gfc_offset where = s->logical_offset;
  gfc_char4_t *result = (gfc_char4_t *) s->buffer;

  m = where + *len;

  if (where < s->buffer_offset)
    return NULL;

  if (m > s->file_length)
    return NULL;

  s->logical_offset = m;
  return &result[where - s->buffer_offset];
}


/* Stream read function for character(kind=1) internal units.  */

static ssize_t
mem_read (stream * s, void * buf, ssize_t nbytes)
{
  void *p;
  int nb = nbytes;

  p = mem_alloc_r (s, &nb);
  if (p)
    {
      memcpy (buf, p, nb);
      return (ssize_t) nb;
    }
  else
    return 0;
}


/* Stream read function for chracter(kind=4) internal units.  */

static ssize_t
mem_read4 (stream * s, void * buf, ssize_t nbytes)
{
  void *p;
  int nb = nbytes;

  p = mem_alloc_r (s, &nb);
  if (p)
    {
      memcpy (buf, p, nb);
      return (ssize_t) nb;
    }
  else
    return 0;
}


/* Stream write function for character(kind=1) internal units.  */

static ssize_t
mem_write (stream * s, const void * buf, ssize_t nbytes)
{
  void *p;
  int nb = nbytes;

  p = mem_alloc_w (s, &nb);
  if (p)
    {
      memcpy (p, buf, nb);
      return (ssize_t) nb;
    }
  else
    return 0;
}


/* Stream write function for character(kind=4) internal units.  */

static ssize_t
mem_write4 (stream * s, const void * buf, ssize_t nwords)
{
  gfc_char4_t *p;
  int nw = nwords;

  p = mem_alloc_w4 (s, &nw);
  if (p)
    {
      while (nw--)
	*p++ = (gfc_char4_t) *((char *) buf);
      return nwords;
    }
  else
    return 0;
}


static gfc_offset
mem_seek (stream * strm, gfc_offset offset, int whence)
{
  unix_stream * s = (unix_stream *) strm;
  switch (whence)
    {
    case SEEK_SET:
      break;
    case SEEK_CUR:
      offset += s->logical_offset;
      break;
    case SEEK_END:
      offset += s->file_length;
      break;
    default:
      return -1;
    }

  /* Note that for internal array I/O it's actually possible to have a
     negative offset, so don't check for that.  */
  if (offset > s->file_length)
    {
      errno = EINVAL;
      return -1;
    }

  s->logical_offset = offset;

  /* Returning < 0 is the error indicator for sseek(), so return 0 if
     offset is negative.  Thus if the return value is 0, the caller
     has to use stell() to get the real value of logical_offset.  */
  if (offset >= 0)
    return offset;
  return 0;
}


static gfc_offset
mem_tell (stream * s)
{
  return ((unix_stream *)s)->logical_offset;
}


static int
mem_truncate (unix_stream * s __attribute__ ((unused)), 
	      gfc_offset length __attribute__ ((unused)))
{
  return 0;
}


static int
mem_flush (unix_stream * s __attribute__ ((unused)))
{
  return 0;
}


static int
mem_close (unix_stream * s)
{
  free (s);

  return 0;
}

static const struct stream_vtable mem_vtable = {
  .read = (void *) mem_read,
  .write = (void *) mem_write,
  .seek = (void *) mem_seek,
  .tell = (void *) mem_tell,
  /* buf_size is not a typo, we just reuse an identical
     implementation.  */
  .size = (void *) buf_size,
  .trunc = (void *) mem_truncate,
  .close = (void *) mem_close,
  .flush = (void *) mem_flush 
};

static const struct stream_vtable mem4_vtable = {
  .read = (void *) mem_read4,
  .write = (void *) mem_write4,
  .seek = (void *) mem_seek,
  .tell = (void *) mem_tell,
  /* buf_size is not a typo, we just reuse an identical
     implementation.  */
  .size = (void *) buf_size,
  .trunc = (void *) mem_truncate,
  .close = (void *) mem_close,
  .flush = (void *) mem_flush 
};

/*********************************************************************
  Public functions -- A reimplementation of this module needs to
  define functional equivalents of the following.
*********************************************************************/

/* open_internal()-- Returns a stream structure from a character(kind=1)
   internal file */

stream *
open_internal (char *base, int length, gfc_offset offset)
{
  unix_stream *s;

  s = xcalloc (1, sizeof (unix_stream));

  s->buffer = base;
  s->buffer_offset = offset;

  s->active = s->file_length = length;

  s->st.vptr = &mem_vtable;

  return (stream *) s;
}

/* open_internal4()-- Returns a stream structure from a character(kind=4)
   internal file */

stream *
open_internal4 (char *base, int length, gfc_offset offset)
{
  unix_stream *s;

  s = xcalloc (1, sizeof (unix_stream));

  s->buffer = base;
  s->buffer_offset = offset;

  s->active = s->file_length = length * sizeof (gfc_char4_t);

  s->st.vptr = &mem4_vtable;

  return (stream *) s;
}


/* "Unbuffered" really means I/O statement buffering. For formatted
   I/O, the fbuf manages this, and then uses raw I/O. For unformatted
   I/O, buffered I/O is used, and the buffer is flushed at the end of
   each I/O statement, where this function is called.  */

int
flush_if_unbuffered (stream* s)
{
  unix_stream* us = (unix_stream*) s;
  if (us->unbuffered)
    return sflush (s);
  return 0;
}


/* fd_to_stream()-- Given an open file descriptor, build a stream
 * around it. */

static stream *
fd_to_stream (int fd, bool unformatted)
{
  struct stat statbuf;
  unix_stream *s;

  s = xcalloc (1, sizeof (unix_stream));

  s->fd = fd;

  /* Get the current length of the file. */

  if (fstat (fd, &statbuf) == -1)
    {
      s->st_dev = s->st_ino = -1;
      s->file_length = 0;
      if (errno == EBADF)
	s->fd = -1;
      raw_init (s);
      return (stream *) s;
    }

  s->st_dev = statbuf.st_dev;
  s->st_ino = statbuf.st_ino;
  s->file_length = statbuf.st_size;

  /* Only use buffered IO for regular files.  */
  if (S_ISREG (statbuf.st_mode)
      && !options.all_unbuffered
      && !(options.unbuffered_preconnected && 
	   (s->fd == STDIN_FILENO 
	    || s->fd == STDOUT_FILENO 
	    || s->fd == STDERR_FILENO)))
    buf_init (s);
  else
    {
      if (unformatted)
	{
	  s->unbuffered = true;
	  buf_init (s);
	}
      else
	raw_init (s);
    }

  return (stream *) s;
}


/* Given the Fortran unit number, convert it to a C file descriptor.  */

int
unit_to_fd (int unit)
{
  gfc_unit *us;
  int fd;

  us = find_unit (unit);
  if (us == NULL)
    return -1;

  fd = ((unix_stream *) us->s)->fd;
  unlock_unit (us);
  return fd;
}


/* Set the close-on-exec flag for an existing fd, if the system
   supports such.  */

static void __attribute__ ((unused))
set_close_on_exec (int fd __attribute__ ((unused)))
{
  /* Mingw does not define F_SETFD.  */
#if defined(HAVE_FCNTL) && defined(F_SETFD) && defined(FD_CLOEXEC)
  if (fd >= 0)
    fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif
}


/* Helper function for tempfile(). Tries to open a temporary file in
   the directory specified by tempdir. If successful, the file name is
   stored in fname and the descriptor returned. Returns -1 on
   failure.  */

static int
tempfile_open (const char *tempdir, char **fname)
{
  int fd;
  const char *slash = "/";
#if defined(HAVE_UMASK) && defined(HAVE_MKSTEMP)
  mode_t mode_mask;
#endif

  if (!tempdir)
    return -1;

  /* Check for the special case that tempdir ends with a slash or
     backslash.  */
  size_t tempdirlen = strlen (tempdir);
  if (*tempdir == 0 || tempdir[tempdirlen - 1] == '/'
#ifdef __MINGW32__
      || tempdir[tempdirlen - 1] == '\\'
#endif
     )
    slash = "";

  // Take care that the template is longer in the mktemp() branch.
  char * template = xmalloc (tempdirlen + 23);

#ifdef HAVE_MKSTEMP
  snprintf (template, tempdirlen + 23, "%s%sgfortrantmpXXXXXX", 
	    tempdir, slash);

#ifdef HAVE_UMASK
  /* Temporarily set the umask such that the file has 0600 permissions.  */
  mode_mask = umask (S_IXUSR | S_IRWXG | S_IRWXO);
#endif

#if defined(HAVE_MKOSTEMP) && defined(O_CLOEXEC)
  fd = mkostemp (template, O_CLOEXEC);
#else
  fd = mkstemp (template);
  set_close_on_exec (fd);
#endif

#ifdef HAVE_UMASK
  (void) umask (mode_mask);
#endif

#else /* HAVE_MKSTEMP */
  fd = -1;
  int count = 0;
  size_t slashlen = strlen (slash);
  int flags = O_RDWR | O_CREAT | O_EXCL;
#if defined(HAVE_CRLF) && defined(O_BINARY)
  flags |= O_BINARY;
#endif
#ifdef O_CLOEXEC
  flags |= O_CLOEXEC;
#endif
  do
    {
      snprintf (template, tempdirlen + 23, "%s%sgfortrantmpaaaXXXXXX", 
		tempdir, slash);
      if (count > 0)
	{
	  int c = count;
	  template[tempdirlen + slashlen + 13] = 'a' + (c% 26);
	  c /= 26;
	  template[tempdirlen + slashlen + 12] = 'a' + (c % 26);
	  c /= 26;
	  template[tempdirlen + slashlen + 11] = 'a' + (c % 26);
	  if (c >= 26)
	    break;
	}

      if (!mktemp (template))
      {
	errno = EEXIST;
	count++;
	continue;
      }

      fd = open (template, flags, S_IRUSR | S_IWUSR);
    }
  while (fd == -1 && errno == EEXIST);
#ifndef O_CLOEXEC
  set_close_on_exec (fd);
#endif
#endif /* HAVE_MKSTEMP */

  *fname = template;
  return fd;
}


/* tempfile()-- Generate a temporary filename for a scratch file and
 * open it.  mkstemp() opens the file for reading and writing, but the
 * library mode prevents anything that is not allowed.  The descriptor
 * is returned, which is -1 on error.  The template is pointed to by 
 * opp->file, which is copied into the unit structure
 * and freed later. */

static int
tempfile (st_parameter_open *opp)
{
  const char *tempdir;
  char *fname;
  int fd = -1;

  tempdir = secure_getenv ("TMPDIR");
  fd = tempfile_open (tempdir, &fname);
#ifdef __MINGW32__
  if (fd == -1)
    {
      char buffer[MAX_PATH + 1];
      DWORD ret;
      ret = GetTempPath (MAX_PATH, buffer);
      /* If we are not able to get a temp-directory, we use
	 current directory.  */
      if (ret > MAX_PATH || !ret)
        buffer[0] = 0;
      else
        buffer[ret] = 0;
      tempdir = strdup (buffer);
      fd = tempfile_open (tempdir, &fname);
    }
#elif defined(__CYGWIN__)
  if (fd == -1)
    {
      tempdir = secure_getenv ("TMP");
      fd = tempfile_open (tempdir, &fname);
    }
  if (fd == -1)
    {
      tempdir = secure_getenv ("TEMP");
      fd = tempfile_open (tempdir, &fname);
    }
#endif
  if (fd == -1)
    fd = tempfile_open (P_tmpdir, &fname);
 
  opp->file = fname;
  opp->file_len = strlen (fname);	/* Don't include trailing nul */

  return fd;
}


/* regular_file2()-- Open a regular file.
 * Change flags->action if it is ACTION_UNSPECIFIED on entry,
 * unless an error occurs.
 * Returns the descriptor, which is less than zero on error. */

static int
regular_file2 (const char *path, st_parameter_open *opp, unit_flags *flags)
{
  int mode;
  int rwflag;
  int crflag, crflag2;
  int fd;

#ifdef __CYGWIN__
  if (opp->file_len == 7)
    {
      if (strncmp (path, "CONOUT$", 7) == 0
	  || strncmp (path, "CONERR$", 7) == 0)
	{
	  fd = open ("/dev/conout", O_WRONLY);
	  flags->action = ACTION_WRITE;
	  return fd;
	}
    }

  if (opp->file_len == 6 && strncmp (path, "CONIN$", 6) == 0)
    {
      fd = open ("/dev/conin", O_RDONLY);
      flags->action = ACTION_READ;
      return fd;
    }
#endif


#ifdef __MINGW32__
  if (opp->file_len == 7)
    {
      if (strncmp (path, "CONOUT$", 7) == 0
	  || strncmp (path, "CONERR$", 7) == 0)
	{
	  fd = open ("CONOUT$", O_WRONLY);
	  flags->action = ACTION_WRITE;
	  return fd;
	}
    }

  if (opp->file_len == 6 && strncmp (path, "CONIN$", 6) == 0)
    {
      fd = open ("CONIN$", O_RDONLY);
      flags->action = ACTION_READ;
      return fd;
    }
#endif

  switch (flags->action)
    {
    case ACTION_READ:
      rwflag = O_RDONLY;
      break;

    case ACTION_WRITE:
      rwflag = O_WRONLY;
      break;

    case ACTION_READWRITE:
    case ACTION_UNSPECIFIED:
      rwflag = O_RDWR;
      break;

    default:
      internal_error (&opp->common, "regular_file(): Bad action");
    }

  switch (flags->status)
    {
    case STATUS_NEW:
      crflag = O_CREAT | O_EXCL;
      break;

    case STATUS_OLD:		/* open will fail if the file does not exist*/
      crflag = 0;
      break;

    case STATUS_UNKNOWN:
      if (rwflag == O_RDONLY)
	crflag = 0;
      else
	crflag = O_CREAT;
      break;

    case STATUS_REPLACE:
      crflag = O_CREAT | O_TRUNC;
      break;

    default:
      /* Note: STATUS_SCRATCH is handled by tempfile () and should
	 never be seen here.  */
      internal_error (&opp->common, "regular_file(): Bad status");
    }

  /* rwflag |= O_LARGEFILE; */

#if defined(HAVE_CRLF) && defined(O_BINARY)
  crflag |= O_BINARY;
#endif

#ifdef O_CLOEXEC
  crflag |= O_CLOEXEC;
#endif

  mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  fd = open (path, rwflag | crflag, mode);
  if (flags->action != ACTION_UNSPECIFIED)
    return fd;

  if (fd >= 0)
    {
      flags->action = ACTION_READWRITE;
      return fd;
    }
  if (errno != EACCES && errno != EROFS)
     return fd;

  /* retry for read-only access */
  rwflag = O_RDONLY;
  if (flags->status == STATUS_UNKNOWN)
    crflag2 = crflag & ~(O_CREAT);
  else
    crflag2 = crflag;
  fd = open (path, rwflag | crflag2, mode);
  if (fd >=0)
    {
      flags->action = ACTION_READ;
      return fd;		/* success */
    }
  
  if (errno != EACCES && errno != ENOENT)
    return fd;			/* failure */

  /* retry for write-only access */
  rwflag = O_WRONLY;
  fd = open (path, rwflag | crflag, mode);
  if (fd >=0)
    {
      flags->action = ACTION_WRITE;
      return fd;		/* success */
    }
  return fd;			/* failure */
}


/* Wrapper around regular_file2, to make sure we free the path after
   we're done.  */

static int
regular_file (st_parameter_open *opp, unit_flags *flags)
{
  char *path = fc_strdup (opp->file, opp->file_len);
  int fd = regular_file2 (path, opp, flags);
  free (path);
  return fd;
}

/* open_external()-- Open an external file, unix specific version.
 * Change flags->action if it is ACTION_UNSPECIFIED on entry.
 * Returns NULL on operating system error. */

stream *
open_external (st_parameter_open *opp, unit_flags *flags)
{
  int fd;

  if (flags->status == STATUS_SCRATCH)
    {
      fd = tempfile (opp);
      if (flags->action == ACTION_UNSPECIFIED)
	flags->action = ACTION_READWRITE;

#if HAVE_UNLINK_OPEN_FILE
      /* We can unlink scratch files now and it will go away when closed. */
      if (fd >= 0)
	unlink (opp->file);
#endif
    }
  else
    {
      /* regular_file resets flags->action if it is ACTION_UNSPECIFIED and
       * if it succeeds */
      fd = regular_file (opp, flags);
#ifndef O_CLOEXEC
      set_close_on_exec (fd);
#endif
    }

  if (fd < 0)
    return NULL;
  fd = fix_fd (fd);

  return fd_to_stream (fd, flags->form == FORM_UNFORMATTED);
}


/* input_stream()-- Return a stream pointer to the default input stream.
 * Called on initialization. */

stream *
input_stream (void)
{
  return fd_to_stream (STDIN_FILENO, false);
}


/* output_stream()-- Return a stream pointer to the default output stream.
 * Called on initialization. */

stream *
output_stream (void)
{
  stream * s;

#if defined(HAVE_CRLF) && defined(HAVE_SETMODE)
  setmode (STDOUT_FILENO, O_BINARY);
#endif

  s = fd_to_stream (STDOUT_FILENO, false);
  return s;
}


/* error_stream()-- Return a stream pointer to the default error stream.
 * Called on initialization. */

stream *
error_stream (void)
{
  stream * s;

#if defined(HAVE_CRLF) && defined(HAVE_SETMODE)
  setmode (STDERR_FILENO, O_BINARY);
#endif

  s = fd_to_stream (STDERR_FILENO, false);
  return s;
}


/* compare_file_filename()-- Given an open stream and a fortran string
 * that is a filename, figure out if the file is the same as the
 * filename. */

int
compare_file_filename (gfc_unit *u, const char *name, int len)
{
  struct stat st;
  int ret;
#ifdef HAVE_WORKING_STAT
  unix_stream *s;
#else
# ifdef __MINGW32__
  uint64_t id1, id2;
# endif
#endif

  char *path = fc_strdup (name, len);

  /* If the filename doesn't exist, then there is no match with the
   * existing file. */

  if (stat (path, &st) < 0)
    {
      ret = 0;
      goto done;
    }

#ifdef HAVE_WORKING_STAT
  s = (unix_stream *) (u->s);
  ret = (st.st_dev == s->st_dev) && (st.st_ino == s->st_ino);
  goto done;
#else

# ifdef __MINGW32__
  /* We try to match files by a unique ID.  On some filesystems (network
     fs and FAT), we can't generate this unique ID, and will simply compare
     filenames.  */
  id1 = id_from_path (path);
  id2 = id_from_fd (((unix_stream *) (u->s))->fd);
  if (id1 || id2)
    {
      ret = (id1 == id2);
      goto done;
    }
# endif

  if (len != u->file_len)
    ret = 0;
  else
    ret = (memcmp(path, u->file, len) == 0);
#endif
 done:
  free (path);
  return ret;
}


#ifdef HAVE_WORKING_STAT
# define FIND_FILE0_DECL struct stat *st
# define FIND_FILE0_ARGS st
#else
# define FIND_FILE0_DECL uint64_t id, const char *file, gfc_charlen_type file_len
# define FIND_FILE0_ARGS id, file, file_len
#endif

/* find_file0()-- Recursive work function for find_file() */

static gfc_unit *
find_file0 (gfc_unit *u, FIND_FILE0_DECL)
{
  gfc_unit *v;
#if defined(__MINGW32__) && !HAVE_WORKING_STAT
  uint64_t id1;
#endif

  if (u == NULL)
    return NULL;

#ifdef HAVE_WORKING_STAT
  if (u->s != NULL)
    {
      unix_stream *s = (unix_stream *) (u->s);
      if (st[0].st_dev == s->st_dev && st[0].st_ino == s->st_ino)
	return u;
    }
#else
# ifdef __MINGW32__ 
  if (u->s && ((id1 = id_from_fd (((unix_stream *) u->s)->fd)) || id1))
    {
      if (id == id1)
	return u;
    }
  else
# endif
    if (compare_string (u->file_len, u->file, file_len, file) == 0)
      return u;
#endif

  v = find_file0 (u->left, FIND_FILE0_ARGS);
  if (v != NULL)
    return v;

  v = find_file0 (u->right, FIND_FILE0_ARGS);
  if (v != NULL)
    return v;

  return NULL;
}


/* find_file()-- Take the current filename and see if there is a unit
 * that has the file already open.  Returns a pointer to the unit if so. */

gfc_unit *
find_file (const char *file, gfc_charlen_type file_len)
{
  struct stat st[1];
  gfc_unit *u;
#if defined(__MINGW32__) && !HAVE_WORKING_STAT
  uint64_t id = 0ULL;
#endif

  char *path = fc_strdup (file, file_len);

  if (stat (path, &st[0]) < 0)
    {
      u = NULL;
      goto done;
    }

#if defined(__MINGW32__) && !HAVE_WORKING_STAT
  id = id_from_path (path);
#endif

  __gthread_mutex_lock (&unit_lock);
retry:
  u = find_file0 (unit_root, FIND_FILE0_ARGS);
  if (u != NULL)
    {
      /* Fast path.  */
      if (! __gthread_mutex_trylock (&u->lock))
	{
	  /* assert (u->closed == 0); */
	  __gthread_mutex_unlock (&unit_lock);
	  goto done;
	}

      inc_waiting_locked (u);
    }
  __gthread_mutex_unlock (&unit_lock);
  if (u != NULL)
    {
      __gthread_mutex_lock (&u->lock);
      if (u->closed)
	{
	  __gthread_mutex_lock (&unit_lock);
	  __gthread_mutex_unlock (&u->lock);
	  if (predec_waiting_locked (u) == 0)
	    free (u);
	  goto retry;
	}

      dec_waiting_unlocked (u);
    }
 done:
  free (path);
  return u;
}

static gfc_unit *
flush_all_units_1 (gfc_unit *u, int min_unit)
{
  while (u != NULL)
    {
      if (u->unit_number > min_unit)
	{
	  gfc_unit *r = flush_all_units_1 (u->left, min_unit);
	  if (r != NULL)
	    return r;
	}
      if (u->unit_number >= min_unit)
	{
	  if (__gthread_mutex_trylock (&u->lock))
	    return u;
	  if (u->s)
	    sflush (u->s);
	  __gthread_mutex_unlock (&u->lock);
	}
      u = u->right;
    }
  return NULL;
}

void
flush_all_units (void)
{
  gfc_unit *u;
  int min_unit = 0;

  __gthread_mutex_lock (&unit_lock);
  do
    {
      u = flush_all_units_1 (unit_root, min_unit);
      if (u != NULL)
	inc_waiting_locked (u);
      __gthread_mutex_unlock (&unit_lock);
      if (u == NULL)
	return;

      __gthread_mutex_lock (&u->lock);

      min_unit = u->unit_number + 1;

      if (u->closed == 0)
	{
	  sflush (u->s);
	  __gthread_mutex_lock (&unit_lock);
	  __gthread_mutex_unlock (&u->lock);
	  (void) predec_waiting_locked (u);
	}
      else
	{
	  __gthread_mutex_lock (&unit_lock);
	  __gthread_mutex_unlock (&u->lock);
	  if (predec_waiting_locked (u) == 0)
	    free (u);
	}
    }
  while (1);
}


/* delete_file()-- Given a unit structure, delete the file associated
 * with the unit.  Returns nonzero if something went wrong. */

int
delete_file (gfc_unit * u)
{
  char *path = fc_strdup (u->file, u->file_len);
  int err = unlink (path);
  free (path);
  return err;
}


/* file_exists()-- Returns nonzero if the current filename exists on
 * the system */

int
file_exists (const char *file, gfc_charlen_type file_len)
{
  char *path = fc_strdup (file, file_len);
  int res = !(access (path, F_OK));
  free (path);
  return res;
}


/* file_size()-- Returns the size of the file.  */

GFC_IO_INT
file_size (const char *file, gfc_charlen_type file_len)
{
  char *path = fc_strdup (file, file_len);
  struct stat statbuf;
  int err = stat (path, &statbuf);
  free (path);
  if (err == -1)
    return -1;
  return (GFC_IO_INT) statbuf.st_size;
}

static const char yes[] = "YES", no[] = "NO", unknown[] = "UNKNOWN";

/* inquire_sequential()-- Given a fortran string, determine if the
 * file is suitable for sequential access.  Returns a C-style
 * string. */

const char *
inquire_sequential (const char *string, int len)
{
  struct stat statbuf;

  if (string == NULL)
    return unknown;

  char *path = fc_strdup (string, len);
  int err = stat (path, &statbuf);
  free (path);
  if (err == -1)
    return unknown;

  if (S_ISREG (statbuf.st_mode) ||
      S_ISCHR (statbuf.st_mode) || S_ISFIFO (statbuf.st_mode))
    return unknown;

  if (S_ISDIR (statbuf.st_mode) || S_ISBLK (statbuf.st_mode))
    return no;

  return unknown;
}


/* inquire_direct()-- Given a fortran string, determine if the file is
 * suitable for direct access.  Returns a C-style string. */

const char *
inquire_direct (const char *string, int len)
{
  struct stat statbuf;

  if (string == NULL)
    return unknown;

  char *path = fc_strdup (string, len);
  int err = stat (path, &statbuf);
  free (path);
  if (err == -1)
    return unknown;

  if (S_ISREG (statbuf.st_mode) || S_ISBLK (statbuf.st_mode))
    return unknown;

  if (S_ISDIR (statbuf.st_mode) ||
      S_ISCHR (statbuf.st_mode) || S_ISFIFO (statbuf.st_mode))
    return no;

  return unknown;
}


/* inquire_formatted()-- Given a fortran string, determine if the file
 * is suitable for formatted form.  Returns a C-style string. */

const char *
inquire_formatted (const char *string, int len)
{
  struct stat statbuf;

  if (string == NULL)
    return unknown;

  char *path = fc_strdup (string, len);
  int err = stat (path, &statbuf);
  free (path);
  if (err == -1)
    return unknown;

  if (S_ISREG (statbuf.st_mode) ||
      S_ISBLK (statbuf.st_mode) ||
      S_ISCHR (statbuf.st_mode) || S_ISFIFO (statbuf.st_mode))
    return unknown;

  if (S_ISDIR (statbuf.st_mode))
    return no;

  return unknown;
}


/* inquire_unformatted()-- Given a fortran string, determine if the file
 * is suitable for unformatted form.  Returns a C-style string. */

const char *
inquire_unformatted (const char *string, int len)
{
  return inquire_formatted (string, len);
}


/* inquire_access()-- Given a fortran string, determine if the file is
 * suitable for access. */

static const char *
inquire_access (const char *string, int len, int mode)
{
  if (string == NULL)
    return no;
  char *path = fc_strdup (string, len);
  int res = access (path, mode);
  free (path);
  if (res == -1)
    return no;

  return yes;
}


/* inquire_read()-- Given a fortran string, determine if the file is
 * suitable for READ access. */

const char *
inquire_read (const char *string, int len)
{
  return inquire_access (string, len, R_OK);
}


/* inquire_write()-- Given a fortran string, determine if the file is
 * suitable for READ access. */

const char *
inquire_write (const char *string, int len)
{
  return inquire_access (string, len, W_OK);
}


/* inquire_readwrite()-- Given a fortran string, determine if the file is
 * suitable for read and write access. */

const char *
inquire_readwrite (const char *string, int len)
{
  return inquire_access (string, len, R_OK | W_OK);
}


int
stream_isatty (stream *s)
{
  return isatty (((unix_stream *) s)->fd);
}

int
stream_ttyname (stream *s  __attribute__ ((unused)),
		char * buf  __attribute__ ((unused)),
		size_t buflen  __attribute__ ((unused)))
{
#ifdef HAVE_TTYNAME_R
  return ttyname_r (((unix_stream *) s)->fd, buf, buflen);
#elif defined HAVE_TTYNAME
  char *p;
  size_t plen;
  p = ttyname (((unix_stream *) s)->fd);
  if (!p)
    return errno;
  plen = strlen (p);
  if (buflen < plen)
    plen = buflen;
  memcpy (buf, p, plen);
  return 0;
#else
  return ENOSYS;
#endif
}




/* How files are stored:  This is an operating-system specific issue,
   and therefore belongs here.  There are three cases to consider.

   Direct Access:
      Records are written as block of bytes corresponding to the record
      length of the file.  This goes for both formatted and unformatted
      records.  Positioning is done explicitly for each data transfer,
      so positioning is not much of an issue.

   Sequential Formatted:
      Records are separated by newline characters.  The newline character
      is prohibited from appearing in a string.  If it does, this will be
      messed up on the next read.  End of file is also the end of a record.

   Sequential Unformatted:
      In this case, we are merely copying bytes to and from main storage,
      yet we need to keep track of varying record lengths.  We adopt
      the solution used by f2c.  Each record contains a pair of length
      markers:

	Length of record n in bytes
	Data of record n
	Length of record n in bytes

	Length of record n+1 in bytes
	Data of record n+1
	Length of record n+1 in bytes

     The length is stored at the end of a record to allow backspacing to the
     previous record.  Between data transfer statements, the file pointer
     is left pointing to the first length of the current record.

     ENDFILE records are never explicitly stored.

*/
