/* This is part of libio/iostream, providing -*- C++ -*- input/output.
Copyright (C) 1993 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

/* Written by Per Bothner (bothner@cygnus.com). */

#ifdef __GNUG__
#pragma implementation
#endif

#include "libioP.h"
#include <stdiostream.h>

// A stdiobuf is "tied" to a FILE object (as used by the stdio package).
// Thus a stdiobuf is always synchronized with the corresponding FILE,
// though at the cost of some overhead.  (If you use the implementation
// of stdio supplied with this library, you don't need stdiobufs.)
// This implementation inherits from filebuf, but implement the virtual
// functions sys_read/..., using the stdio functions fread/... instead
// of the low-level read/... system calls.  This has the advantage that
// we get all of the nice filebuf semantics automatically, though
// with some overhead.


#ifndef SEEK_SET
#define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif
#ifndef SEEK_END
#define SEEK_END 2
#endif

stdiobuf::stdiobuf(FILE *f) : filebuf(fileno(f))
{
    _file = f;
    // Turn off buffer in stdiobuf.  Instead, rely on buffering in (FILE).
    // Thus the stdiobuf will be synchronized with the FILE.
    setbuf(NULL, 0);
}

stdiobuf::~stdiobuf()
{
  /* Only needed if we're buffered.  Not buffered is the default. */
  _IO_do_flush((_IO_FILE*)this);
}

streamsize stdiobuf::sys_read(char* buf, streamsize size)
{
  // A minor optimization, but it makes a noticable difference.
  // A bigger optimization would be to write stdiobuf::underflow,
  // but that has some modularity disadvantages.  Re-evaluate that
  // after we have gotten rid of the double indirection.  FIXME
  if (size == 1)
    {
      register int ch = getc(_file);
      if (ch == EOF)
	return 0;
      *buf = (char)ch;
      return 1;
    }
  else
    return fread(buf, 1, size, _file);
}

streamsize stdiobuf::sys_write(const char *buf, streamsize n)
{
    _IO_ssize_t count = fwrite(buf, 1, n, _file);
    if (_offset >= 0)
	_offset += n;
    return count;
}

streampos stdiobuf::sys_seek(streamoff offset, _seek_dir dir)
{
    // Normally, equivalent to: fdir=dir
    int fdir =
	(dir == ios::beg) ? SEEK_SET :
        (dir == ios::cur) ? SEEK_CUR :
        (dir == ios::end) ? SEEK_END :
        dir;
    return fseek(_file, offset, fdir);
}

int stdiobuf::sys_close()
{
    int status = fclose(_file);
    _file = NULL;
    return status;
}

int stdiobuf::sync()
{
  if (_IO_do_flush((_IO_FILE*)this))
    return EOF;
  if (!(xflags() & _IO_NO_WRITES))
    if (fflush(_file))
      return EOF;
  return 0;
}

int stdiobuf::overflow(int c /* = EOF*/)
{
    if (filebuf::overflow(c) == EOF)
	return EOF;
    if (c != EOF)
	return c;
    return fflush(_file);
}

streamsize stdiobuf::xsputn(const char* s, streamsize n)
{
  if (buffered ())
    {
      // The filebuf implementation of sputn loses.
      return streambuf::xsputn(s, n);
    }
  else
    return fwrite (s, 1, n, _file);
}

void stdiobuf::buffered (int b)
{
  if (b)
    {
      if (_flags & _IO_UNBUFFERED)
	{ /* Was unbuffered, make it buffered. */
	  _flags &= ~_IO_UNBUFFERED;
	}
    }
  else
    {
      if (!(_flags & _IO_UNBUFFERED))
	{ /* Was buffered, make it unbuffered. */
	  setbuf(NULL, 0);
	}
    }
}
