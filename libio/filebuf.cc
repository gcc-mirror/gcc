/* This is part of libio/iostream, providing -*- C++ -*- input/output.
Copyright (C) 1993, 1995 Free Software Foundation

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
the executable file might be covered by the GNU General Public License.

Written by Per Bothner (bothner@cygnus.com). */

#include "iostreamP.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include "builtinbuf.h"

void filebuf::init()
{
  _IO_file_init(this);
}

filebuf::filebuf()
{
  _IO_file_init(this);
}

#if !_IO_UNIFIED_JUMPTABLES
/* This is like "new filebuf()", but it uses the _IO_file_jump jumptable,
   for eficiency. */

filebuf* filebuf::__new()
{
  filebuf *fb = new filebuf;
  _IO_JUMPS(fb) = &_IO_file_jumps;
  fb->_vtable() = builtinbuf_vtable;
  return fb;
}
#endif

filebuf::filebuf(int fd)
{
  _IO_file_init(this);
  _IO_file_attach(this, fd);
}

filebuf::filebuf(int fd, char* p, int len)
{
  _IO_file_init(this);
  _IO_file_attach(this, fd);
  setbuf(p, len);
}

filebuf::~filebuf()
{
  if (_IO_file_is_open(this))
    {
      _IO_do_flush (this);
      if (!(xflags() & _IO_DELETE_DONT_CLOSE))
	_IO_SYSCLOSE (this);
    }
}

filebuf* filebuf::open(const char *filename, ios::openmode mode, int prot)
{
  if (_IO_file_is_open (this))
    return NULL;
  int posix_mode;
  int read_write;
  if (mode & ios::app)
    mode |= ios::out;
  if ((mode & (ios::in|ios::out)) == (ios::in|ios::out)) {
    posix_mode = O_RDWR;
    read_write = 0;
  }
  else if (mode & ios::out)
    posix_mode = O_WRONLY, read_write = _IO_NO_READS;
  else if (mode & (int)ios::in)
    posix_mode = O_RDONLY, read_write = _IO_NO_WRITES;
  else
    posix_mode = 0, read_write = _IO_NO_READS+_IO_NO_WRITES;
  if (mode & ios::binary)
    {
      mode &= ~ios::binary;
#ifdef O_BINARY
      /* This is a (mis-)feature of DOS/Windows C libraries. */
      posix_mode |= O_BINARY;
#endif
    }
  if ((mode & (int)ios::trunc) || mode == (int)ios::out)
    posix_mode |= O_TRUNC;
  if (mode & ios::app)
    posix_mode |= O_APPEND, read_write |= _IO_IS_APPENDING;
  if (!(mode & (int)ios::nocreate) && mode != ios::in)
    posix_mode |= O_CREAT;
  if (mode & (int)ios::noreplace)
    posix_mode |= O_EXCL;
#if _G_HAVE_IO_FILE_OPEN
  return (filebuf*)_IO_file_open (this, filename, posix_mode, prot,
				  read_write, 0);
#else
  int fd = ::open(filename, posix_mode, prot);
  if (fd < 0)
    return NULL;
  _fileno = fd;
  xsetflags(read_write, _IO_NO_READS+_IO_NO_WRITES+_IO_IS_APPENDING);
  if (mode & (ios::ate|ios::app)) {
    if (pubseekoff(0, ios::end) == EOF)
      return NULL;
  }
  _IO_link_in(this);
  return this;
#endif
}

filebuf* filebuf::open(const char *filename, const char *mode)
{
#if _G_IO_IO_FILE_VERSION == 0x20001
  return (filebuf*)_IO_file_fopen(this, filename, mode, 0);
#else
  return (filebuf*)_IO_file_fopen(this, filename, mode);
#endif
}

filebuf* filebuf::attach(int fd)
{
  return (filebuf*)_IO_file_attach(this, fd);
}

streambuf* filebuf::setbuf(char* p, int len)
{
  return (streambuf*)_IO_file_setbuf (this, p, len);
}

int filebuf::doallocate() { return _IO_file_doallocate(this); }

int filebuf::overflow(int c)
{
  return _IO_file_overflow(this, c);
}

int filebuf::underflow()
{
  return _IO_file_underflow(this);
}

int filebuf::sync()
{
  return _IO_file_sync(this);
}

streampos filebuf::seekoff(streamoff offset, _seek_dir dir, int mode)
{
  return _IO_file_seekoff (this, offset, dir, mode);
}

filebuf* filebuf::close()
{
  return (_IO_file_close_it(this) ? (filebuf*)NULL : this);
}

streamsize filebuf::sys_read(char* buf, streamsize size)
{
  return _IO_file_read(this, buf, size);
}

streampos filebuf::sys_seek(streamoff offset, _seek_dir dir)
{
  return _IO_file_seek(this, offset, dir);
}

streamsize filebuf::sys_write(const char *buf, streamsize n)
{
  return _IO_file_write (this, buf, n);
}

int filebuf::sys_stat(void* st)
{
  return _IO_file_stat (this, st);
}

int filebuf::sys_close()
{
  return _IO_file_close (this);
}

streamsize filebuf::xsputn(const char *s, streamsize n)
{
  return _IO_file_xsputn(this, s, n);
}

streamsize filebuf::xsgetn(char *s, streamsize n)
{
    // FIXME: OPTIMIZE THIS (specifically, when unbuffered()).
    return streambuf::xsgetn(s, n);
}

// Non-ANSI AT&T-ism:  Default open protection.
const int filebuf::openprot = 0644;
