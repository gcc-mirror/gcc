/* 
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

#ifdef __GNUC__
#pragma implementation
#endif
#define _STREAM_COMPAT
#include "builtinbuf.h"
#include "iostreamP.h"
#if !_IO_UNIFIED_JUMPTABLES
int builtinbuf::overflow(int ch) { return _IO_OVERFLOW (this, ch); }

int builtinbuf::underflow() { return _IO_UNDERFLOW (this); }

streamsize builtinbuf::xsgetn(char* buf, streamsize n)
{ return _IO_XSGETN (this, buf, n); }

streamsize builtinbuf::xsputn(const char* buf, streamsize n)
{ return _IO_XSPUTN (this, buf, n); }

int builtinbuf::doallocate() { return _IO_DOALLOCATE (this); }

builtinbuf::~builtinbuf() { _IO_FINISH (this); }

int builtinbuf::sync() { return _IO_SYNC (this); }

streambuf* builtinbuf::setbuf(char *buf, int n)
{ return (streambuf*)_IO_SETBUF (this, buf, n); }

streampos builtinbuf::seekoff(streamoff off, _seek_dir dir, int mode)
{
  return _IO_SEEKOFF (this, off, dir, mode);
}

streampos builtinbuf::seekpos(streampos pos, int mode)
{
  return _IO_SEEKPOS (this, pos, mode);
}

int builtinbuf::pbackfail(int c)
{ return _IO_PBACKFAIL (this, c); }

streamsize builtinbuf::sys_read(char* buf, streamsize size)
{ return _IO_SYSREAD (this, buf, size); }

streampos builtinbuf::sys_seek(streamoff off, _seek_dir dir)
{ return _IO_SYSSEEK (this, off, dir); }

streamsize builtinbuf::sys_write(const char* buf, streamsize size)
{ return _IO_SYSWRITE (this, buf, size); }

int builtinbuf::sys_stat(void* buf) // Actually, a (struct stat*)
{ return _IO_SYSSTAT (this, buf); }

int builtinbuf::sys_close()
{ return _IO_SYSCLOSE (this); }
#endif
