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
#include "iostreamP.h"
#include "strstream.h"
#include <string.h>

static void* default_alloc(_IO_size_t size)
{
    return (void*)new char[size];
}

static void default_free(void* ptr)
{
    delete [] (char*)ptr;
}

istrstream::istrstream(const char *cp, int n)
{
  __my_sb.init_readonly (cp, n);
}

strstreambase::strstreambase(char *cp, int n, int mode)
: __my_sb (cp, n,
	   (mode == ios::app || mode == ios::ate) ? cp + strlen(cp) : cp)
{
  init (&__my_sb);
}

char *strstreambuf::str()
{
    freeze(1);
    return base();
}

_IO_ssize_t strstreambuf::pcount () { return _IO_write_ptr - _IO_write_base; }

int strstreambuf::overflow(int c /* = EOF */)
{
  return _IO_str_overflow (this, c);
}

int strstreambuf::underflow()
{
  return _IO_str_underflow(this);
}


void strstreambuf::init_dynamic(_IO_alloc_type alloc, _IO_free_type free,
				int initial_size)
				
{
    _s._allocate_buffer = alloc ? alloc : default_alloc;
    _s._free_buffer = free ? free : default_free;
    if (initial_size > 0)
      {
	char * buf = (char*)(*_s._allocate_buffer)(initial_size);
	setb(buf, buf + initial_size, 1);
	setp(buf, buf + initial_size);
	setg(buf, buf, buf);
      }
}

void strstreambuf::init_static(char *ptr, int size, char *pstart)
{
  _IO_str_init_static (this, ptr, size, pstart);
}

void strstreambuf::init_readonly (const char *ptr, int size)
{
  _IO_str_init_readonly (this, ptr, size);
}

strstreambuf::~strstreambuf()
{
    if (_IO_buf_base && !(_flags & _IO_USER_BUF))
        (_s._free_buffer)(_IO_buf_base);
    _IO_buf_base = NULL;
}

streampos strstreambuf::seekoff(streamoff off, _seek_dir dir,
					int mode /*=ios::in|ios::out*/)
{
  return _IO_str_seekoff (this, off, dir, mode);
}

int strstreambuf::pbackfail(int c)
{
  return _IO_str_pbackfail (this, c);
}
