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

#include "libioP.h"
#include <iostream.h>
#include <stdarg.h>

istream& istream::scan(const char *format ...)
{
    if (ipfx0()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	va_list ap;
	va_start(ap, format);
	_strbuf->vscan(format, ap, this);
	va_end(ap);
	isfx();
	_IO_cleanup_region_end (0);
    }
    return *this;
}

istream& istream::vscan(const char *format, _IO_va_list args)
{
    if (ipfx0())
      {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	_strbuf->vscan(format, args, this);
	isfx();
	_IO_cleanup_region_end (0);
      }
    return *this;
}
