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

#include "libioP.h"
#include "strfile.h"

int
_IO_vsprintf (string, format, args)
     char *string;
     const char *format;
     _IO_va_list args;
{
  _IO_strfile sf;
#ifdef _IO_MTSAFE_IO
  _IO_lock_t lock;
#endif
  int ret;

#ifdef _IO_MTSAFE_IO
  sf._sbf._f._lock = &lock;
#endif
  _IO_init (&sf._sbf._f, 0);
  _IO_JUMPS (&sf._sbf._f) = &_IO_str_jumps;
  _IO_str_init_static (&sf._sbf._f, string, -1, string);
  _IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile, &sf);
  _IO_flockfile (&sf._sbf._f);
  ret = _IO_vfprintf (&sf._sbf._f, format, args);
  _IO_putc_unlocked ('\0', &sf._sbf._f);
  _IO_cleanup_region_end (1);
  return ret;
}

#ifdef weak_alias
weak_alias (_IO_vsprintf, vsprintf)
#elif defined(_G_STDIO_USES_LIBIO) && defined(_G_HAVE_WEAK_SYMBOL)
int vsprintf (char *, const char *, _IO_va_list)
	      __attribute__ ((weak, alias("_IO_vsprintf")));
#endif
