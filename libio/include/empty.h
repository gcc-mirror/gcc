/* Thread package specific definitions of stream lock type.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef _LIBIO_LOCK_H
#define _LIBIO_LOCK_H	1

typedef void *_IO_lock_t;

/* We need recursive (counting) mutexes.  */
#define _IO_lock_initializer NULL


#define _IO_cleanup_region_start(_fct, _fp)
#define _IO_cleanup_region_end(_doit)
#define _IO_lock_init(_name)
#define _IO_lock_fini(_name)
#define _IO_lock_lock(_name)
#define _IO_lock_unlock(_name)

#endif /* libio-lock.h */
/* this will be used later*/
