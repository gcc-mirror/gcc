/* endian.h file crafted from relevant parts of libc include/endian.h
   string/endian.h and arch-specific bits/endian.h.
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#ifndef LIBGCCM_ENDIAN_H
#define LIBGCCM_ENDIAN_H

#define __BIG_ENDIAN 4321
#define __LITTLE_ENDIAN 1234
#define BIG_ENDIAN 4321
#define LITTLE_ENDIAN 1234

/* Change this.  */
#define __FLOAT_WORD_ORDER 1234
#define __BYTE_ORDER 1234

#if __FLOAT_WORD_ORDER == BIG_ENDIAN
# define BIG_ENDI 1
# undef LITTLE_ENDI
# define HIGH_HALF 0
# define  LOW_HALF 1
#else
# if __FLOAT_WORD_ORDER == LITTLE_ENDIAN
#  undef BIG_ENDI
#  define LITTLE_ENDI 1
#  define HIGH_HALF 1
#  define  LOW_HALF 0
# endif
#endif

#endif
