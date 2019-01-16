/* Copyright (C) 2018-2019 Free Software Foundation, Inc.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libgomp.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>  /* For PRIx64.  */
#endif
#ifdef HAVE_UNAME
#include <sys/utsname.h>
#endif

/* The HAVE_GETPID and HAVE_GETHOSTNAME configure tests are passing for nvptx,
   while the nvptx newlib implementation does not support those functions.
   Override the configure test results here.  */
#undef HAVE_GETPID
#undef HAVE_GETHOSTNAME

/* The nvptx newlib implementation does not support fwrite, but it does support
   write.  Map fwrite to write.  */
#undef fwrite
#define fwrite(ptr, size, nmemb, stream) write (1, (ptr), (nmemb) * (size))

#include "../../affinity-fmt.c"

