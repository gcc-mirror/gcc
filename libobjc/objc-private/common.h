/* GNU Objective C Runtime Common Private Definitions
   Copyright (C) 2010-2023 Free Software Foundation, Inc.
   Contributed by Nicola Pero

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef __objc_private_common_INCLUDE_GNU
#define __objc_private_common_INCLUDE_GNU

/* This file contains definitions that should be included by all .c
   and .m files in libobjc.  */

/* When debugging libobjc, add

   #define DEBUG 1

   at the very beginning of a file in libobjc (before including this file) to turn
   on DEBUG_PRINTF().  */
#ifdef DEBUG
#include <stdio.h>
#define DEBUG_PRINTF(format, args...) printf (format, ## args)
#else
#define DEBUG_PRINTF(format, args...)
#endif 

#endif /* __objc_private_common_INCLUDE_GNU */
