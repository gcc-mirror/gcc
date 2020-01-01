/* GNU Objective C Runtime accessors - Private Declarations
   Copyright (C) 2010-2020 Free Software Foundation, Inc.
   Contributed by Nicola Pero <nicola.pero@meta-innovation.com>

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

#ifndef __objc_private_accessors_INCLUDE_GNU
#define __objc_private_accessors_INCLUDE_GNU

/* This function needs to be called at startup by init.c.  */
void
__objc_accessors_init (void);

#endif /* not __objc_private_accessors_INCLUDE_GNU */
