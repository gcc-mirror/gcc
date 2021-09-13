/* GNU Objective C Runtime Common Private Definitions
   Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

#ifndef __objc_private_error_INCLUDE_GNU
#define __objc_private_error_INCLUDE_GNU

/* Prints an unrecoverable error to stderr, then aborts the program.
   This should only be used for errors that really are unrecorevable:
   failure to allocate memory, and failure to load an Objective-C
   module.  All other usages of this function should be converted into
   some milder type of error (unless aborting is explicitly required
   by the documentation/API).
*/
void
_objc_abort (const char *fmt, ...) __attribute__ ((noreturn));

#endif /* __objc_private_error_INCLUDE_GNU */
