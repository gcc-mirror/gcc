/* GNU Objective C Runtime Error Functions
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 2002, 2009, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "objc-private/common.h"
#include "objc-private/error.h"

/* __USE_FIXED_PROTOTYPES__ used to be required to get prototypes for
   malloc, free, etc. on some platforms.  It is unclear if we still
   need it, but it can't hurt.  */
#define __USE_FIXED_PROTOTYPES__
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

/* Prints an error message and aborts the program.  */
void
_objc_abort (const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  abort ();
  va_end (ap);
}

/* The rest of the file is deprecated.  */
#include "objc/objc-api.h" /* For objc_error_handler.  */

/*
** Error handler function
** NULL so that default is to just print to stderr
*/
static objc_error_handler _objc_error_handler = NULL;

/* Trigger an objc error */
void
objc_error (id object, int code, const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  objc_verror (object, code, fmt, ap);
  va_end (ap);
}

/* Trigger an objc error */
void
objc_verror (id object, int code, const char *fmt, va_list ap)
{
  BOOL result = NO;

  /* Call the error handler if its there
     Otherwise print to stderr */
  if (_objc_error_handler)
    result = (*_objc_error_handler) (object, code, fmt, ap);
  else
    vfprintf (stderr, fmt, ap);

  /* Continue if the error handler says its ok
     Otherwise abort the program */
  if (result)
    return;
  else
    abort ();
}

/* Set the error handler */
objc_error_handler
objc_set_error_handler (objc_error_handler func)
{
  objc_error_handler temp = _objc_error_handler;
  _objc_error_handler = func;
  return temp;
}
