/* Provide a version vfprintf in terms of _doprnt.
   By Kaveh Ghazi  (ghazi@caip.rutgers.edu)  3/29/98
   Copyright (C) 1998 Free Software Foundation, Inc.
 */

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>
#undef vfprintf

int
vfprintf (stream, format, ap)
  FILE * stream;
  const char * format;
  va_list ap;
{
  return _doprnt (format, ap, stream);
}
