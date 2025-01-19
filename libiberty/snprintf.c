/* Implement the snprintf function.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
   Written by Kaveh R. Ghazi <ghazi@caip.rutgers.edu>.

This file is part of the libiberty library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

/*

@deftypefn Supplemental int snprintf (char *@var{buf}, size_t @var{n}, @
  const char *@var{format}, ...)

This function is similar to @code{sprintf}, but it will write to
@var{buf} at most @code{@var{n}-1} bytes of text, followed by a
terminating null byte, for a total of @var{n} bytes.
On error the return value is -1, otherwise it returns the number of
bytes, not including the terminating null byte, that would have been
written had @var{n} been sufficiently large, regardless of the actual
value of @var{n}.  Note some pre-C99 system libraries do not implement
this correctly so users cannot generally rely on the return value if
the system version of this function is used.

@end deftypefn

*/

#include "ansidecl.h"

#include <stdarg.h>
#include <stddef.h>

int vsnprintf (char *, size_t, const char *, va_list);

int
snprintf (char *s, size_t n, const char *format, ...)
{
  int result;
  va_list ap;
  va_start (ap, format);
  result = vsnprintf (s, n, format, ap);
  va_end (ap);
  return result;
}
