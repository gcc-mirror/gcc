/* Support run-time routines for error handling.

   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   Under Section 7 of GPL version 3, you are granted additional permissions
   described in the GCC Runtime Library Exception, version 3.1, as published by
   the Free Software Foundation.

   You should have received a copy of the GNU General Public License and a copy
   of the GCC Runtime Library Exception along with this program; see the files
   COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <stdio.h>
#include <stdlib.h> /* For abort.  */

#include "ga68.h"

/* Run-time error handling.

   Please use the following format when outputing runtime error messages:

     FILE:LINE:[COLUMN:] TEXT

   This keeps the output aligned with other runtime libraries such as the
   sanitizers.  */

/* Emit a formatted error message to the standard output and then terminate the
   process with an error code.  */

void
_libga68_abort (const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  abort ();
  va_end (ap);
}

/* Assertion failure.  */

void
_libga68_assert (const char *filename, unsigned int lineno)
{
  _libga68_abort ("%s:%u: runtime error: ASSERT failure\n",
		 filename, lineno);
}

/* Attempt to dereference NIL failure.  */

void
_libga68_derefnil (const char *filename, unsigned int lineno)
{
  _libga68_abort ("%s:%u: runtime error: attempt to dereference NIL\n",
		  filename, lineno);
}

/* Invalid character expression.  */

void
_libga68_invalidcharerror (const char *filename, unsigned int lineno,
			   int c)
{
  if (c < 0)
    _libga68_abort ("%s:%u: runtime error: %d is not a valid character point\n",
		    filename, lineno, c);
  _libga68_abort ("%s:%u: runtime error: U+%x is not a valid character point\n",
		  filename, lineno, c);
}

/* Out of bounds error in bits ELEM operator.  */

void
_libga68_bitsboundserror (const char *filename, unsigned int lineno,
			  ssize_t pos)
{
  _libga68_abort ("%s:%u: runtime error: bound %zd out of range in ELEM\n",
		  filename, lineno, pos);
}

/* Unreachable error.  */

void
_libga68_unreachable (const char *filename, unsigned int lineno)
{
  _libga68_abort ("%s:%u: runtime error: unreachable reached\n",
		  filename, lineno);
}

/* Lower bound failure.  */

void
_libga68_lower_bound (const char *filename, unsigned int lineno,
			   ssize_t index, ssize_t lower_bound)
{
  _libga68_abort ("%s:%u: runtime error: lower bound %zd must be >= %zd\n",
		  filename, lineno, index, lower_bound);
}

/* Upper bound failure.  */

void
_libga68_upper_bound (const char *filename, unsigned int lineno,
			   ssize_t index, ssize_t upper_bound)
{
  _libga68_abort ("%s:%u: runtime error: upper bound %zd must be <= %zd\n",
		  filename, lineno, index, upper_bound);
}

/* Bounds failure.  */

void
_libga68_bounds (const char *filename, unsigned int lineno,
		 ssize_t index, ssize_t lower_bound, ssize_t upper_bound)
{
  _libga68_abort ("%s:%u: runtime error: bound %zd out of range [%zd:%zd]\n",
		  filename, lineno, index, lower_bound, upper_bound);
}

/* Dimension failure.  */

void
_libga68_dim (const char *filename, unsigned int lineno,
	      size_t dim, size_t index)
{
  _libga68_abort ("%s:%u: runtime error: invalid dimension %zd; shall be > 0 and <= %zu\n",
		  filename, lineno, index, dim);
}

/* Multiples have different bounds in assignations.  */

void
_libga68_bounds_mismatch (const char *filename, unsigned int lineno,
			  size_t dim, ssize_t lb1, ssize_t ub1,
			  ssize_t lb2, ssize_t ub2)
{
  _libga68_abort ("%s:%u: runtime error: multiple bounds mismatch in \
assignation: dim %zu: [%zd:%zd] /= [%zd:%zd]\n",
		  filename, lineno, dim, lb1, ub1, lb2, ub2);
}
