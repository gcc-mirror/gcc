/*-
 * Copyright (c) 2014 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. [rescinded 22 July 1999]
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*

@deftypefn Supplemental {long long int} strtoll (const char *@var{string}, @
  char **@var{endptr}, int @var{base})
@deftypefnx Supplemental {unsigned long long int} strtoul (@
  const char *@var{string}, char **@var{endptr}, int @var{base})

The @code{strtoll} function converts the string in @var{string} to a
long long integer value according to the given @var{base}, which must be
between 2 and 36 inclusive, or be the special value 0.  If @var{base}
is 0, @code{strtoll} will look for the prefixes @code{0} and @code{0x}
to indicate bases 8 and 16, respectively, else default to base 10.
When the base is 16 (either explicitly or implicitly), a prefix of
@code{0x} is allowed.  The handling of @var{endptr} is as that of
@code{strtod} above.  The @code{strtoull} function is the same, except
that the converted value is unsigned.

@end deftypefn

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>
#ifdef NEED_DECLARATION_ERRNO
extern int errno;
#endif
#include "safe-ctype.h"

#ifdef HAVE_LONG_LONG

__extension__
typedef unsigned long long ullong_type;

__extension__
typedef long long llong_type;

/* FIXME: It'd be nice to configure around these, but the include files are too
   painful.  These macros should at least be more portable than hardwired hex
   constants. */

#ifndef ULLONG_MAX
#define ULLONG_MAX (~(ullong_type)0) /* 0xFFFFFFFFFFFFFFFF */
#endif

#ifndef LLONG_MAX
#define LLONG_MAX ((llong_type)(ULLONG_MAX >> 1)) /* 0x7FFFFFFFFFFFFFFF */
#endif

#ifndef LLONG_MIN
#define LLONG_MIN (~LLONG_MAX) /* 0x8000000000000000 */
#endif

/*
 * Convert a string to a long long integer.
 *
 * Ignores `locale' stuff.  Assumes that the upper and lower case
 * alphabets and digits are each contiguous.
 */
llong_type
strtoll(const char *nptr, char **endptr, register int base)
{
	register const char *s = nptr;
	register ullong_type acc;
	register int c;
	register ullong_type cutoff;
	register int neg = 0, any, cutlim;

	/*
	 * Skip white space and pick up leading +/- sign if any.
	 * If base is 0, allow 0x for hex and 0 for octal, else
	 * assume decimal; if base is already 16, allow 0x.
	 */
	do {
		c = *s++;
	} while (ISSPACE(c));
	if (c == '-') {
		neg = 1;
		c = *s++;
	} else if (c == '+')
		c = *s++;
	if ((base == 0 || base == 16) &&
	    c == '0' && (*s == 'x' || *s == 'X')) {
		c = s[1];
		s += 2;
		base = 16;
	}
	if (base == 0)
		base = c == '0' ? 8 : 10;

	/*
	 * Compute the cutoff value between legal numbers and illegal
	 * numbers.  That is the largest legal value, divided by the
	 * base.  An input number that is greater than this value, if
	 * followed by a legal input character, is too big.  One that
	 * is equal to this value may be valid or not; the limit
	 * between valid and invalid numbers is then based on the last
	 * digit.  For instance, if the range for longs is
	 * [-2147483648..2147483647] and the input base is 10,
	 * cutoff will be set to 214748364 and cutlim to either
	 * 7 (neg==0) or 8 (neg==1), meaning that if we have accumulated
	 * a value > 214748364, or equal but the next digit is > 7 (or 8),
	 * the number is too big, and we will return a range error.
	 *
	 * Set any if any `digits' consumed; make it negative to indicate
	 * overflow.
	 */
	cutoff = neg ? -(ullong_type)LLONG_MIN : LLONG_MAX;
	cutlim = cutoff % (ullong_type)base;
	cutoff /= (ullong_type)base;
	for (acc = 0, any = 0;; c = *s++) {
		if (ISDIGIT(c))
			c -= '0';
		else if (ISALPHA(c))
			c -= ISUPPER(c) ? 'A' - 10 : 'a' - 10;
		else
			break;
		if (c >= base)
			break;
		if (any < 0 || acc > cutoff || (acc == cutoff && c > cutlim))
			any = -1;
		else {
			any = 1;
			acc *= base;
			acc += c;
		}
	}
	if (any < 0) {
		acc = neg ? LLONG_MIN : LLONG_MAX;
		errno = ERANGE;
	} else if (neg)
		acc = -acc;
	if (endptr != 0)
		*endptr = (char *) (any ? s - 1 : nptr);
	return (acc);
}

#endif /* ifdef HAVE_LONG_LONG */
