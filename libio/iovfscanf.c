/* 
Copyright (C) 1993 Free Software Foundation

This file is part of the GNU IO Library.  This library is free
software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option)
any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

As a special exception, if you link this library with files
compiled with a GNU compiler to produce an executable, this does not cause
the resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why
the executable file might be covered by the GNU General Public License. */

/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/* Extensively hacked for GNU iostream by Per Bothner 1991, 1992, 1993.
   Changes copyright Free Software Foundation 1992, 1993. */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "%W% (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <libioP.h>
#include <ctype.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifndef	NO_FLOATING_POINT
#define FLOATING_POINT
#endif

#ifdef FLOATING_POINT
#include "floatio.h"
#define	BUF	(MAXEXP+MAXFRACT+3)	/* 3 = sign + decimal point + NUL */
#else
#define	BUF	40
#endif

/*
 * Flags used during conversion.
 */
#define	LONG		0x01	/* l: long or double */
#define	LONGDBL		0x02	/* L: long double; unimplemented */
#define	SHORT		0x04	/* h: short */
#define	SUPPRESS	0x08	/* suppress assignment */
#define	POINTER		0x10	/* weird %p pointer (`fake hex') */
#define	NOSKIP		0x20	/* do not skip blanks */
#define	WIDTH		0x40	/* width */

/*
 * The following are used in numeric conversions only:
 * SIGNOK, NDIGITS, DPTOK, and EXPOK are for floating point;
 * SIGNOK, NDIGITS, PFXOK, and NZDIGITS are for integral.
 */
#define	SIGNOK		0x40	/* +/- is (still) legal */
#define	NDIGITS		0x80	/* no digits detected */

#define	DPTOK		0x100	/* (float) decimal point is still legal */
#define	EXPOK		0x200	/* (float) exponent (e+3, etc) still legal */

#define	PFXOK		0x100	/* 0x prefix is (still) legal */
#define	NZDIGITS	0x200	/* no zero digits detected */

/*
 * Conversion types.
 */
#define	CT_CHAR		0	/* %c conversion */
#define	CT_CCL		1	/* %[...] conversion */
#define	CT_STRING	2	/* %s conversion */
#define	CT_INT		3	/* integer, i.e., strtol or strtoul */
#define	CT_FLOAT	4	/* floating, i.e., strtod */

#define u_char unsigned char
#define u_long unsigned long

#ifdef __cplusplus
extern "C" {
#endif
extern u_long strtoul __P((const char*, char**, int));
extern long strtol __P((const char*, char**, int));
static const u_char *__sccl __P((char *tab, const u_char *fmt));
#ifndef _IO_USE_DTOA
extern double atof();
#endif
#ifdef __cplusplus
}
#endif

/* If errp != NULL, *errp|=1 if we see a premature EOF;
   *errp|=2 if we an invalid character. */

int
DEFUN(_IO_vfscanf, (fp, fmt0, ap, errp),
      register _IO_FILE *fp AND char const *fmt0
      AND _IO_va_list ap AND int *errp)
{
	register const u_char *fmt = (const u_char *)fmt0;
	register int c;		/* character from format, or conversion */
	register _IO_ssize_t width;	/* field width, or 0 */
	register char *p;	/* points into all kinds of strings */
	register int n;		/* handy integer */
	register int flags = 0;	/* flags as defined above */
	register char *p0;	/* saves original value of p when necessary */
	int nassigned;		/* number of fields assigned */
	int nread;		/* number of characters consumed from fp */
	/* Assignments to base and ccfn are just to suppress warnings from gcc.*/
	int base = 0;		/* base argument to strtol/strtoul */
	typedef u_long (*strtoulfn) __P((const char*, char**, int));
	strtoulfn ccfn = 0;
	/* conversion function (strtol/strtoul) */
	char ccltab[256];	/* character class table for %[...] */
	char buf[BUF];		/* buffer for numeric conversions */
	int seen_eof = 0;

	/* `basefix' is used to avoid `if' tests in the integer scanner */
	static short basefix[17] =
		{ 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

	nassigned = 0;
	nread = 0;
	for (;;) {
		c = *fmt++;
		if (c == 0)
			goto done;
		if (isspace(c)) {
			for (;;) {
			        c = _IO_getc(fp);
				if (c == EOF) {
				  seen_eof++;
				  break;
				}
				if (!isspace(c)) {
				    _IO_ungetc (c, fp);
				    break;
				}
				nread++;
			}
			continue;
		}
		if (c != '%')
			goto literal;
		width = 0;
		flags = 0;
		/*
		 * switch on the format.  continue if done;
		 * break once format type is derived.
		 */
again:		c = *fmt++;
		switch (c) {
		case '%':
literal:
		        n = _IO_getc(fp);
			if (n == EOF)
			    goto eof_failure;
			if (n != c) {
			    _IO_ungetc (n, fp);
			    goto match_failure;
			}
			nread++;
			continue;

		case '*':
			if (flags) goto control_failure;
			flags = SUPPRESS;
			goto again;
		case 'l':
			if (flags & ~(SUPPRESS | WIDTH)) goto control_failure;
			flags |= LONG;
			goto again;
		case 'L':
			if (flags & ~(SUPPRESS | WIDTH)) goto control_failure;
			flags |= LONGDBL;
			goto again;
		case 'h':
			if (flags & ~(SUPPRESS | WIDTH)) goto control_failure;
			flags |= SHORT;
			goto again;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			if (flags & ~(SUPPRESS | WIDTH)) goto control_failure;
			flags |= WIDTH;
			width = width * 10 + c - '0';
			goto again;

		/*
		 * Conversions.
		 * Those marked `compat' are for 4.[123]BSD compatibility.
		 *
		 * (According to ANSI, E and X formats are supposed
		 * to the same as e and x.  Sorry about that.)
		 */
		case 'D':	/* compat */
			flags |= LONG;
			/* FALLTHROUGH */
		case 'd':
			c = CT_INT;
			ccfn = (strtoulfn)strtol;
			base = 10;
			break;

		case 'i':
			c = CT_INT;
			ccfn = (strtoulfn)strtol;
			base = 0;
			break;

		case 'O':	/* compat */
			flags |= LONG;
			/* FALLTHROUGH */
		case 'o':
			c = CT_INT;
			ccfn = strtoul;
			base = 8;
			break;

		case 'u':
			c = CT_INT;
			ccfn = strtoul;
			base = 10;
			break;

		case 'X':
		case 'x':
			flags |= PFXOK;	/* enable 0x prefixing */
			c = CT_INT;
			ccfn = strtoul;
			base = 16;
			break;

#ifdef FLOATING_POINT
		case 'E': case 'F':
		case 'e': case 'f': case 'g':
			c = CT_FLOAT;
			break;
#endif

		case 's':
			c = CT_STRING;
			break;

		case '[':
			fmt = __sccl(ccltab, fmt);
			flags |= NOSKIP;
			c = CT_CCL;
			break;

		case 'c':
			flags |= NOSKIP;
			c = CT_CHAR;
			break;

		case 'p':	/* pointer format is like hex */
			flags |= POINTER | PFXOK;
			c = CT_INT;
			ccfn = strtoul;
			base = 16;
			break;

		case 'n':
			if (flags & SUPPRESS)	/* ??? */
				continue;
			if (flags & SHORT)
				*va_arg(ap, short *) = nread;
			else if (flags & LONG)
				*va_arg(ap, long *) = nread;
			else
				*va_arg(ap, int *) = nread;
			continue;

		/*
		 * Disgusting backwards compatibility hacks.	XXX
		 */
		case '\0':	/* compat */
		        nassigned = EOF;
			goto done;

		default:	/* compat */
			if (isupper(c))
				flags |= LONG;
			c = CT_INT;
			ccfn = (strtoulfn)strtol;
			base = 10;
			break;
		}

		/*
		 * We have a conversion that requires input.
		 */
		if (_IO_peekc(fp) == EOF)
			goto eof_failure;

		/*
		 * Consume leading white space, except for formats
		 * that suppress this.
		 */
		if ((flags & NOSKIP) == 0) {
		    n = (unsigned char)*fp->_IO_read_ptr;
		    while (isspace(n)) {
			fp->_IO_read_ptr++;
			nread++;
			n = _IO_peekc(fp);
			if (n == EOF)
			    goto eof_failure;
		    }
		    /* Note that there is at least one character in
		       the buffer, so conversions that do not set NOSKIP
		       can no longer result in an input failure. */
		}

		/*
		 * Do the conversion.
		 */
		switch (c) {

		case CT_CHAR:
			/* scan arbitrary characters (sets NOSKIP) */
			if (width == 0) /* FIXME! */
				width = 1;
			if (flags & SUPPRESS) {
			    _IO_size_t sum = 0;
			    for (;;) {
			        n = fp->_IO_read_end - fp->_IO_read_ptr;
				if (n < (int)width) {
				    sum += n;
				    width -= n;
				    fp->_IO_read_ptr += n;
				    if (__underflow(fp) == EOF)
					if (sum == 0)
					    goto eof_failure;
					else {
					    seen_eof++;
					    break;
					}
				} else {
				    sum += width;
				    fp->_IO_read_ptr += width;
				    break;
				}
			    }
			    nread += sum;
			} else {
			    _IO_size_t r =

			      _IO_XSGETN (fp, (char*)va_arg(ap, char*), width);
			    if (r != width)
				goto eof_failure;
			    nread += r;
			    nassigned++;
			}
			break;

		case CT_CCL:
			/* scan a (nonempty) character class (sets NOSKIP) */
			if (width == 0)
				width = ~0;	/* `infinity' */
			/* take only those things in the class */
			if (flags & SUPPRESS) {
				n = 0;
				while (ccltab[(unsigned char)*fp->_IO_read_ptr]) {
				    n++, fp->_IO_read_ptr++;
				    if (--width == 0)
					break;
				    if (_IO_peekc(fp) == EOF) {
					if (n == 0)
					    goto eof_failure;
					seen_eof++;
					break;
				    }
				}
				if (n == 0)
					goto match_failure;
			} else {
			    p0 = p = va_arg(ap, char *);
			    while (ccltab[(unsigned char)*fp->_IO_read_ptr]) {
				*p++ = *fp->_IO_read_ptr++;
				if (--width == 0)
				    break;
				if (_IO_peekc(fp) == EOF) {
				    if (p == p0)
					goto eof_failure;
				    seen_eof++;
				    break;
				}
			    }
			    n = p - p0;
			    if (n == 0)
				goto match_failure;
			    *p = 0;
			    nassigned++;
			}
			nread += n;
			break;

		case CT_STRING:
			/* like CCL, but zero-length string OK, & no NOSKIP */
			if (width == 0)
				width = ~0;
			if (flags & SUPPRESS) {
				n = 0;
				while (!isspace((unsigned char)*fp->_IO_read_ptr)) {
					n++, fp->_IO_read_ptr++;
					if (--width == 0)
						break;
					if (_IO_peekc(fp) == EOF) {
					    seen_eof++;
					    break;
					}
				}
				nread += n;
			} else {
				p0 = p = va_arg(ap, char *);
				while (!isspace((unsigned char)*fp->_IO_read_ptr)) {
					*p++ = *fp->_IO_read_ptr++;
					if (--width == 0)
						break;
					if (_IO_peekc(fp) == EOF) {
					    seen_eof++;
					    break;
					}
				}
				*p = 0;
				nread += p - p0;
				nassigned++;
			}
			continue;

		case CT_INT:
			/* scan an integer as if by strtol/strtoul */
			if (width == 0 || width > sizeof(buf) - 1)
				width = sizeof(buf) - 1;
			flags |= SIGNOK | NDIGITS | NZDIGITS;
			for (p = buf; width; width--) {
				c = (unsigned char)*fp->_IO_read_ptr;
				/*
				 * Switch on the character; `goto ok'
				 * if we accept it as a part of number.
				 */
				switch (c) {

				/*
				 * The digit 0 is always legal, but is
				 * special.  For %i conversions, if no
				 * digits (zero or nonzero) have been
				 * scanned (only signs), we will have
				 * base==0.  In that case, we should set
				 * it to 8 and enable 0x prefixing.
				 * Also, if we have not scanned zero digits
				 * before this, do not turn off prefixing
				 * (someone else will turn it off if we
				 * have scanned any nonzero digits).
				 */
				case '0':
					if (base == 0) {
						base = 8;
						flags |= PFXOK;
					}
					if (flags & NZDIGITS)
					    flags &= ~(SIGNOK|NZDIGITS|NDIGITS);
					else
					    flags &= ~(SIGNOK|PFXOK|NDIGITS);
					goto ok;

				/* 1 through 7 always legal */
				case '1': case '2': case '3':
				case '4': case '5': case '6': case '7':
					base = basefix[base];
					flags &= ~(SIGNOK | PFXOK | NDIGITS);
					goto ok;

				/* digits 8 and 9 ok iff decimal or hex */
				case '8': case '9':
					base = basefix[base];
					if (base <= 8)
						break;	/* not legal here */
					flags &= ~(SIGNOK | PFXOK | NDIGITS);
					goto ok;

				/* letters ok iff hex */
				case 'A': case 'B': case 'C':
				case 'D': case 'E': case 'F':
				case 'a': case 'b': case 'c':
				case 'd': case 'e': case 'f':
					/* no need to fix base here */
					if (base <= 10)
						break;	/* not legal here */
					flags &= ~(SIGNOK | PFXOK | NDIGITS);
					goto ok;

				/* sign ok only as first character */
				case '+': case '-':
					if (flags & SIGNOK) {
						flags &= ~SIGNOK;
						goto ok;
					}
					break;

				/* x ok iff flag still set & 2nd char */
				case 'x': case 'X':
					if (flags & PFXOK && p == buf + 1) {
						base = 16;	/* if %i */
						flags &= ~PFXOK;
						goto ok;
					}
					break;
				}

				/*
				 * If we got here, c is not a legal character
				 * for a number.  Stop accumulating digits.
				 */
				break;
		ok:
				/*
				 * c is legal: store it and look at the next.
				 */
				*p++ = c;
				fp->_IO_read_ptr++;
				if (_IO_peekc(fp) == EOF) {
				    seen_eof++;
				    break;		/* EOF */
				}
		        }
			/*
			 * If we had only a sign, it is no good; push
			 * back the sign.  If the number ends in `x',
			 * it was [sign] '0' 'x', so push back the x
			 * and treat it as [sign] '0'.
			 */
			if (flags & NDIGITS) {
				if (p > buf)
					(void) _IO_ungetc(*(u_char *)--p, fp);
				goto match_failure;
			}
			c = ((u_char *)p)[-1];
			if (c == 'x' || c == 'X') {
				--p;
				(void) _IO_ungetc (c, fp);
			}
			if ((flags & SUPPRESS) == 0) {
				u_long res;

				*p = 0;
				res = (*ccfn)(buf, (char **)NULL, base);
				if (flags & POINTER)
					*va_arg(ap, void **) = (void *)res;
				else if (flags & SHORT)
					*va_arg(ap, short *) = res;
				else if (flags & LONG)
					*va_arg(ap, long *) = res;
				else
					*va_arg(ap, int *) = res;
				nassigned++;
			}
			nread += p - buf;
			break;

#ifdef FLOATING_POINT
		case CT_FLOAT:
			/* scan a floating point number as if by strtod */
			if (width == 0 || width > sizeof(buf) - 1)
				width = sizeof(buf) - 1;
			flags |= SIGNOK | NDIGITS | DPTOK | EXPOK;
			for (p = buf; width; width--) {
				c = (unsigned char)*fp->_IO_read_ptr;
				/*
				 * This code mimicks the integer conversion
				 * code, but is much simpler.
				 */
				switch (c) {

				case '0': case '1': case '2': case '3':
				case '4': case '5': case '6': case '7':
				case '8': case '9':
					flags &= ~(SIGNOK | NDIGITS);
					goto fok;

				case '+': case '-':
					if (flags & SIGNOK) {
						flags &= ~SIGNOK;
						goto fok;
					}
					break;
				case '.':
					if (flags & DPTOK) {
						flags &= ~(SIGNOK | DPTOK);
						goto fok;
					}
					break;
				case 'e': case 'E':
					/* no exponent without some digits */
					if ((flags&(NDIGITS|EXPOK)) == EXPOK) {
						flags =
						    (flags & ~(EXPOK|DPTOK)) |
						    SIGNOK | NDIGITS;
						goto fok;
					}
					break;
				}
				break;
		fok:
				*p++ = c;
				fp->_IO_read_ptr++;
				if (_IO_peekc(fp) == EOF) {
				    seen_eof++;
				    break;	/* EOF */
				}
			}
			/*
			 * If no digits, might be missing exponent digits
			 * (just give back the exponent) or might be missing
			 * regular digits, but had sign and/or decimal point.
			 */
			if (flags & NDIGITS) {
				if (flags & EXPOK) {
					/* no digits at all */
					while (p > buf)
					    _IO_ungetc (*(u_char *)--p, fp);
					goto match_failure;
				}
				/* just a bad exponent (e and maybe sign) */
				c = *(u_char *)--p;
				if (c != 'e' && c != 'E') {
					(void) _IO_ungetc (c, fp);/* sign */
					c = *(u_char *)--p;
				}
				(void) _IO_ungetc (c, fp);
			}
			if ((flags & SUPPRESS) == 0) {
				double res;
				*p = 0;
#ifdef _IO_USE_DTOA
				res = _IO_strtod(buf, NULL);
#else
				res = atof(buf);
#endif
				if (flags & LONG)
					*va_arg(ap, double *) = res;
				else
					*va_arg(ap, float *) = res;
				nassigned++;
			}
			nread += p - buf;
			break;
#endif /* FLOATING_POINT */
		}
	}
eof_failure:
	seen_eof++;
input_failure:
	if (nassigned == 0)
	    nassigned = -1;
control_failure:
match_failure:
	if (errp)
	    *errp |= 2;
done:
	if (errp && seen_eof)
		*errp |= 1;
	return (nassigned);
}

/*
 * Fill in the given table from the scanset at the given format
 * (just after `[').  Return a pointer to the character past the
 * closing `]'.  The table has a 1 wherever characters should be
 * considered part of the scanset.
 */
static const u_char *
DEFUN(__sccl, (tab, fmt),
      register char *tab AND register const u_char *fmt)
{
	register int c, n, v;

	/* first `clear' the whole table */
	c = *fmt++;		/* first char hat => negated scanset */
	if (c == '^') {
		v = 1;		/* default => accept */
		c = *fmt++;	/* get new first char */
	} else
		v = 0;		/* default => reject */
	/* should probably use memset here */
	for (n = 0; n < 256; n++)
		tab[n] = v;
	if (c == 0)
		return (fmt - 1);/* format ended before closing ] */

	/*
	 * Now set the entries corresponding to the actual scanset
	 * to the opposite of the above.
	 *
	 * The first character may be ']' (or '-') without being special;
	 * the last character may be '-'.
	 */
	v = 1 - v;
	for (;;) {
		tab[c] = v;		/* take character c */
doswitch:
		n = *fmt++;		/* and examine the next */
		switch (n) {

		case 0:			/* format ended too soon */
			return (fmt - 1);

		case '-':
			/*
			 * A scanset of the form
			 *	[01+-]
			 * is defined as `the digit 0, the digit 1,
			 * the character +, the character -', but
			 * the effect of a scanset such as
			 *	[a-zA-Z0-9]
			 * is implementation defined.  The V7 Unix
			 * scanf treats `a-z' as `the letters a through
			 * z', but treats `a-a' as `the letter a, the
			 * character -, and the letter a'.
			 *
			 * For compatibility, the `-' is not considerd
			 * to define a range if the character following
			 * it is either a close bracket (required by ANSI)
			 * or is not numerically greater than the character
			 * we just stored in the table (c).
			 */
			n = *fmt;
			if (n == ']' || n < c) {
				c = '-';
				break;	/* resume the for(;;) */
			}
			fmt++;
			do {		/* fill in the range */
				tab[++c] = v;
			} while (c < n);
#if 1	/* XXX another disgusting compatibility hack */
			/*
			 * Alas, the V7 Unix scanf also treats formats
			 * such as [a-c-e] as `the letters a through e'.
			 * This too is permitted by the standard....
			 */
			goto doswitch;
#else
			c = *fmt++;
			if (c == 0)
				return (fmt - 1);
			if (c == ']')
				return (fmt);
#endif
			break;

		case ']':		/* end of scanset */
			return (fmt);

		default:		/* just another character */
			c = n;
			break;
		}
	}
	/* NOTREACHED */
}
