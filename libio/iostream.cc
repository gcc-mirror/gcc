/* This is part of libio/iostream, providing -*- C++ -*- input/output.
   Copyright (C) 1993, 1997, 2000 Free Software Foundation, Inc.

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
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307,
   USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License. */

/* Written by Per Bothner (bothner@cygnus.com). */

#ifdef __GNUC__
#pragma implementation
#endif
#define _STREAM_COMPAT
#include <iostream.h>
#include "libioP.h"
#include <stdio.h>  /* Needed for sprintf */
#include <ctype.h>
#include <string.h>
#include <limits.h>

#if _G_HAVE_PRINTF_FP
#include <printf.h>
extern "C" int __printf_fp (_IO_FILE *, const struct printf_info *,
			    const void *const *);
#else
#include "floatio.h"
# ifndef _IO_USE_DTOA
int __cvt_double(double number, register int prec, int flags, int *signp,
                 int fmtch, char *startp, char *endp);
# endif
#endif

#define	BUF		(MAXEXP+MAXFRACT+1)	/* + decimal point */

//#define isspace(ch) ((ch)==' ' || (ch)=='\t' || (ch)=='\n')

istream::istream(streambuf *sb, ostream* tied)
{
  init (sb, tied);
  _gcount = 0;
}

int skip_ws(streambuf* sb)
{
    int ch;
    for (;;) {
	ch = sb->sbumpc();
	if (ch == EOF || !isspace(ch))
	    return ch;
    }
}

istream& istream::get(char& c)
{
    if (ipfx1()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	int ch = _strbuf->sbumpc();
	if (ch == EOF) {
	  set(ios::eofbit|ios::failbit);
	  _gcount = 0;
	}
	else {
	  c = (char)ch;
	  _gcount = 1;
	}
	isfx();
	_IO_cleanup_region_end (0);
    }
    else
      _gcount = 0;
    return *this;
}

int istream::peek()
{
  if (!good())
    return EOF;
  if (_tie && rdbuf()->in_avail() == 0)
    _tie->flush();
  int ch = _strbuf->sgetc();
  if (ch == EOF)
    set(ios::eofbit);
  return ch;
}

istream& istream::ignore(int n /* = 1 */, int delim /* = EOF */)
{
    _gcount = 0;
    if (ipfx1()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	register streambuf* sb = _strbuf;
	if (delim == EOF) {
	    _gcount = sb->ignore(n);
	    goto unlock;
	}
	for (;;) {
#if 0
	    if (n != MAXINT) // FIXME
#endif
	    if (--n < 0)
		break;
	    int ch = sb->sbumpc();
	    if (ch == EOF) {
		set(ios::eofbit|ios::failbit);
		break;
	    }
	    _gcount++;
	    if (ch == delim)
		break;
	}
    unlock:
	isfx();
	_IO_cleanup_region_end (0);
    }
    return *this;
}

istream& istream::read(char *s, streamsize n)
{
    if (ipfx1()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	_gcount = _strbuf->sgetn(s, n);
	if (_gcount != n)
	    set(ios::failbit|ios::eofbit);
	isfx();
	_IO_cleanup_region_end (0);
    }
    else
      _gcount = 0;
    return *this;
}

int
istream::sync ()
{
  streambuf *sb = rdbuf ();
  if (sb == NULL)
    return EOF;
  if (sb->sync ()) // Later: pubsync
    {
      setstate (ios::badbit);
      return EOF;
    }
  else
    return 0;
}

istream& istream::seekg(streampos pos)
{
    pos = _strbuf->pubseekpos(pos, ios::in);
    if (pos == streampos(EOF))
	set(ios::badbit);
    return *this;
}

istream& istream::seekg(streamoff off, _seek_dir dir)
{
  streampos pos = _IO_seekoff (_strbuf, off, (int) dir, _IOS_INPUT);
  if (pos == streampos(EOF))
    set(ios::badbit);
  return *this;
}

streampos istream::tellg()
{
#if 0
    streampos pos = _strbuf->pubseekoff(0, ios::cur, ios::in);
#else
    streampos pos = _IO_seekoff (_strbuf, 0, _IO_seek_cur, _IOS_INPUT);
#endif
    if (pos == streampos(EOF))
	set(ios::badbit);
    return pos;
}

istream& istream::operator>>(char& c)
{
    if (ipfx0()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	int ch = _strbuf->sbumpc();
	if (ch == EOF)
	    set(ios::eofbit|ios::failbit);
	else
	    c = (char)ch;
	isfx();
	_IO_cleanup_region_end (0);
    }
    return *this;
}

istream&
istream::operator>> (char* ptr)
{
  register char *p = ptr;
  int w = width(0);
  if (ipfx0())
    {
      _IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				_strbuf);
      register streambuf* sb = _strbuf;
      for (;;)
	{
	  int ch = sb->sbumpc();
	  if (ch == EOF)
	    {
	      set(ios::eofbit);
	      break;
	    }
	  else if (isspace(ch) || w == 1)
	    {
	      sb->sputbackc(ch);
	      break;
	    }
	  else *p++ = ch;
	  w--;
	}
      if (p == ptr)
	set(ios::failbit);
      isfx();
      _IO_cleanup_region_end (0);
    }
  *p = '\0';
  return *this;
}

#if defined(__GNUC__) && !defined(__STRICT_ANSI__)
#define LONGEST long long
#else
#define LONGEST long
#endif

static int read_int(istream& stream, unsigned LONGEST& val, int& neg)
{
    if (!stream.ipfx0())
      return 0;
    int retval;
    _IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
			      stream._strbuf);
    register streambuf* sb = stream.rdbuf();
    int base = 10;
    int ndigits = 0;
    register int ch = skip_ws(sb);
    if (ch == EOF)
	goto eof_fail;
    neg = 0;
    if (ch == '+') {
	ch = skip_ws(sb);
    }
    else if (ch == '-') {
	neg = 1;
	ch = skip_ws(sb);
    }
    if (ch == EOF) goto eof_fail;
    if (!(stream.flags() & ios::basefield)) {
	if (ch == '0') {
	    ch = sb->sbumpc();
	    if (ch == EOF) {
		val = 0;
		goto unlock;
	    }
	    if (ch == 'x' || ch == 'X') {
		base = 16;
		ch = sb->sbumpc();
		if (ch == EOF) goto eof_fail;
	    }
	    else {
		sb->sputbackc(ch);
		base = 8;
		ch = '0';
	    }
	}
    }
    else if ((stream.flags() & ios::basefield) == ios::hex)
	base = 16;
    else if ((stream.flags() & ios::basefield) == ios::oct)
	base = 8;
    val = 0;
    for (;;) {
	if (ch == EOF)
	    break;
	int digit;
	if (ch >= '0' && ch <= '9')
	    digit = ch - '0';
	else if (ch >= 'A' && ch <= 'F')
	    digit = ch - 'A' + 10;
	else if (ch >= 'a' && ch <= 'f')
	    digit = ch - 'a' + 10;
	else
	    digit = 999;
	if (digit >= base) {
	    sb->sputbackc(ch);
	    if (ndigits == 0)
		goto fail;
	    else
		goto unlock;
	}
	ndigits++;
	val = base * val + digit;
	ch = sb->sbumpc();
    }
  unlock:
    retval = 1;
    goto out;
  fail:
    stream.set(ios::failbit);
    retval = 0;
    goto out;
  eof_fail:
    stream.set(ios::failbit|ios::eofbit);
    retval = 0;
  out:
    stream.isfx();
    _IO_cleanup_region_end (0);
    return retval;
}

#define READ_INT(TYPE) \
istream& istream::operator>>(TYPE& i)\
{\
    unsigned LONGEST val; int neg;\
    if (read_int(*this, val, neg)) {\
	if (neg) val = -val;\
	i = (TYPE)val;\
    }\
    return *this;\
}

READ_INT(short)
READ_INT(unsigned short)
READ_INT(int)
READ_INT(unsigned int)
READ_INT(long)
READ_INT(unsigned long)
#if defined(__GNUC__) && !defined(__STRICT_ANSI__)
READ_INT(long long)
READ_INT(unsigned long long)
#endif
#if _G_HAVE_BOOL
READ_INT(bool)
#endif

istream& istream::operator>>(long double& x)
{
    if (ipfx0())
      {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
#if _G_HAVE_LONG_DOUBLE_IO
	scan("%Lg", &x);
#else
	double y;
	scan("%lg", &y);
	x = y;
#endif
	isfx();
	_IO_cleanup_region_end (0);
      }
    return *this;
}

istream& istream::operator>>(double& x)
{
    if (ipfx0())
      {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	scan("%lg", &x);
	isfx();
	_IO_cleanup_region_end (0);
      }
    return *this;
}

istream& istream::operator>>(float& x)
{
    if (ipfx0())
      {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	scan("%g", &x);
	isfx();
	_IO_cleanup_region_end (0);
      }
    return *this;
}

istream& istream::operator>>(register streambuf* sbuf)
{
    if (ipfx0()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	register streambuf* inbuf = rdbuf();
	// FIXME: Should optimize!
	for (;;) {
	    register int ch = inbuf->sbumpc();
	    if (ch == EOF) {
		set(ios::eofbit);
		break;
	    }
	    if (sbuf->sputc(ch) == EOF) {
		set(ios::failbit);
		break;
	    }
	}
	isfx();
	_IO_cleanup_region_end (0);
    }
    return *this;
}

ostream& ostream::operator<<(char c)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
#if 1
	// This is what the cfront implementation does.
	if (_strbuf->sputc(c) == EOF) {
	    set(ios::badbit);
	    goto failed;
	}
#else
	// This is what cfront documentation and current ANSI drafts say.
	int w = width(0);
	char fill_char = fill();
	register int padding = w > 0 ? w - 1 : 0;
	register streambuf *sb = _strbuf;
	if (!(flags() & ios::left) && padding) // Default adjustment.
	    if (_IO_padn(sb, fill_char, padding) < padding) {
	      set(ios::badbit);
	      goto failed;
	    }
	if (sb->sputc(c) == EOF) {
	  set(ios::badbit);
	  goto failed;
        }
	if (flags() & ios::left && padding) // Left adjustment.
	    if (_IO_padn(sb, fill_char, padding) < padding)
	      set(ios::badbit);
#endif
       failed:
	osfx();
	_IO_cleanup_region_end (0);
    }
    return *this;
}

/* Write VAL on STREAM.
   If SIGN<0, val is the absolute value of a negative number.
   If SIGN>0, val is a signed non-negative number.
   If SIGN==0, val is unsigned. */

static void write_int(ostream& stream, unsigned LONGEST val, int sign)
{
#define WRITE_BUF_SIZE (10 + sizeof(unsigned LONGEST) * 3)
    char buf[WRITE_BUF_SIZE];
    register char *buf_ptr = buf+WRITE_BUF_SIZE; // End of buf.
    const char *show_base = "";
    int show_base_len = 0;
    int show_pos = 0; // If 1, print a '+'.

    // Now do the actual conversion, placing the result at the *end* of buf.
    // Note that we use separate code for decimal, octal, and hex,
    // so we can divide by optimizable constants.
    if ((stream.flags() & ios::basefield) == ios::oct) { // Octal
	do {
	    *--buf_ptr = (val & 7) + '0';
	    val = val >> 3;
	} while (val != 0);
	if ((stream.flags() & ios::showbase) && (*buf_ptr != '0'))
	    *--buf_ptr = '0';
    }
    else if ((stream.flags() & ios::basefield) == ios::hex) { // Hex
	const char *xdigs = (stream.flags() & ios::uppercase) ? "0123456789ABCDEF0X"
	    : "0123456789abcdef0x";
	do {
	    *--buf_ptr = xdigs[val & 15];
	    val = val >> 4;
	} while (val != 0);
	if ((stream.flags() & ios::showbase)) {
	    show_base = xdigs + 16; // Either "0X" or "0x".
	    show_base_len = 2;
	}
    }
    else { // Decimal
#if defined(__GNUC__) && !defined(__STRICT_ANSI__)
	// Optimization:  Only use long long when we need to.
	while (val > UINT_MAX) {
	    *--buf_ptr = (val % 10) + '0';
	    val /= 10;
	}
	// Use more efficient (int) arithmetic for the rest.
	register unsigned int ival = (unsigned int)val;
#else
	register unsigned LONGEST ival = val;
#endif
	do {
	    *--buf_ptr = (ival % 10) + '0';
	    ival /= 10;
	} while (ival != 0);
	if (sign > 0 && (stream.flags() & ios::showpos))
	    show_pos=1;
    }

    int buf_len = buf+WRITE_BUF_SIZE - buf_ptr;
    int w = stream.width(0);

    // Calculate padding.
    int len = buf_len+show_pos;
    if (sign < 0) len++;
    len += show_base_len;
    int padding = len > w ? 0 : w - len;

    // Do actual output.
    register streambuf* sbuf = stream.rdbuf();
    ios::fmtflags pad_kind =
	stream.flags() & (ios::left|ios::right|ios::internal);
    char fill_char = stream.fill();
    if (padding > 0
	&& pad_kind != (ios::fmtflags)ios::left
	&& pad_kind != (ios::fmtflags)ios::internal) // Default (right) adjust.
	if (_IO_padn(sbuf, fill_char, padding) < padding)
	  goto failed;
    if (sign < 0 || show_pos)
      {
	char ch = sign < 0 ? '-' : '+';
	if (sbuf->sputc(ch) < 0)
	  goto failed;
      }
    if (show_base_len)
	if (_IO_sputn(sbuf, show_base, show_base_len) <= 0)
	  goto failed;
    if (pad_kind == (ios::fmtflags)ios::internal && padding > 0)
      if (_IO_padn(sbuf, fill_char, padding) < padding)
	goto failed;
    if (_IO_sputn (sbuf, buf_ptr, buf_len) != buf_len)
      goto failed;
    if (pad_kind == (ios::fmtflags)ios::left && padding > 0) // Left adjustment
      if (_IO_padn(sbuf, fill_char, padding) < padding)
	goto failed;
    stream.osfx();
    return;
  failed:
    stream.set(ios::badbit);
    stream.osfx();
}

ostream& ostream::operator<<(int n)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	int sign = 1;
	unsigned int abs_n = (unsigned)n;
	if (n < 0 && (flags() & (ios::oct|ios::hex)) == 0)
	    abs_n = -((unsigned)n), sign = -1;
	write_int(*this, abs_n, sign);
	_IO_cleanup_region_end (0);
    }
    return *this;
}

ostream& ostream::operator<<(unsigned int n)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	write_int(*this, n, 0);
	_IO_cleanup_region_end (0);
    }
    return *this;
}


ostream& ostream::operator<<(long n)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	int sign = 1;
	unsigned long abs_n = (unsigned long)n;
	if (n < 0 && (flags() & (ios::oct|ios::hex)) == 0)
	    abs_n = -((unsigned long)n), sign = -1;
	write_int(*this, abs_n, sign);
	_IO_cleanup_region_end (0);
    }
    return *this;
}

ostream& ostream::operator<<(unsigned long n)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	write_int(*this, n, 0);
	_IO_cleanup_region_end (0);
    }
    return *this;
}

#if defined(__GNUC__) && !defined(__STRICT_ANSI__)
ostream& ostream::operator<<(long long n)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	int sign = 1;
	unsigned long long abs_n = (unsigned long long)n;
	if (n < 0 && (flags() & (ios::oct|ios::hex)) == 0)
	    abs_n = -((unsigned long long)n), sign = -1;
	write_int(*this, abs_n, sign);
	_IO_cleanup_region_end (0);
    }
    return *this;
}


ostream& ostream::operator<<(unsigned long long n)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	write_int(*this, n, 0);
	_IO_cleanup_region_end (0);
    }
    return *this;
}
#endif /*__GNUC__*/

ostream& ostream::operator<<(double n)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	// Uses __cvt_double (renamed from static cvt), in Chris Torek's
	// stdio implementation.  The setup code uses the same logic
	// as in __vsbprintf.C (also based on Torek's code).
	int format_char;
	if ((flags() & ios::floatfield) == ios::fixed)
	    format_char = 'f';
	else if ((flags() & ios::floatfield) == ios::scientific)
	    format_char = flags() & ios::uppercase ? 'E' : 'e';
	else
	    format_char = flags() & ios::uppercase ? 'G' : 'g';

	int prec = precision();
	if (prec <= 0 && !(flags() & ios::fixed))
	  prec = 6; /* default */

	// Do actual conversion.
#if _G_HAVE_PRINTF_FP
	{
	  struct printf_info info = { /* prec: */ prec,
				      /* width: */ width(0),
				      /* spec: */ format_char,
				      /* is_long_double: */ 0,
				      /* is_short: */ 0,
				      /* is_long: */ 0,
				      /* alt: */ (flags() & ios::showpoint) != 0,
				      /* space: */ 0,
				      /* left: */ (flags() & ios::left) != 0,
				      /* showsign: */ (flags() & ios::showpos) != 0,
				      /* group: */ 0,
#if defined __GLIBC__ && __GLIBC__ >= 2
				      /* extra: */ 0,
#if __GLIBC_MINOR__ >= 1
				      /* is_char: */ 0,
#if __GLIBC_MINOR__ >= 2
				      /* wide: */ 0,
				      /* i18n: */ 0,
#endif
#endif
#endif
				      /* pad: */ fill()
	  };
	  const void *ptr = (const void *) &n;
	  if (__printf_fp (rdbuf(), &info, &ptr) < 0)
	    set(ios::badbit|ios::failbit);
	}
#elif defined  _IO_USE_DTOA
	if (_IO_outfloat(n, rdbuf(), format_char, width(0),
			 prec, flags(),
			 flags() & ios::showpos ? '+' : 0,
			 fill()) < 0)
	    set(ios::badbit|ios::failbit); // ??
#else
	int fpprec = 0; // 'Extra' (suppressed) floating precision.
	if (prec > MAXFRACT) {
	    if (flags() & (ios::fixed|ios::scientific) & ios::showpos)
		fpprec = prec - MAXFRACT;
	    prec = MAXFRACT;
	}
	int negative;
	char buf[BUF];
	int sign = '\0';
	char *cp = buf;
	*cp = 0;
	int size = __cvt_double(n, prec,
				flags() & ios::showpoint ? 0x80 : 0,
				&negative,
				format_char, cp, buf + sizeof(buf));
	if (negative) sign = '-';
	else if (flags() & ios::showpos) sign = '+';
	if (*cp == 0)
	    cp++;

	// Calculate padding.
	int fieldsize = size + fpprec;
	if (sign) fieldsize++;
	int padding = 0;
	int w = width(0);
	if (fieldsize < w)
	    padding = w - fieldsize;

	// Do actual output.
	register streambuf* sbuf = rdbuf();
	register i;
	char fill_char = fill();
	ios::fmtflags pad_kind =
	    flags() & (ios::left|ios::right|ios::internal);
	if (pad_kind != (ios::fmtflags)ios::left // Default (right) adjust.
	    && pad_kind != (ios::fmtflags)ios::internal)
	    for (i = padding; --i >= 0; ) sbuf->sputc(fill_char);
	if (sign)
	    sbuf->sputc(sign);
	if (pad_kind == (ios::fmtflags)ios::internal)
	    for (i = padding; --i >= 0; ) sbuf->sputc(fill_char);

	// Emit the actual concented field, followed by extra zeros.
	_IO_sputn (sbuf, cp, size);
	for (i = fpprec; --i >= 0; ) sbuf->sputc('0');

	if (pad_kind == (ios::fmtflags)ios::left) // Left adjustment
	    for (i = padding; --i >= 0; ) sbuf->sputc(fill_char);
#endif
	osfx();
	_IO_cleanup_region_end (0);
    }
    return *this;
}

#if _G_HAVE_LONG_DOUBLE_IO
ostream& ostream::operator<<(long double n)
{
  if (opfx())
    {
      _IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				_strbuf);
      int format_char;
      if ((flags() & ios::floatfield) == ios::fixed)
	format_char = 'f';
      else if ((flags() & ios::floatfield) == ios::scientific)
	format_char = flags() & ios::uppercase ? 'E' : 'e';
      else
	format_char = flags() & ios::uppercase ? 'G' : 'g';

      int prec = precision();
      if (prec <= 0 && !(flags() & ios::fixed))
	prec = 6; /* default */

#if _G_HAVE_PRINTF_FP
      // Do actual conversion.
      struct printf_info info = { /* prec: */ prec,
				  /* width: */ width(0),
				  /* spec: */ format_char,
			          /* is_long_double: */ 1,
				  /* is_short: */ 0,
				  /* is_long: */ 0,
				  /* alt: */ (flags() & ios::showpoint) != 0,
				  /* space: */ 0,
				  /* left: */ (flags() & ios::left) != 0,
				  /* showsign: */ (flags() & ios::showpos) != 0,
				  /* group: */ 0,
#if defined __GLIBC__ && __GLIBC__ >= 2
				  /* extra: */ 0,
#if __GLIBC_MINOR__ >= 1
				  /* is_char: */ 0,
#if __GLIBC_MINOR__ >= 2
				  /* wide: */ 0,
				  /* i18n: */ 0,
#endif
#endif
#endif
				  /* pad: */ fill()
      };

      const void *ptr = (const void *) &n;

      if (__printf_fp (rdbuf(), &info, &ptr) < 0)
	set (ios::badbit|ios::failbit);
#else
# error "long double I/O using dtoa or cvt_double is not implemented"
#endif
      osfx();
      _IO_cleanup_region_end (0);
    }
  return *this;
}
#endif

ostream& ostream::operator<<(const char *s)
{
  if (opfx())
    {
      _IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				_strbuf);
      if (s == NULL)
	s = "(null)";
      int len = strlen(s);
      int w = width(0);
// FIXME: Should we: if (w && len>w) len = w;
      char fill_char = fill();
      register streambuf *sbuf = rdbuf();
      register int padding = w > len ? w - len : 0;
      if (!(flags() & ios::left) && padding > 0) // Default adjustment.
	if (_IO_padn(sbuf, fill_char, padding) != padding)
	  {
	    set(ios::badbit);
	    goto failed;
	  }
      if (_IO_sputn (sbuf, s, len) != len)
	{
	  set(ios::badbit);
	  goto failed;
	}
      if (flags() & ios::left && padding > 0) // Left adjustment.
	if (_IO_padn(sbuf, fill_char, padding) != padding)
	  set(ios::badbit);
     failed:
      osfx();
      _IO_cleanup_region_end (0);
    }
  return *this;
}

#if 0
ostream& ostream::operator<<(const void *p)
{ Is in osform.cc, to avoid pulling in all of _IO_vfprintf by this file. */ }
#endif

ostream& ostream::operator<<(register streambuf* sbuf)
{
  if (opfx())
    {
      _IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				_strbuf);
      char buffer[_IO_BUFSIZ];
      register streambuf* outbuf = _strbuf;
      for (;;)
	{
	  _IO_size_t count = _IO_sgetn(sbuf, buffer, _IO_BUFSIZ);
	  if (count <= 0)
	    break;
	  if (_IO_sputn(outbuf, buffer, count) != count)
	    {
	      set(ios::badbit);
	      break;
	    }
	}
      osfx();
      _IO_cleanup_region_end (0);
    }
  return *this;
}

ostream::ostream(streambuf* sb, ostream* tied)
{
  init (sb, tied);
}

ostream& ostream::seekp(streampos pos)
{
    pos = _strbuf->pubseekpos(pos, ios::out);
    if (pos == streampos(EOF))
	set(ios::badbit);
    return *this;
}

ostream& ostream::seekp(streamoff off, _seek_dir dir)
{
  streampos pos = _IO_seekoff (_strbuf, off, (int) dir, _IOS_OUTPUT);
  if (pos == streampos(EOF))
    set(ios::badbit);
  return *this;
}

streampos ostream::tellp()
{
#if 1
    streampos pos = _IO_seekoff (_strbuf, 0, _IO_seek_cur, _IOS_OUTPUT);
#else
    streampos pos = _strbuf->pubseekoff(0, ios::cur, ios::out);
#endif
    if (pos == streampos(EOF))
	set(ios::badbit);
    return pos;
}

ostream& ostream::flush()
{
    if (_strbuf->sync())
	set(ios::badbit);
    return *this;
}

ostream& flush(ostream& outs)
{
  return outs.flush();
}

istream& ws(istream& ins)
{
    if (ins.ipfx1()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  ins._strbuf);
	int ch = skip_ws(ins._strbuf);
	if (ch == EOF)
	    ins.set(ios::eofbit);
	else
	    ins._strbuf->sputbackc(ch);
	ins.isfx();
	_IO_cleanup_region_end (0);
    }
    return ins;
}

// Skip white-space.  Return 0 on failure (EOF), or 1 on success.
// Differs from ws() manipulator in that failbit is set on EOF.
// Called by ipfx() and ipfx0() if needed.

int istream::_skip_ws()
{
    int ch = skip_ws(_strbuf);
    if (ch == EOF) {
	set(ios::eofbit|ios::failbit);
	return 0;
    }
    else {
	_strbuf->sputbackc(ch);
	return 1;
    }
}

ostream& ends(ostream& outs)
{
    outs.put('\0');
    return outs;
}

ostream& endl(ostream& outs)
{
    return flush(outs.put('\n'));
}

istream& lock(istream& ins)
{
  _IO_flockfile (ins._strbuf);
  return ins;
}
istream& unlock(istream& ins)
{
  _IO_funlockfile (ins._strbuf);
  return ins;
}
ostream& lock(ostream& outs)
{
  _IO_flockfile (outs._strbuf);
  return outs;
}
ostream& unlock(ostream& outs)
{
  _IO_funlockfile (outs._strbuf);
  return outs;
}


ostream& ostream::write(const char *s, streamsize n)
{
    if (opfx()) {
	_IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile,
				  _strbuf);
	if (_IO_sputn(_strbuf, s, n) != n)
	    set(ios::failbit);
	osfx();
	_IO_cleanup_region_end (0);
    }
    return *this;
}

void ostream::do_osfx()
{
    if (flags() & ios::unitbuf)
	flush();
    if (flags() & ios::stdio) {
	fflush(stdout);
	fflush(stderr);
    }
}

iostream::iostream(streambuf* sb, ostream* tied)
{
  init (sb, tied);
}

// NOTE: extension for compatibility with old libg++.
// Not really compatible with fistream::close().
#ifdef _STREAM_COMPAT
void ios::close()
{
  if (_strbuf->_flags & _IO_IS_FILEBUF)
    ((struct filebuf*)rdbuf())->close();
  else if (_strbuf != NULL)
    rdbuf()->sync();
  _strbuf = NULL;
  _state = badbit;
}

int istream::skip(int i)
{
    int old = (_flags & ios::skipws) != 0;
    if (i)
	_flags |= ios::skipws;
    else
	_flags &= ~ios::skipws;
    return old;
}
#endif
