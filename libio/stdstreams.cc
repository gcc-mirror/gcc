/* This is part of libio/iostream, providing -*- C++ -*- input/output.
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

/* Written by Per Bothner (bothner@cygnus.com). */

#include "libioP.h"
#include "streambuf.h"
#include <stdio.h>

// The ANSI draft requires that operations on cin/cout/cerr can be
// mixed with operations on stdin/stdout/stderr on a character by
// character basis.  This normally requires that the streambuf's
// used by cin/cout/cerr be stdiostreams.  However, if the stdio
// implementation is the one that is built using this library,
// then we don't need to, since in that case stdin/stdout/stderr
// are identical to _IO_stdin/_IO_stdout/_IO_stderr.

#include "libio.h"

#ifdef _STDIO_USES_IOSTREAM
#define CIN_SBUF _IO_stdin_
#define COUT_SBUF _IO_stdout_
#define CERR_SBUF _IO_stderr_
static int use_stdiobuf = 0;
#else
#define CIN_SBUF _IO_stdin_buf
#define COUT_SBUF _IO_stdout_buf
#define CERR_SBUF _IO_stderr_buf
static int use_stdiobuf = 1;
#endif

#define cin CIN
#define cout COUT
#define cerr CERR
#define clog CLOG
#include "iostream.h"
#undef cin
#undef cout
#undef cerr
#undef clog

#ifdef __GNUG__
#define PAD 0 /* g++ allows 0-length arrays. */
#else
#define PAD 1
#endif
struct _fake_istream {
    struct myfields {
#ifdef __GNUC__
	_ios_fields *vb; /* pointer to virtual base class ios */
	_IO_ssize_t _gcount;
#else
	/* This is supposedly correct for cfront. */
	_IO_ssize_t _gcount;
	void *vptr;
	_ios_fields *vb; /* pointer to virtual base class ios */
#endif
    } mine;
    _ios_fields base;
    char filler[sizeof(struct istream)-sizeof(struct _ios_fields)+PAD];
};
struct _fake_ostream {
    struct myfields {
#ifndef __GNUC__
	void *vptr;
#endif
	_ios_fields *vb; /* pointer to virtual base class ios */
    } mine;
    _ios_fields base;
    char filler[sizeof(struct ostream)-sizeof(struct _ios_fields)+PAD];
};


#ifdef _IO_NEW_STREAMS
#define STD_STR(SBUF, TIE, EXTRA_FLAGS) \
 (streambuf*)&SBUF, TIE, 0, ios::skipws|ios::dec|EXTRA_FLAGS, ' ',0,0,6
#else
#define STD_STR(SBUF, TIE, EXTRA_FLAGS) \
 (streambuf*)&SBUF, TIE, 0, ios::dont_close|ios::dec|ios::skipws|EXTRA_FLAGS, ' ',0,0,6
#endif

#ifdef __GNUC__
#define OSTREAM_DEF(NAME, SBUF, TIE, EXTRA_FLAGS, ASM) \
  _fake_ostream NAME ASM = { {&NAME.base}, {STD_STR(SBUF, TIE, EXTRA_FLAGS) }};
#define ISTREAM_DEF(NAME, SBUF, TIE, EXTRA_FLAGS) \
  _fake_istream NAME = { {&NAME.base}, {STD_STR(SBUF, TIE, EXTRA_FLAGS) }};
#else
#define OSTREAM_DEF(NAME, SBUF, TIE, EXTRA_FLAGS) \
  _fake_ostream NAME = { {0, &NAME.base}, {STD_STR(SBUF, TIE, EXTRA_FLAGS) }};
#define ISTREAM_DEF(NAME, SBUF, TIE, EXTRA_FLAGS) \
  _fake_istream NAME = {{0, 0, &NAME.base}, {STD_STR(SBUF, TIE, EXTRA_FLAGS)}};
#endif

OSTREAM_DEF(cout, COUT_SBUF, NULL, 0, )
OSTREAM_DEF(cerr, CERR_SBUF,(ostream*)&cout, ios::unitbuf, )
ISTREAM_DEF(cin, CIN_SBUF,  (ostream*)&cout, 0)

/* Only for (partial) compatibility with AT&T's library. */
#if _G_CLOG_CONFLICT
OSTREAM_DEF(clog, CERR_SBUF, (ostream*)&cout, 0, __asm__ ("__IO_clog"))
#else
OSTREAM_DEF(clog, CERR_SBUF, (ostream*)&cout, 0, )
#endif

// Switches between using _IO_std{in,out,err} and __std{in,out,err}_buf
// for standard streams.  This does not normally need to be called
// explicitly, but is provided for AT&T compatibility.

int ios::sync_with_stdio(int new_state)
{
#ifdef _STDIO_USES_IOSTREAM
    // It is always synced.
    return 0;
#else
    if (new_state == use_stdiobuf) // The usual case now.
	return use_stdiobuf;
    if (new_state) {
	cin.base._strbuf = (streambuf*)&_IO_stdin_buf;
	cout.base._strbuf = (streambuf*)&_IO_stdout_buf;
	cerr.base._strbuf = (streambuf*)&_IO_stderr_buf;
	clog.base._strbuf = (streambuf*)&_IO_stderr_buf;
    } else {
	cin.base._strbuf = (streambuf*)_IO_stdin;
	cout.base._strbuf = (streambuf*)_IO_stdout;
	cerr.base._strbuf = (streambuf*)_IO_stderr;
	clog.base._strbuf = (streambuf*)_IO_stderr;
    }
    int old_state = use_stdiobuf;
    use_stdiobuf = new_state;
    return old_state;
#endif
}
