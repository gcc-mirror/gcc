/* This is part of libio/iostream, providing -*- C++ -*- input/output.
Copyright (C) 1991, 1992, 1993, 1995 Free Software Foundation

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

#define _STREAM_COMPAT
#ifdef __GNUG__
#pragma implementation
#endif
#include "iostreamP.h"
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#ifndef errno
extern int errno;
#endif

void streambuf::_un_link() { _IO_un_link(this); }

void streambuf::_link_in() { _IO_link_in(this); }

int streambuf::switch_to_get_mode()
{ return _IO_switch_to_get_mode(this); }

void streambuf::free_backup_area()
{ _IO_free_backup_area(this); }

#if 0
int streambuf::switch_to_put_mode()
{ return _IO_:switch_to_put_mode(this); }
#endif

int __overflow(streambuf* sb, int c)
{
    return sb->overflow(c);
}

int streambuf::underflow()
{ return EOF; }

int streambuf::uflow()
{ return _IO_default_uflow (this); }

int streambuf::overflow(int /* = EOF */)
{ return EOF; }

streamsize streambuf::xsputn(register const char* s, streamsize n)
{ return _IO_default_xsputn(this, s, n); }

streamsize streambuf::xsgetn(char* s, streamsize n)
{ return _IO_default_xsgetn(this, s, n); }

int streambuf::ignore(int n)
{
    register int more = n;
    for (;;) {
	int count = _IO_read_end - _IO_read_ptr; // Data available.
	if (count > 0) {
	    if (count > more)
		count = more;
	    _IO_read_ptr += count;
	    more -= count;
	}
	if (more == 0 || __underflow(this) == EOF)
	    break;
    }
    return n - more;
}

int streambuf::sync()
{
  return 0;
}

int streambuf::pbackfail(int c)
{
  return _IO_default_pbackfail(this, c);
}

streambuf* streambuf::setbuf(char* p, int len)
{
    if (sync() == EOF)
	return NULL;
    if (p == NULL || len == 0) {
	unbuffered(1);
	setb(_shortbuf, _shortbuf+1, 0);
    }
    else {
	unbuffered(0);
	setb(p, p+len, 0);
    }
    setp(0, 0);
    setg(0, 0, 0);
    return this;
}

streampos streambuf::seekpos(streampos pos, int mode)
{
    return seekoff(pos, ios::beg, mode);
}

streampos streambuf::sseekpos(streampos pos, int mode)
{
  return _IO_seekpos (this, pos, mode);
}

void streambuf::setb(char* b, char* eb, int a)
{ _IO_setb(this, b, eb, a); }

int streambuf::doallocate() { return _IO_default_doallocate(this); }

void streambuf::doallocbuf() { _IO_doallocbuf(this); }

#if !_IO_UNIFIED_JUMPTABLES
/* The following are jump table entries that just call the virtual method */

static int _IO_sb_overflow(_IO_FILE *fp, int c)
{ return ((streambuf*)fp)->overflow(c); }
static int _IO_sb_underflow(_IO_FILE *fp)
{ return ((streambuf*)fp)->underflow(); }
static _IO_size_t _IO_sb_xsputn(_IO_FILE *fp, const void *s, _IO_size_t n)
{ return ((streambuf*)fp)->xsputn((const char*)s, n); }
static _IO_size_t _IO_sb_xsgetn(_IO_FILE *fp, void *s, _IO_size_t n)
{ return ((streambuf*)fp)->xsgetn((char*)s, n); }
static int _IO_sb_close(_IO_FILE *fp)
{ return ((streambuf*)fp)->sys_close(); }
static int _IO_sb_stat(_IO_FILE *fp, void *b)
{ return ((streambuf*)fp)->sys_stat(b); }
static int _IO_sb_doallocate(_IO_FILE *fp)
{ return ((streambuf*)fp)->doallocate(); }

static _IO_pos_t _IO_sb_seekoff(_IO_FILE *fp, _IO_off_t pos, int dir, int mode)
{
  return ((streambuf*)fp)->seekoff(pos, (ios::seek_dir)dir, mode);
}

static _IO_pos_t _IO_sb_seekpos(_IO_FILE *fp, _IO_pos_t pos, int mode)
{
  return ((streambuf*)fp)->seekpos(pos, mode);
}

static int _IO_sb_pbackfail(_IO_FILE *fp, int ch)
{ return ((streambuf*)fp)->pbackfail(ch); }
static void _IO_sb_finish(_IO_FILE *fp)
{ ((streambuf*)fp)->~streambuf(); }
static _IO_ssize_t _IO_sb_read(_IO_FILE *fp, void *buf, _IO_ssize_t n)
{ return ((streambuf*)fp)->sys_read((char*)buf, n); }
static _IO_ssize_t _IO_sb_write(_IO_FILE *fp, const void *buf, _IO_ssize_t n)
{ return ((streambuf*)fp)->sys_write((const char*)buf, n); }
static int _IO_sb_sync(_IO_FILE *fp)
{ return ((streambuf*)fp)->sync(); }
static _IO_pos_t _IO_sb_seek(_IO_FILE *fp, _IO_off_t off, int dir)
{ return ((streambuf*)fp)->sys_seek(off, (_seek_dir)dir); }
static _IO_FILE* _IO_sb_setbuf(_IO_FILE *fp, char *buf, _IO_ssize_t n)
{ return ((streambuf*)fp)->setbuf(buf, n); }

/* This callbacks in this jumptable just call the corresponding
   virtual function, so that C functions can access (potentially user-defined)
   streambuf-derived objects.
   Contrast the builtinbuf class, which does the converse:  Allow
   C++ virtual calls to to be used on _IO_FILE objects that are builtin
   (or defined by C code). */


struct _IO_jump_t _IO_streambuf_jumps = {
  JUMP_INIT_DUMMY,
  JUMP_INIT(finish, _IO_sb_finish),
  JUMP_INIT(overflow, _IO_sb_overflow),
  JUMP_INIT(underflow, _IO_sb_underflow),
  JUMP_INIT(uflow, _IO_default_uflow),
  JUMP_INIT(pbackfail, _IO_sb_pbackfail),
  JUMP_INIT(xsputn, _IO_sb_xsputn),
  JUMP_INIT(xsgetn, _IO_sb_xsgetn),
  JUMP_INIT(seekoff, _IO_sb_seekoff),
  JUMP_INIT(seekpos, _IO_sb_seekpos),
  JUMP_INIT(setbuf, _IO_sb_setbuf),
  JUMP_INIT(sync, _IO_sb_sync),
  JUMP_INIT(doallocate, _IO_sb_doallocate),
  JUMP_INIT(read, _IO_sb_read),
  JUMP_INIT(write, _IO_sb_write),
  JUMP_INIT(seek, _IO_sb_seek),
  JUMP_INIT(close, _IO_sb_close),
  JUMP_INIT(stat, _IO_sb_stat)
};
#endif

streambuf::streambuf(int flags)
{
  _IO_init(this, flags);
#if !_IO_UNIFIED_JUMPTABLES
  _jumps = &_IO_streambuf_jumps;
#endif
}

streambuf::~streambuf() { _IO_default_finish(this); }

streampos
streambuf::seekoff(streamoff, _seek_dir, int /*=ios::in|ios::out*/)
{
    return EOF;
}

streampos
streambuf::sseekoff(streamoff o , _seek_dir d, int m /*=ios::in|ios::out*/)
{
  return _IO_seekoff (this, o, d, m);
}

int streambuf::sputbackc(char c)
{
  return _IO_sputbackc(this, c);
}

int streambuf::sungetc()
{
  return _IO_sungetc(this);
}

#if 0 /* Work in progress */
void streambuf::collumn(int c)
{
    if (c == -1)
	_collumn = -1;
    else
	_collumn = c - (_IO_write_ptr - _IO_write_base);
}
#endif


int streambuf::get_column()
{
    if (_cur_column) 
	return _IO_adjust_column(_cur_column - 1, pbase(), pptr() - pbase());
    return -1;
}

int streambuf::set_column(int i)
{
    _cur_column = i+1;
    return 0;
}

int streambuf::flush_all() { return _IO_flush_all (); }

void streambuf::flush_all_linebuffered()
{ _IO_flush_all_linebuffered(); }

int streambuf::sys_stat(void *)
{
#ifdef EIO
  errno = EIO;
#endif
  return -1;
}

streamsize streambuf::sys_read(char* /*buf*/, streamsize /*size*/)
{
  return 0;
}

streamsize streambuf::sys_write(const char* /*buf*/, streamsize /*size*/)
{
  return 0;
}

streampos streambuf::sys_seek(streamoff, _seek_dir)
{
  return EOF;
}

int streambuf::sys_close() { return 0; /* Suceess; do nothing */ }

streammarker::streammarker(streambuf *sb)
{
  _IO_init_marker(this, sb);
}

streammarker::~streammarker()
{
  _IO_remove_marker(this);
}

#define BAD_DELTA EOF

int streammarker::delta(streammarker& other_mark)
{
  return _IO_marker_difference(this, &other_mark);
}

int streammarker::delta()
{
  return _IO_marker_delta(this);
}

int streambuf::seekmark(streammarker& mark, int delta /* = 0 */)
{
  return _IO_seekmark(this, &mark, delta);
}

void streambuf::unsave_markers()
{
  _IO_unsave_markers(this);
}

int ios::readable() { return !(rdbuf()->_flags & _IO_NO_READS); }
int ios::writable() { return !(rdbuf()->_flags & _IO_NO_WRITES); }
int ios::is_open() { return rdbuf()
			 && (rdbuf()->_flags & _IO_NO_READS+_IO_NO_WRITES)
			     != _IO_NO_READS+_IO_NO_WRITES; }

#if defined(linux)
#define IO_CLEANUP
#endif

#ifdef IO_CLEANUP
  IO_CLEANUP
#else
struct __io_defs {
    ~__io_defs() { _IO_cleanup (); }
};   
__io_defs io_defs__;
#endif
