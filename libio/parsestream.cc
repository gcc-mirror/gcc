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
the executable file might be covered by the GNU General Public License.

Written by Per Bothner (bothner@cygnus.com). */

#ifdef __GNUG__
#pragma implementation
#endif
#include "libioP.h"
#include "parsestream.h"
#include <stdlib.h>

streambuf* parsebuf::setbuf(char*, int)
{
    return NULL;
}

int parsebuf::tell_in_line()
{
    return 0;
}

int parsebuf::pbackfail(int c)
{
    if (c == EOF)
	return 0;
    if (seekoff(-1, ios::cur) == EOF)
	return EOF;
    return (unsigned char)c;
}

char* parsebuf::current_line() { return NULL; }

streampos parsebuf::seekoff(streamoff offset, _seek_dir dir, int)
{
    // Make offset relative to line start.
    switch (dir) {
      case ios::beg:
	offset -= pos_at_line_start;
	break;
      case ios::cur:
	offset += tell_in_line();
	break;
      default:
	return EOF;
    }
    if (offset < -1)
	return EOF;
    if (offset > _line_length + 1)
	return EOF;
    return seek_in_line(offset) + pos_at_line_start;
}

// string_parsebuf invariants:
// The reserve ares (base() .. ebuf()) is always the entire string.
// The get area (eback() .. egptr()) is the extended current line
// (i.e. with the '\n' at either end, if these exist).

string_parsebuf::string_parsebuf(char *buf, int len,
				 int delete_at_close /* = 0*/)
: parsebuf()
{
    setb(buf, buf+len, delete_at_close);
    register char *ptr = buf;
    while (ptr < ebuf() && *ptr != '\n') ptr++;
    _line_length = ptr - buf;
    setg(buf, buf, ptr);
}

int string_parsebuf::underflow()
{
    register char* ptr = egptr(); // Point to end of current_line
    do {
	int i = right() - ptr;
	if (i <= 0)
	    return EOF;
	ptr++; i--; // Skip '\n'.
	char *line_start = ptr;
	while (ptr < right() && *ptr == '\n') ptr++;
	setg(line_start-1, line_start, ptr + (ptr < right()));
	pos_at_line_start = line_start - left();
	_line_length = ptr - line_start;
	__line_number++;
    } while (gptr() == ptr);
    return *gptr();
}

char* string_parsebuf::current_line()
{
    char *ptr = eback();
    if (__line_number > 0)
	ptr++; // Skip '\n' at end of previous line.
    return ptr;
}

int string_parsebuf::tell_in_line()
{
    int offset = gptr() - eback();
    if (__line_number > 0)
	offset--;
    return offset;
}

int string_parsebuf::seek_in_line(int i)
{
    int delta = i - tell_in_line();
    gbump(delta); // FIXME: Needs error (bounds) checking!
    return i;
}

static const char NewLine[1] = { '\n' };

general_parsebuf::general_parsebuf(streambuf *buf, int delete_arg_buf)
 : parsebuf()
{
    delete_buf = delete_arg_buf;
    sbuf = buf;
    int buf_size = 128;
    char* buffer = (char*)malloc(buf_size);
    setb(buffer, buffer+buf_size, 1);
//    setg(buffer, buffer, buffer);
}

general_parsebuf::~general_parsebuf()
{
    if (delete_buf)
	delete sbuf;
}

int general_parsebuf::underflow()
{
    register char *ptr = base();
    int has_newline = eback() < gptr() && gptr()[-1] == '\n';
    if (has_newline)
	*ptr++ = '\n';
    register streambuf *sb = sbuf;
    register int ch;
    for (;;) {
	ch = sb->sbumpc();
	if (ch == EOF)
	    break;
	if (ptr == ebuf()) {
	    int old_size = ebuf() - base();
	    char *new_buffer = new char[old_size * 2];
	    memcpy(new_buffer, base(), old_size);
	    setb(new_buffer, new_buffer + 2 * old_size, 1);
	    ptr = new_buffer + old_size;
	}
	*ptr++ = ch;
	if (ch == '\n')
	    break;
    }
    char *cur_pos = base() + has_newline;
    pos_at_line_start += _line_length + 1;
    _line_length = ptr - cur_pos;
    if (ch != EOF || _line_length > 0)
	__line_number++;
    setg(base(), cur_pos, ptr);
    return ptr == cur_pos ? EOF : cur_pos[0];
}

char* general_parsebuf::current_line()
{
    char* ret = base();
    if (__line_number > 1)
	ret++; // Move past '\n' from end of previous line.
    return ret;
}

int general_parsebuf::tell_in_line()
{
    int off = gptr() - base();
    if (__line_number > 1)
	off--; // Subtract 1 for '\n' from end of previous line.
    return off;
}

int general_parsebuf::seek_in_line(int i)
{
    if (__line_number == 0)
	(void)general_parsebuf::underflow();
    if (__line_number > 1)
	i++; // Add 1 for '\n' from end of previous line.
    if (i < 0) i = 0;
    int len = egptr() - eback();
    if (i > len) i = len;
    setg(base(), base() + i, egptr());
    return i;
}

func_parsebuf::func_parsebuf(CharReader func, void *argm) : parsebuf()
{
    read_func = func;
    arg = argm;
    buf_start = NULL;
    buf_end = NULL;
    setb((char*)NewLine, (char*)NewLine+1, 0);
    setg((char*)NewLine, (char*)NewLine+1, (char*)NewLine+1);
    backed_up_to_newline = 0;
}

int func_parsebuf::tell_in_line()
{
    if (buf_start == NULL)
	return 0;
    if (egptr() != (char*)NewLine+1)
	// Get buffer was line buffer.
	return gptr() - buf_start;
    if (backed_up_to_newline)
	return -1;  // Get buffer is '\n' preceding current line.
    // Get buffer is '\n' following current line.
    return (buf_end - buf_start) + (gptr() - (char*)NewLine);
}

char* func_parsebuf::current_line()
{
    return buf_start;
}

int func_parsebuf::seek_in_line(int i)
{
    if (i < 0) {
	// Back up to preceding '\n'.
	if (i < -1) i = -1;
	backed_up_to_newline = 1;
	setg((char*)NewLine, (char*)NewLine+(i+1), (char*)NewLine+1);
	return i;
    }
    backed_up_to_newline = 0;
    int line_length = buf_end-buf_start;
    if (i <= line_length) {
	setg(buf_start, buf_start+i, buf_end);
	return i;
    }
    i -= line_length;
    if (i > 0) i = 1;
    setg((char*)NewLine, (char*)NewLine+i, (char*)NewLine+1);
    return line_length + i;
}

int func_parsebuf::underflow()
{
  retry:
    if (gptr() < egptr())
	return *gptr();
    if (gptr() != (char*)NewLine+1) {
	// Get buffer was line buffer.  Move to following '\n'.
	setg((char*)NewLine, (char*)NewLine, (char*)NewLine+1);
	return *gptr();
    }
    if (backed_up_to_newline)
	// Get buffer was '\n' preceding current line. Move to current line.
	backed_up_to_newline = 0;
    else {
	// Get buffer was '\n' following current line. Read new line.
	if (buf_start) free(buf_start);
	char *str = (*read_func)(arg);
	buf_start = str;
	if (str == NULL)
	    return EOF;
	// Initially, _line_length == -1, so pos_at_line_start becomes 0.
	pos_at_line_start += _line_length + 1;
	_line_length = strlen(str);
	buf_end = str + _line_length;
	__line_number++;
    }
    setg(buf_start, buf_start, buf_end);
    goto retry;
}

#if 0
size_t parsebuf::line_length()
{
    if (current_line_length == (size_t)(-1)) // Initial value;
	(void)sgetc();
    return current_line_length;
}
#endif

int parsebuf::seek_in_line(int i)
{
#if 1
    abort();
    return i; // Suppress warnings.
#else
    if (i > 0) {
	size_t len = line_length();
	if ((unsigned)i > len) i = len;
    }
    else if (i < -1) i = -1;
    int new_pos = seekoff(pos_at_line_start + i, ios::beg);
    if (new_pos == EOF)
	return tell_in_line();
    else return new_pos - pos_at_line_start;
#endif
}
