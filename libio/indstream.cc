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

#include <indstream.h>

indirectbuf::indirectbuf(streambuf *get, streambuf *put, int delete_mode)
: streambuf()
{
    _get_stream = get;
    _put_stream = put == NULL ? get : put;
    _delete_flags = delete_mode;
}

indirectbuf::~indirectbuf()
{
    if (_delete_flags & ios::in)  delete get_stream();
    if (_delete_flags & ios::out)  delete put_stream();
}

streamsize indirectbuf::xsputn(const char* s, streamsize n)
{
    return put_stream()->sputn(s, n);
}

streamsize indirectbuf::xsgetn(char* s, streamsize n)
{
    return get_stream()->sgetn(s, n);
}

int indirectbuf::overflow(int c /* = EOF */)
{
    if (c == EOF)
	return put_stream()->overflow(c);
    else
	return put_stream()->sputc(c);
}

int indirectbuf::underflow()
{
    return get_stream()->sgetc();
}

int indirectbuf::uflow()
{
    return get_stream()->sbumpc();
}

streampos indirectbuf::seekoff(streamoff off, _seek_dir dir, int mode)
{
    int ret_val = 0;
    int select = mode == 0 ? (ios::in|ios::out) : mode;
    streambuf *gbuf = (select & ios::in) ? get_stream() : (streambuf*)NULL;
    streambuf *pbuf = (select & ios::out) ? put_stream() : (streambuf*)NULL;
    if (gbuf == pbuf)
	ret_val = gbuf->seekoff(off, dir, mode);
    else {
	if (gbuf)
	    ret_val = gbuf->seekoff(off, dir, ios::in);
	if (pbuf && ret_val != EOF)
	    ret_val = pbuf->seekoff(off, dir, ios::out);
    }
    return ret_val;
}

streampos indirectbuf::seekpos(streampos pos, int mode)
{
    int ret_val = EOF;
    int select = mode == 0 ? (ios::in|ios::out) : mode;
    streambuf *gbuf = (select & ios::in) ? get_stream() : (streambuf*)NULL;
    streambuf *pbuf = (select & ios::out) ? put_stream() : (streambuf*)NULL;
    if (gbuf == pbuf && gbuf != NULL)
	ret_val = gbuf->seekpos(pos, mode);
    else {
	if (gbuf)
	    ret_val = gbuf->seekpos(pos, ios::in);
	if (pbuf && ret_val != EOF)
	    ret_val = pbuf->seekpos(pos, ios::out);
    }
    return ret_val;
}

int indirectbuf::sync()
{
  streambuf *gbuf = get_stream();
  int get_ret_val = gbuf ? gbuf->sync() : 0;
  streambuf *pbuf = put_stream();
  int put_ret_val = (pbuf && pbuf != gbuf) ?  pbuf->sync() : 0;
  return get_ret_val || put_ret_val;
}

int indirectbuf::pbackfail(int c)
{
    return get_stream()->sputbackc(c);
}
