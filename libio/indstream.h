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

#ifndef _INDSTREAM_H
#define _INDSTREAM_H

#ifdef __GNUG__
#pragma interface
#endif

#include <iostream.h>

extern "C++" {
// An indirectbuf is one that forwards all of its I/O requests
// to another streambuf.
// All get-related requests are sent to get_stream().
// All put-related requests are sent to put_stream().

// An indirectbuf can be used to implement Common Lisp
// synonym-streams and two-way-streams.
//
// class synonymbuf : public indirectbuf {
//    Symbol *sym;
//    synonymbuf(Symbol *s) { sym = s; }
//    virtual streambuf *lookup_stream(int mode) {
//        return coerce_to_streambuf(lookup_value(sym)); }
// };

class indirectbuf : public streambuf {
  protected:
    streambuf *_get_stream;  // Optional cache for get_stream().
    streambuf *_put_stream;  // Optional cache for put_stream().
    int _delete_flags;
  public:
    streambuf *get_stream()
	{ return _get_stream ? _get_stream : lookup_stream(ios::in); }
    streambuf *put_stream()
	{ return _put_stream ? _put_stream : lookup_stream(ios::out); }
    virtual streambuf *lookup_stream(int/*mode*/) { return NULL; } // ERROR!
    indirectbuf(streambuf *get=NULL, streambuf *put=NULL, int delete_mode=0);
    virtual ~indirectbuf();
    virtual streamsize xsputn(const char* s, streamsize n);
    virtual streamsize xsgetn(char* s, streamsize n);
    virtual int underflow();
    virtual int uflow();
    virtual int overflow(int c = EOF);
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
    virtual streampos seekpos(streampos pos, int mode = ios::in|ios::out);
    virtual int sync();
    virtual int pbackfail(int c);
};
} // extern "C++"

#endif /* !_INDSTREAM_H */
