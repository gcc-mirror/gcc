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

#ifndef _STDIOSTREAM_H
#define _STDIOSTREAM_H

#ifdef __GNUG__
#pragma interface
#endif

#include <iostream.h>
#include <stdio.h>

extern "C++" {
class stdiobuf : public filebuf {
  protected:
    FILE *_file;
  public:
    FILE* stdiofile() const { return _file; }
    stdiobuf(FILE *);
    ~stdiobuf();
    int buffered () const { return _flags & _IO_UNBUFFERED ? 0 : 1; }
    void buffered (int);
    virtual streamsize sys_read(char*, streamsize);
    virtual streampos sys_seek(streamoff, _seek_dir);
    virtual streamsize sys_write(const char*, streamsize);
    virtual int sys_close();
    virtual int sync();
    virtual int overflow(int c = EOF);
    streamsize xsputn(const char* s, streamsize n);
};

class istdiostream : public istream
{
private:
  stdiobuf _file;
public:
  istdiostream (FILE* __f) : istream(), _file(__f) { init(&_file); }
  stdiobuf* rdbuf()/* const */ { return &_file; }
  int buffered () const { return _file.buffered (); }
  void buffered (int _i) { _file.buffered (_i); }
};

class ostdiostream : public ostream
{
private:
  stdiobuf _file;
public:
  ostdiostream (FILE* __f) : ostream(), _file(__f) { init(&_file); }
  stdiobuf* rdbuf() /* const */ { return &_file; }
  int buffered () const { return _file.buffered (); }
  void buffered (int _i) { _file.buffered (_i); }
};
} // extern "C++"

#endif /* !_STDIOSTREAM_H */
