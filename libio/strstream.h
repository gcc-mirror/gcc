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

#ifndef __STRSTREAM_H
#define __STRSTREAM_H
#ifdef __GNUG__
#pragma interface
#endif
#include <iostream.h>
#include <strfile.h>

extern "C++" {
class strstreambuf : public streambuf
{
  struct _IO_str_fields _s;
  friend class istrstream;

    void init_dynamic(_IO_alloc_type alloc, _IO_free_type free,
		      int initial_size = 0);
    void init_static(char *ptr, int size, char *pstart);
    void init_readonly(const char *ptr, int size);
  protected:
    virtual int overflow(int = EOF);
    virtual int underflow();
    virtual int pbackfail(int c);
  public:
    virtual ~strstreambuf();
    strstreambuf() { init_dynamic(0, 0); }
    strstreambuf(int initial_size) { init_dynamic(0, 0, initial_size); }
    strstreambuf(void *(*alloc)(_IO_size_t), void (*free)(void*))
	{ init_dynamic(alloc, free); }
    strstreambuf(char *ptr, int size, char *pstart = NULL)
	{ init_static(ptr, size, pstart); }
    strstreambuf(unsigned char *ptr, int size, unsigned char *pstart = NULL)
	{ init_static((char*)ptr, size, (char*)pstart); }
    strstreambuf(const char *ptr, int size)
	{ init_readonly(ptr, size); }
    strstreambuf(const unsigned char *ptr, int size)
	{ init_readonly((const char*)ptr, size); }
    strstreambuf(signed char *ptr, int size, signed char *pstart = NULL)
	{ init_static((char*)ptr, size, (char*)pstart); }
    strstreambuf(const signed char *ptr, int size)
	{ init_readonly((const char*)ptr, size); }
    // Note: frozen() is always true if !_IO_STR_DYNAMIC(this).
    int frozen() { return _flags & _IO_USER_BUF ? 1 : 0; }
    void freeze(int n=1)
	{ if (_IO_STR_DYNAMIC(this))
	    { if (n) _flags |= _IO_USER_BUF; else _flags &= ~_IO_USER_BUF; } }
    _IO_ssize_t pcount();
    char *str();
    virtual streampos seekoff(streamoff, _seek_dir, int mode=ios::in|ios::out);
};

class strstreambase : virtual public ios {
  protected:
    strstreambuf __my_sb;
  public:
    strstreambuf* rdbuf() { return &__my_sb; }
  protected:
    strstreambase() { init (&__my_sb); }
    strstreambase(char *cp, int n, int mode=ios::out);
};

class istrstream : public strstreambase, public istream {
  public:
    istrstream(const char*, int=0);
};

class ostrstream : public strstreambase, public ostream {
  public:
    ostrstream() { }
    ostrstream(char *cp, int n, int mode=ios::out) :strstreambase(cp,n,mode){}
    _IO_ssize_t pcount() { return ((strstreambuf*)_strbuf)->pcount(); }
    char *str() { return ((strstreambuf*)_strbuf)->str(); }
    void freeze(int n = 1) { ((strstreambuf*)_strbuf)->freeze(n); }
    int frozen() { return ((strstreambuf*)_strbuf)->frozen(); }
};

class strstream : public strstreambase, public iostream {
  public:
  strstream() { }
    strstream(char *cp, int n, int mode=ios::out) :strstreambase(cp,n,mode){}
    _IO_ssize_t pcount() { return ((strstreambuf*)_strbuf)->pcount(); }
    char *str() { return ((strstreambuf*)_strbuf)->str(); }
    void freeze(int n = 1) { ((strstreambuf*)_strbuf)->freeze(n); }
    int frozen() { return ((strstreambuf*)_strbuf)->frozen(); }
};
} // extern "C++"

#endif /*!__STRSTREAM_H*/
