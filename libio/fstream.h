/* This is part of libio/iostream, providing -*- C++ -*- input/output.
Copyright (C) 1993, 2000 Free Software Foundation

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

#ifndef _FSTREAM_H
#define _FSTREAM_H
#ifdef __GNUG__
#pragma interface
#endif
#include <iostream.h>

extern "C++" {
class fstreambase : virtual public ios {
#ifdef _IO_NEW_STREAMS
    mutable filebuf __my_fb; // mutable so rdbuf() can be const
#endif
    void __fb_init ();
  public:
    fstreambase();
    fstreambase(int fd);
    fstreambase(int fd, char *p, int l); /* Deprecated */
    fstreambase(const char *name, int mode, int prot=0664);
    void close();
#ifdef _IO_NEW_STREAMS
    filebuf* rdbuf() const { return &__my_fb; }
#else
    filebuf* rdbuf() const { return (filebuf*) ios::rdbuf(); }
#endif
    void open(const char *name, int mode, int prot=0664);
    int is_open() const { return rdbuf()->is_open(); }
    void setbuf(char *ptr, int len) { rdbuf()->setbuf(ptr, len); }
    void attach(int fd);
#ifdef _STREAM_COMPAT
    int filedesc() { return rdbuf()->fd(); }
    fstreambase& raw() { rdbuf()->setbuf(NULL, 0); return *this; }
#endif
};

class ifstream : public fstreambase, public istream {
  public:
    ifstream() : fstreambase() { }
    ifstream(int fd) : fstreambase(fd) { }
    ifstream(int fd, char *p, int l) : fstreambase(fd, p, l) { } /*Deprecated*/
    ifstream(const char *name, int mode=ios::in, int prot=0664)
	: fstreambase(name, mode | ios::in, prot) { }
    void open(const char *name, int mode=ios::in, int prot=0664)
	{ fstreambase::open(name, mode | ios::in, prot); }
};

class ofstream : public fstreambase, public ostream {
  public:
    ofstream() : fstreambase() { }
    ofstream(int fd) : fstreambase(fd) { }
    ofstream(int fd, char *p, int l) : fstreambase(fd, p, l) { } /*Deprecated*/
    ofstream(const char *name, int mode=ios::out, int prot=0664)
	: fstreambase(name, mode | ios::out, prot) { }
    void open(const char *name, int mode=ios::out, int prot=0664)
	{ fstreambase::open(name, mode | ios::out, prot); }
};

class fstream : public fstreambase, public iostream {
  public:
    fstream() : fstreambase() { }
    fstream(int fd) : fstreambase(fd) { }
    fstream(const char *name, int mode, int prot=0664)
	: fstreambase(name, mode, prot) { }
    fstream(int fd, char *p, int l) : fstreambase(fd, p, l) { } /*Deprecated*/
    void open(const char *name, int mode, int prot=0664)
	{ fstreambase::open(name, mode, prot); }
};
} // extern "C++"
#endif /*!_FSTREAM_H*/
