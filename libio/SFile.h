/* This is part of libio/iostream, providing -*- C++ -*- input/output.
Copyright (C) 1988, 1992, 1993 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

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

#ifndef _SFile_h
#ifdef __GNUG__
#pragma interface
#endif
#define _SFile_h 1

#include <fstream.h>

extern "C++" {
class SFile: public fstream
{
  protected:
    int       sz;                   // unit size for structured binary IO

public:
    SFile() : fstream() { }
    SFile(int fd, int size);
    SFile(const char *name, int size, int mode, int prot=0664);
    void open(const char *name, int size, int mode, int prot=0664);
    
    int       size() { return sz; }
    int       setsize(int s) { int old = sz; sz = s; return old; }
    
    SFile&    get(void* x);
    SFile&    put(void* x);
    SFile&    operator[](long i);
};
} // extern "C++"

#endif
