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

#ifndef _PFSTREAM_H
#define _PFSTREAM_H
#ifdef __GNUG__
#pragma interface
#endif
#include <fstream.h>

extern "C++" {
// ipfstream foo("NAME") is like: ifstream foo("NAME").  However,
// if NAME starts *or ends* with a '|', the remainder of NAME is
// evaluated as a shell command (using a procbuf), and all input
// read from foo is whatever that shell writes to its standard output.
// E.g. ipfstream foo("|zcat foo.Z") or ipfstream foo("zcat foo.Z|")
// (These two forms are equivalent.)

class ipfstream : public ifstream {
  public:
    ipfstream(const char *name, int mode=ios::in, int prot=0664);
};

// opfstream foo("NAME") is like: ofstream foo("NAME").
// However, if NAME starts with a '|', the remainder of NAME is
// evaluated as a shell command (using a procbuf), and all output
// written to foo is piped to the standard input of that shell.
// E.g. opfstream foo("|more");

class opfstream : public ofstream {
  public:
    opfstream(const char *name, int mode=ios::out, int prot=0664);
};
} // extern "C++"
#endif /*!_PFSTREAM_H*/

