/* 
Copyright (C) 1993, 1999 Free Software Foundation

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

#ifndef _COMPAT_STREAM_H
#define _COMPAT_STREAM_H

// Compatibility with old library.
// DO NOT USE THESE FUNCTIONS IN NEW CODE!
// They are obsolete, non-standard, and non-reentrant.

#define _STREAM_COMPAT
#include <iostream.h>

extern "C++" {
extern char* form(const char*, ...);

extern char* dec(long, int=0);
extern char* dec(int, int=0);
extern char* dec(unsigned long, int=0);
extern char* dec(unsigned int, int=0);

extern char* hex(long, int=0);
extern char* hex(int, int=0);
extern char* hex(unsigned long, int=0);
extern char* hex(unsigned int, int=0);

extern char* oct(long, int=0);
extern char* oct(int, int=0);
extern char* oct(unsigned long, int=0);
extern char* oct(unsigned int, int=0);

char*        chr(char ch, int width = 0);
char*        str(const char* s, int width = 0);

inline istream& WS(istream& __str) { return ws(__str); }
} // extern "C++"

#endif /* !_COMPAT_STREAM_H */
