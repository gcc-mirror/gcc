/* 
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

#include <stdarg.h>
#include <string.h>
#include "libioP.h"
#include "stream.h"
#include "strstream.h"

static char Buffer[_IO_BUFSIZ];
#define EndBuffer (Buffer+_IO_BUFSIZ)
static char* next_chunk = Buffer; // Start of available part of Buffer.

char* form(const char* format, ...)
{
    int space_left = EndBuffer - next_chunk;
    // If less that 25% of the space is available start over.
    if (space_left < (_IO_BUFSIZ>>2))
	next_chunk = Buffer;
    char* buf = next_chunk;

    strstreambuf stream(buf, EndBuffer-buf-1, buf);
    va_list ap;
    va_start(ap, format);
    int count = stream.vform(format, ap);
    va_end(ap);
    stream.sputc(0);
    next_chunk = buf + stream.pcount();
    return buf;
}

#define u_long unsigned long

static char* itoa(unsigned long i, int size, int neg, int base)
{
    // Conservative estimate: If base==2, might need 8 characters
    // for each input byte, but normally 3 is plenty.
    int needed = size ? size
	: (base >= 8 ? 3 : 8) * sizeof(unsigned long) + 2;
    int space_left = EndBuffer - next_chunk;
    if (space_left <= needed)
	next_chunk = Buffer; // start over.

    char* buf = next_chunk;

    register char* ptr = buf+needed+1;
    next_chunk = ptr;

    if (needed < (2+neg) || ptr > EndBuffer)
	return NULL;
    *--ptr = 0;
    
    if (i == 0)
	*--ptr = '0';
    while (i != 0 && ptr > buf) {
	int ch = i % base;
	i = i / base;
	if (ch >= 10)
	    ch += 'a' - 10;
	else
	    ch += '0';
	*--ptr = ch;
    }
    if (neg)
	*--ptr = '-';
    if (size == 0)
	return ptr;
    while (ptr > buf)
	*--ptr = ' ';
    return buf;
}

char* dec(long i, int len /* = 0 */)
{
    if (i >= 0) return itoa((unsigned long)i, len, 0, 10);
    else return itoa((unsigned long)(-i), len, 1, 10);
}
char* dec(int i, int len /* = 0 */)
{
    if (i >= 0) return itoa((unsigned long)i, len, 0, 10);
    else return itoa((unsigned long)(-i), len, 1, 10);
}
char* dec(unsigned long i, int len /* = 0 */)
{
    return itoa(i, len, 0, 10);
}
char* dec(unsigned int i, int len /* = 0 */)
{
    return itoa(i, len, 0, 10);
}

char* hex(long i, int len /* = 0 */)
{
    return itoa((unsigned long)i, len, 0, 16);
}
char* hex(int i, int len /* = 0 */)
{
    return itoa((unsigned long)i, len, 0, 16);
}
char* hex(unsigned long i, int len /* = 0 */)
{
    return itoa(i, len, 0, 16);
}
char* hex(unsigned int i, int len /* = 0 */)
{
    return itoa(i, len, 0, 16);
}

char* oct(long i, int len /* = 0 */)
{
    return itoa((unsigned long)i, len, 0, 8);
}
char* oct(int i, int len /* = 0 */)
{
    return itoa((unsigned long)i, len, 0, 8);
}
char* oct(unsigned long i, int len /* = 0 */)
{
    return itoa(i, len, 0, 8);
}
char* oct(unsigned int i, int len /* = 0 */)
{
    return itoa(i, len, 0, 8);
}

static char *str(const char* s, int len, int width)
{
  if (width < len)
    width = len;
  int space_left = EndBuffer - next_chunk;
  if (space_left <= width + 1)
    next_chunk = Buffer; // start over.
  char* buf = next_chunk;
  memset (buf, ' ', width - len);
  memcpy (buf + width - len, s, len);
  buf[width] = 0;
  return buf;
}

char* str(const char* s, int width)
{
  return str (s, strlen (s), width);
}

char* chr(char ch, int width)
{
  char c = ch;
  return str (&c, 1, width);
}
