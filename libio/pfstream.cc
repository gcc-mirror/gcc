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

#ifdef __GNUG__
#pragma implementation
#endif
#include "libioP.h"
#include <pfstream.h>
#include <procbuf.h>
#include <string.h>

ipfstream::ipfstream(const char *name, int mode, int prot)
{
    const char* p;

    // Look for '| command' (as used by ftp).
    for (p = name; *p == ' ' || *p == '\t'; p++) ;
    if (*p == '|') {
	procbuf *pbuf = new procbuf();
	init(pbuf);
	if (!pbuf->open(p+1, mode))
	    set(ios::badbit);
	return;
    }

    // Look for 'command |'
    while (*p) p++; // Point to last 
    while (p[-1] == ' ' || p[-1] == '\t' || p[-1] == '\n') p--;
    if (p[-1] == '|') {
	// Must remove the final '|'.
	p--;
#if !defined (__GNUC__) || defined (__STRICT_ANSI__)
	char *command = new char[p-name+1];
#else
	char command[p-name+1];
#endif
        memcpy(command, name, p-name);
	command[p-name] = '\0';

	procbuf *pbuf = new procbuf();
	if (pbuf->open(command, mode))
	    set(ios::badbit);
#if !defined (__GNUC__) || defined (__STRICT_ANSI__)
	delete command;
#endif
	return;
    }

    init(new filebuf());
    if (!rdbuf()->open(name, mode, prot))
	set(ios::badbit);
}

opfstream::opfstream(const char *name, int mode, int prot)
{
    const char *p;
    // Look for '| command'.
    for (p = name; *p == ' ' || *p == '\t'; p++) ;
    if (*p == '|') {
	procbuf *pbuf = new procbuf();
	init(pbuf);
	if (!pbuf->open(p+1, mode))
	    set(ios::badbit);
    }
    else {
	init(new filebuf());
	if (!rdbuf()->open(name, mode, prot))
	    set(ios::badbit);
    }
}
