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

#include <iostream.h>
#include <stdlib.h>
#include <string.h>

// Read either "dog", "hound", or "hounddog".
// If "dog" is found, return 1.
// If "hound" is found, return 2.
// If "hounddog" is found, return 3.
// If non of these are found, return -1.
int my_scan(streambuf* sb)
{
    streammarker fence(sb);
    char buffer[20];
    // Try reading "hounddog":
    if (sb->sgetn(buffer, 8) == 8 && strncmp(buffer, "hounddog", 8) == 0)
      return 3;
    // No, no "hounddog":  Backup to 'fence' ...
    sb->seekmark(fence);
    // ... and try reading "dog":
    if (sb->sgetn(buffer, 3) == 3 && strncmp(buffer, "dog", 3) == 0)
      return 1;
    // No, no "dog" either:  Backup to 'fence' ...
    sb->seekmark(fence);
    // ... and try reading "hound":
    if (sb->sgetn(buffer, 5) == 5 && strncmp(buffer, "hound", 5) == 0)
      return 2;
    // No, no "hound" either:  Backup to 'fence' and signal failure.
    sb->seekmark(fence); // Backup to 'fence'..
    return -1;
}

int main(int argc, char **argv)
{
    streambuf *sb = cin.rdbuf();
    if (argc > 1 && strncmp(argv[1], "-b", 2) == 0) {
	streambuf *ret;
	int buffer_size = atoi(&argv[1][2]);
	if (buffer_size == 0)
	    ret = sb->setbuf(NULL, 0);
	else
	    ret = sb->setbuf(new char[buffer_size], buffer_size);
	if (ret != sb)
	    cerr << "Warning: cin.rdbuf()->setbuf failed!\n";
    }
    for (;;) {
	int code = my_scan(sb);
	int ch = sb->sbumpc();
	if (code == -1 && ch == EOF)
	    break;
	int n = 0;
	while (ch != EOF && ch != '\n') {
	    n++;
	    ch = sb->sbumpc();
	};
	if (ch == EOF) {
	    cout << "[Unexpected EOF]\n";
	    break;
	}
	cout << "Code: " << code << " followed by " << n << " chars\n";
    }
}
