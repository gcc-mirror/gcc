/*
  Copyright (c) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License.
 */

package gnu.java.rmi.rmic;

import java.io.FilterWriter;
import java.io.Writer;
import java.io.IOException;

public class TabbedWriter
	extends FilterWriter {

private static final String defaultTabstring = "    ";
private char[] tabstring; 
private int tabs;

public TabbedWriter(Writer o) {
	this(o, defaultTabstring);
}

public TabbedWriter(Writer o, String str) {
	super(o);
	tabs = 0;
	tabstring = str.toCharArray();
}

public void write(int c) throws IOException {
	out.write(c);
	if (c == '\n') {
		for (int i = 0; i < tabs; i++) {
			out.write(tabstring, 0, tabstring.length);
		}
	}
}

public void write(char cbuf[], int off, int len) throws IOException {
	for (int i = 0; i < len; i++) {
		write((int)cbuf[i+off]);
	}
}

public void write(String str, int off, int len) throws IOException {
	write(str.toCharArray(), off, len);
}

public void unindent() throws IOException {
	unindent(1);
}

public void unindent(int nr) throws IOException {
	indent(-nr);
}

public void indent() throws IOException {
	indent(1);
}

public void indent(int nr) throws IOException {
	tabs += nr;
	if (tabs < 0) {
		tabs = 0;
	}
	write((int)'\n');
}

}
