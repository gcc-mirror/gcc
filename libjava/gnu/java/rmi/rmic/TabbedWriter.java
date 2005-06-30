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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

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
