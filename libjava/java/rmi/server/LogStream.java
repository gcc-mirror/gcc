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

package java.rmi.server;

import java.io.PrintStream;
import java.io.OutputStream;
import java.io.IOException;

public class LogStream
	extends PrintStream {

public static final int SILENT = 0;
public static final int BRIEF = 1;
public static final int VERBOSE = 2;

private static PrintStream defStream;

private LogStream(OutputStream s) {
	super(s);
}

public static LogStream log(String name) {
	throw new Error("Not implemented");
}

public static PrintStream getDefaultStream() {
	return (defStream);
}

public static void setDefaultStream(PrintStream s) {
	defStream = s;
}

public OutputStream getOutputStream() {
	return (out);
}

public void setOutputStream(OutputStream s) {
	out = s;
}

public void write(int b) {
	super.write(b);
}

public void write(byte[] b, int off, int len) {
	super.write(b, off, len);
}

public String toString() {
	throw new Error("Not implemented");
}

public static int parseLevel(String s) {
	if (s.equalsIgnoreCase("silent")) {
		return (SILENT);
	}
	if (s.equalsIgnoreCase("brief")) {
		return (BRIEF);
	}
	if (s.equalsIgnoreCase("verbose")) {
		return (VERBOSE);
	}
	return (SILENT);
}

}
