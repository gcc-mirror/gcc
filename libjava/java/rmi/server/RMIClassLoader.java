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

import java.net.URL;
import java.net.URLConnection;
import java.io.IOException;
import java.io.DataInputStream;
import java.net.MalformedURLException;
import java.util.StringTokenizer;

public class RMIClassLoader {

static private class MyClassLoader extends ClassLoader {

Class defineClass(String name, byte[] data) {
	return (defineClass(name, data, 0, data.length));
}
}
static private MyClassLoader loader = new MyClassLoader();

/**
 * @deprecated
 */
public static Class loadClass(String name) throws MalformedURLException, ClassNotFoundException {
	return (loadClass(System.getProperty("java.rmi.server.codebase"), name));
}

public static Class loadClass(URL codebase, String name) throws MalformedURLException, ClassNotFoundException {
	URL u = new URL(codebase, name + ".class");
	try {
		URLConnection conn = u.openConnection();
		DataInputStream strm = new DataInputStream(conn.getInputStream());
		byte data[] = new byte[conn.getContentLength()];
		strm.readFully(data);
		return (loader.defineClass(name, data));
	}
	catch (IOException _) {
		throw new ClassNotFoundException(name);
	}
}

public static Class loadClass(String codebase, String name) throws MalformedURLException, ClassNotFoundException {
	StringTokenizer tok = new StringTokenizer(codebase, ":");
	while (tok.hasMoreTokens()) {
		try {
			return (loadClass(new URL(tok.nextToken()), name));
		}
		catch (ClassNotFoundException _) {
			// Ignore - try the next one.
		}
	}
	throw new ClassNotFoundException(name);
}

public static String getClassAnnotation(Class cl) {
	return (null);	// We don't yet do this.
}

/**
 * @deprecated
 */
public static Object getSecurityContext(ClassLoader loader) {
	throw new Error("Not implemented");
}

}
