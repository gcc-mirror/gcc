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

import java.lang.CloneNotSupportedException;
import java.io.PrintStream;
import java.io.PrintWriter;

public class ServerCloneException
	extends CloneNotSupportedException {

public Exception detail;

public ServerCloneException(String s) {
	super(s);
	detail = null;
}

public ServerCloneException(String s, Exception e) {
	super(s);
	detail = e;
}

public String getMessage() {
	if (detail != null) {
		return (super.getMessage() + ":" + detail.getMessage());
	}
	else {
		return (super.getMessage());
	}
}

public void printStackTrace(PrintStream s) {
	if (detail != null) {
		detail.printStackTrace(s);
	}
	super.printStackTrace(s);
}

public void printStackTrace() {
	printStackTrace(System.err);
}

public void printStackTrace(PrintWriter s) {
	if (detail != null) {
		detail.printStackTrace(s);
	}
	super.printStackTrace(s);
}

}
