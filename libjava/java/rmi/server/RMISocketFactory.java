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

import java.net.Socket;
import java.net.ServerSocket;
import java.io.IOException;
import gnu.java.rmi.server.RMIDefaultSocketFactory;

public abstract class RMISocketFactory
	implements RMIClientSocketFactory, RMIServerSocketFactory {

static private RMISocketFactory defaultFactory;
static private RMISocketFactory currentFactory;
static private RMIFailureHandler currentHandler;

static {
	defaultFactory = new RMIDefaultSocketFactory();
	currentFactory = defaultFactory;
}

public RMISocketFactory() {
}

public abstract Socket createSocket(String host, int port) throws IOException;

public abstract ServerSocket createServerSocket(int port) throws IOException;

public static void setSocketFactory(RMISocketFactory fac) throws IOException {
	currentFactory = fac;
}

public static RMISocketFactory getSocketFactory() {
	return (currentFactory);
}

public static RMISocketFactory getDefaultSocketFactory() {
	return (defaultFactory);
}

public static void setFailureHandler(RMIFailureHandler fh) {
	currentHandler = fh;
}

public static RMIFailureHandler getFailureHandler() {
	return (currentHandler);
}

}
