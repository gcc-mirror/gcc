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

package java.rmi;

import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;

public final class Naming {

public static Remote lookup(String name) throws NotBoundException, MalformedURLException, RemoteException {
	URL u = new URL("http:" + name);
	return (getRegistry(u).lookup(u.getFile().substring(1)));
}

public static void bind(String name, Remote obj) throws AlreadyBoundException, MalformedURLException, RemoteException {
	URL u = new URL("http:" + name);
	getRegistry(u).bind(u.getFile().substring(1), obj);
}

public static void unbind(String name) throws RemoteException, NotBoundException, MalformedURLException {
	URL u = new URL("http:" + name);
	getRegistry(u).unbind(u.getFile().substring(1));
}

public static void rebind(String name, Remote obj) throws RemoteException, MalformedURLException {
	URL u = new URL("http:" + name);
	getRegistry(u).rebind(u.getFile().substring(1), obj);
}

public static String[] list(String name) throws RemoteException, MalformedURLException {
	return (getRegistry(new URL("http:" + name)).list());
}

private static Registry getRegistry(URL u) throws RemoteException {
	if (u.getPort() == -1) {
		return (LocateRegistry.getRegistry(u.getHost()));
	}
	else {
		return (LocateRegistry.getRegistry(u.getHost(), u.getPort()));
	}
}

}
