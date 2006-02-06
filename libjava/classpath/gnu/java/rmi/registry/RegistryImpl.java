/* RegistryImpl.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2005, 2006
   Free Software Foundation, Inc.

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

package gnu.java.rmi.registry;

import gnu.java.rmi.server.UnicastServerRef;

import java.rmi.AccessException;
import java.rmi.AlreadyBoundException;
import java.rmi.NotBoundException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.ObjID;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.RMISocketFactory;
import java.rmi.server.UnicastRemoteObject;
import java.util.Enumeration;
import java.util.Hashtable;

public class RegistryImpl
	extends UnicastRemoteObject implements Registry {

private Hashtable bindings = new Hashtable();

public RegistryImpl(int port) throws RemoteException {
	this(port, RMISocketFactory.getSocketFactory(), RMISocketFactory.getSocketFactory());
}

public RegistryImpl(int port, RMIClientSocketFactory cf, RMIServerSocketFactory sf) throws RemoteException {
	super(new UnicastServerRef(new ObjID(ObjID.REGISTRY_ID), port, sf));
	// The following is unnecessary, because UnicastRemoteObject export itself automatically.
	//((UnicastServerRef)getRef()).exportObject(this);
}

public Remote lookup(String name) throws RemoteException, NotBoundException, AccessException {
	Object obj = bindings.get(name);
	if (obj == null) {
		throw new NotBoundException(name);
	}
	return ((Remote)obj);
}

public void bind(String name, Remote obj) throws RemoteException, AlreadyBoundException, AccessException {
	if (bindings.containsKey(name)) {
		throw new AlreadyBoundException(name);
	}
	bindings.put(name, obj);
}

public void unbind(String name) throws RemoteException, NotBoundException, AccessException {
	Object obj = bindings.remove(name);
	if (obj == null) {
		throw new NotBoundException(name);
	}
}

public void rebind(String name, Remote obj) throws RemoteException, AccessException {
	bindings.put(name, obj);
}

public String[] list() throws RemoteException, AccessException {
	int size = bindings.size();
	String[] strings = new String[size];
	Enumeration e = bindings.keys();
	for (int i = 0; i < size; i++) {
		strings[i] = (String)e.nextElement();
	}
	return (strings);
}

public static void version() {
	System.out.println("rmiregistry ("
			   + System.getProperty("java.vm.name")
			   + ") "
			   + System.getProperty("java.vm.version"));
	System.out.println("Copyright 2006 Free Software Foundation, Inc.");
	System.out.println("This is free software; see the source for copying conditions.  There is NO");
	System.out.println("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.");
	System.exit(0);
}

public static void help() {
	System.out.println(
"Usage: rmiregistry [OPTION | PORT]\n" +
"\n" +
"    --help                Print this help, then exit\n" +
"    --version             Print version number, then exit\n");
	System.exit(0);
}

public static void main(String[] args) {
	int port = Registry.REGISTRY_PORT;
	if (args.length > 0) {
		if (args[0].equals("--version")) {
			version();
		}
		else if (args[0].equals("--help")) {
			help();
		}
		try {
			port = Integer.parseInt(args[0]);
		}
		catch (NumberFormatException _) {
			System.err.println("Bad port number - using default");
		}
	}

	try {
		Registry impl = LocateRegistry.createRegistry(port);
	}
	catch (RemoteException _) {
		System.err.println("Registry failed");
	}
}

}
