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

package gnu.java.rmi.registry;

import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.RemoteException;
import java.rmi.NotBoundException;
import java.rmi.AccessException;
import java.rmi.AlreadyBoundException;
import java.rmi.Remote;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.server.ObjID;
import java.util.Hashtable;
import java.util.Enumeration;
import java.rmi.server.RMISocketFactory;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.server.RMIServerSocketFactory;
import gnu.java.rmi.server.UnicastServerRef;

public class RegistryImpl
	extends UnicastRemoteObject implements Registry {

private Hashtable bindings = new Hashtable();

public RegistryImpl(int port) throws RemoteException {
	this(port, RMISocketFactory.getSocketFactory(), RMISocketFactory.getSocketFactory());
}

public RegistryImpl(int port, RMIClientSocketFactory cf, RMIServerSocketFactory sf) throws RemoteException {
	super(new UnicastServerRef(new ObjID(ObjID.REGISTRY_ID), port, sf));
	((UnicastServerRef)getRef()).exportObject(this);
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

public static void main(String[] args) {
	int port = Registry.REGISTRY_PORT;
	if (args.length > 0) {
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
