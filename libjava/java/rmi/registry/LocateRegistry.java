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

package java.rmi.registry;

import java.io.IOException;
import java.rmi.RemoteException;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.RMISocketFactory;
import java.rmi.server.RemoteRef;
import java.rmi.server.ObjID;
import java.net.Socket;

import gnu.java.rmi.server.UnicastRef;
import gnu.java.rmi.server.UnicastServerRef;
import gnu.java.rmi.registry.RegistryImpl;
import gnu.java.rmi.registry.RegistryImpl_Stub;

public final class LocateRegistry {

public static Registry getRegistry() throws RemoteException {
	return (getRegistry("localhost", Registry.REGISTRY_PORT));
}

public static Registry getRegistry(int port) throws RemoteException {
	return (getRegistry("localhost", port));
}

public static Registry getRegistry(String host) throws RemoteException {
	return (getRegistry(host, Registry.REGISTRY_PORT));
}

public static Registry getRegistry(String host, int port) throws RemoteException {
	return (getRegistry(host, port, RMISocketFactory.getSocketFactory()));
}

public static Registry getRegistry(String host, int port, RMIClientSocketFactory csf) throws RemoteException {
	RemoteRef ref = new UnicastRef(new ObjID(ObjID.REGISTRY_ID), host, port, csf);
	return (new RegistryImpl_Stub(ref));
}

public static Registry createRegistry(int port) throws RemoteException {
	return (createRegistry(port, RMISocketFactory.getSocketFactory(), RMISocketFactory.getSocketFactory()));
}

public static Registry createRegistry(int port, RMIClientSocketFactory csf, RMIServerSocketFactory ssf) throws RemoteException {
	return (new RegistryImpl(port, csf, ssf));
}

}
