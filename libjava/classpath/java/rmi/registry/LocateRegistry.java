/* LocateRegistry.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2004  Free Software Foundation, Inc.

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


package java.rmi.registry;

import gnu.java.rmi.registry.RegistryImpl;
import gnu.java.rmi.registry.RegistryImpl_Stub;
import gnu.java.rmi.server.UnicastRef;

import java.rmi.RemoteException;
import java.rmi.server.ObjID;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.RMISocketFactory;
import java.rmi.server.RemoteRef;

public final class LocateRegistry {
  /**
   * This class isn't intended to be instantiated.
   */
  private LocateRegistry() {}

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
