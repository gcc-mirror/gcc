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

import java.rmi.RemoteException;
import java.rmi.Remote;
import java.rmi.server.RemoteRef;
import java.rmi.NoSuchObjectException;
import gnu.java.rmi.server.UnicastServerRef;

public class UnicastRemoteObject
	extends RemoteServer {

protected UnicastRemoteObject() throws RemoteException {
	this(0);
}

protected UnicastRemoteObject(int port) throws RemoteException {
	this(port, RMISocketFactory.getSocketFactory(), RMISocketFactory.getSocketFactory());
}

protected UnicastRemoteObject(int port, RMIClientSocketFactory csf, RMIServerSocketFactory ssf) throws RemoteException {
	super(new UnicastServerRef(new ObjID(), port, ssf));
}

protected UnicastRemoteObject(RemoteRef ref) throws RemoteException {
	super((UnicastServerRef)ref);
}

public Object clone() throws CloneNotSupportedException {
	throw new Error("Not implemented");
}

public static RemoteStub exportObject(Remote obj) throws RemoteException {
	UnicastServerRef sref = (UnicastServerRef)((RemoteObject)obj).getRef();
	return (sref.exportObject(obj));
}

public static Remote exportObject(Remote obj, int port) throws RemoteException {
	return (exportObject(obj));
}

public static Remote exportObject(Remote obj, int port, RMIClientSocketFactory csf, RMIServerSocketFactory ssf) throws RemoteException {
	return (exportObject(obj));
}

public static boolean unexportObject(Remote obj, boolean force) throws NoSuchObjectException {
	throw new Error("Not implemented");
}

}
