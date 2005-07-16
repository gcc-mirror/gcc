/* UnicastRemoteObject.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2003 Free Software Foundation, Inc.

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


package java.rmi.server;

import gnu.java.rmi.server.UnicastServerRef;

import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;

public class UnicastRemoteObject extends RemoteServer
{
private static final long serialVersionUID = 4974527148936298033L;
//The following serialized fields are from Java API Documentation "Serialized form"
private int port = 0;
private RMIClientSocketFactory csf = null;
private RMIServerSocketFactory ssf = null;

protected UnicastRemoteObject() throws RemoteException {
	this(0);
}

protected UnicastRemoteObject(int port) throws RemoteException {
	this(port, RMISocketFactory.getSocketFactory(), RMISocketFactory.getSocketFactory());
}

protected UnicastRemoteObject(int port, RMIClientSocketFactory csf, RMIServerSocketFactory ssf) throws RemoteException {
  this.port = port;
  //Is RMIXXXSocketFactory serializable
  //this.csf = csf;
  //this.ssf = ssf;
  this.ref = new UnicastServerRef(new ObjID(), port, ssf);
  exportObject(this);
}

protected UnicastRemoteObject(RemoteRef ref) throws RemoteException {
	super((UnicastServerRef)ref);
	exportObject(this);
}

public Object clone() throws CloneNotSupportedException {
	throw new Error("Not implemented");
}

public static RemoteStub exportObject(Remote obj) throws RemoteException {
	UnicastServerRef sref = (UnicastServerRef)((RemoteObject)obj).getRef();
	return (sref.exportObject(obj));
}

  public static Remote exportObject(Remote obj, int port) throws RemoteException 
  {
    return exportObject(obj, port, null);
  }
  
  static Remote exportObject(Remote obj, int port, RMIServerSocketFactory ssf) 
    throws RemoteException 
  {
    UnicastServerRef sref = null;
    if (obj instanceof RemoteObject)
      sref = (UnicastServerRef)((RemoteObject)obj).getRef ();
    if(sref == null)
      {
	sref = new UnicastServerRef(new ObjID (), port, ssf);
      }
    Remote stub = sref.exportObject (obj); 
    addStub(obj, stub);
    return stub;
  }

  /**
   * FIXME
   */
  public static Remote exportObject(Remote obj, int port, RMIClientSocketFactory csf, 
				    RMIServerSocketFactory ssf) 
    throws RemoteException 
  {
    return (exportObject(obj, port, ssf));
  }

  public static boolean unexportObject(Remote obj, boolean force) 
    throws NoSuchObjectException 
  {
    if (obj instanceof RemoteObject)
      {
	deleteStub(obj);
	UnicastServerRef sref = (UnicastServerRef)((RemoteObject)obj).getRef();
	return sref.unexportObject(obj, force);
      }
    else
      {
	//FIX ME
	;
      }
    return true;
  }

}
