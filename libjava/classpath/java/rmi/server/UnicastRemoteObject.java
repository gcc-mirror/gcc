/* UnicastRemoteObject.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2002, 2003, 2006 
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


package java.rmi.server;

import gnu.java.rmi.server.UnicastServerRef;

import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * This class obtains stub that communicates with the remote object. 
 */
public class UnicastRemoteObject extends RemoteServer
{
  /**
   * Use SVUID for interoperability.
   */
  private static final long serialVersionUID = 4974527148936298033L;

  //The following serialized fields are from Java API Documentation
  // "Serialized form"
  
  /**
   * The port, on that the created remote object becomes available,
   * zero meaning the anonymous port.
   */
  private int port;
  
  /**
   * The client socket factory for producing client sockets, used by this
   * object.
   */
  private RMIClientSocketFactory csf;
  
  /**
   * The server socket factory for producing server sockets, used by this
   * object.
   */
  private RMIServerSocketFactory ssf;

  /**
   * Create and export new remote object without specifying the port value.
   * 
   * @throws RemoteException if the attempt to export the object failed.
   */
  protected UnicastRemoteObject()
    throws RemoteException
  {
	this(0);
  }
  
  /**
   * Create and export the new remote object, making it available at the
   * given port, local host.
   * 
   * @param port the port, on that the object should become available.
   * Zero means anonymous port.
   * 
   * @throws RemoteException if the attempt to export the object failed.
   */
  protected UnicastRemoteObject(int port)
    throws RemoteException
  {
	this(port, RMISocketFactory.getSocketFactory(),
         RMISocketFactory.getSocketFactory());
  }

  /**
   * Create and export the new remote object, making it available at the
   * given port, using sockets, produced by the specified factories.
   * 
   * @param port the port, on that the object should become available.
   * Zero means anonymous port.
   * 
   * @param clientSocketFactory the client socket factory
   * @param serverSocketFactory the server socket factory
   * 
   * @throws RemoteException if the attempt to export the object failed.
   */
  protected UnicastRemoteObject(int port, 
                                RMIClientSocketFactory clientSocketFactory,
                                RMIServerSocketFactory serverSocketFactory)
    throws RemoteException
  {
    this.port = port;
    //Is RMIXXXSocketFactory serializable
    //this.csf = csf;
    //this.ssf = ssf;
    this.ref = new UnicastServerRef(new ObjID(), port, serverSocketFactory);
    exportObject(this, port);
  }

  protected UnicastRemoteObject(RemoteRef ref)
    throws RemoteException
  {
	super((UnicastServerRef) ref);
	exportObject(this, 0);
  }

  public Object clone()
    throws CloneNotSupportedException
  {
	throw new Error("Not implemented");
  }
  
  /**
   * Export object, making it available for the remote calls at the 
   * anonymous port. 
   * 
   * This method returns the instance of the abstract class, not an interface.
   * Hence it will not work with the proxy stubs that are supported since
   * jdk 1.5 (such stubs cannot be derived from the RemoteStub). Only use
   * this method if you are sure that the stub class will be accessible.
   * 
   * @param obj the object being exported.
   * 
   * @return the remote object stub
   * 
   * @throws RemoteException if the attempt to export the object failed.
   */
  public static RemoteStub exportObject(Remote obj)
    throws RemoteException
  {
	return (RemoteStub) exportObject(obj, 0);
  }

  /**
   * Export object, making it available for the remote calls at the 
   * specified port.
   * 
   * Since jdk 1.5 this method does not longer require the stub class to be
   * present. If such class is not found, the stub is replaced by the 
   * dynamically constructed proxy class. No attempt to find and load the stubs
   * is made if the system property java.rmi.server.ignoreStubClasses
   * is set to true (set to reduce the starting time if the stubs are 
   * surely not present and exclusively 1.2 RMI is used).
   * 
   * @param obj the object being exported.
   * @param port the remote object port
   * 
   * @return the remote object stub
   * 
   * @throws RemoteException if the attempt to export the object failed.
   */
  public static Remote exportObject(Remote obj, int port)
    throws RemoteException
  {
    return exportObject(obj, port, null);
  }

  /**
   * Create and export the new remote object, making it available at the
   * given port, using sockets, produced by the specified factories.
   * 
   * Since jdk 1.5 this method does not longer require the stub class to be
   * present. If such class is not found, the stub is replaced by the 
   * dynamically constructed proxy class. No attempt to find and load the stubs
   * is made if the system property java.rmi.server.ignoreStubClasses
   * is set to true (set to reduce the starting time if the stubs are 
   * surely not present and exclusively 1.2 RMI is used).
   * 
   * @param port the port, on that the object should become available.
   * Zero means anonymous port.
   * 
   * @param serverSocketFactory the server socket factory
   */ 
  static Remote exportObject(Remote obj, int port, 
                             RMIServerSocketFactory serverSocketFactory) 
    throws RemoteException
  {
    UnicastServerRef sref = null;
    if (obj instanceof RemoteObject)
      sref = (UnicastServerRef) ((RemoteObject) obj).getRef();

    if (sref == null)
      sref = new UnicastServerRef(new ObjID(), port, serverSocketFactory);

    Remote stub = sref.exportObject(obj);
    addStub(obj, stub);
    return stub;
  }

  /**
   * FIXME
   */
  public static Remote exportObject(Remote obj, int port,
                                    RMIClientSocketFactory csf,
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
        UnicastServerRef sref =
          (UnicastServerRef) ((RemoteObject) obj).getRef();
        return sref.unexportObject(obj, force);
      }
    // FIXME
    /* else
      {
        ;
      }
    */
    return true;
  }

}
