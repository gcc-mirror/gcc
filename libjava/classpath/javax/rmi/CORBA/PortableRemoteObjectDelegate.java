/* PortableRemoteObjectDelegate.java -- Interface supporting PortableRemoteObject
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.

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


package javax.rmi.CORBA;

import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * A delegate, implementing the functionality, provided by the
 * {@link PortableRemoteObject}.
 * 
 * The default delegate can be altered by setting the system property
 * "javax.rmi.CORBA.PortableRemoteObjectClass" to the name of the alternative
 * class that must implement {@link PortableRemoteObjectDelegate}.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public interface PortableRemoteObjectDelegate
{
  /**
   * <p>
   * Makes the remote object <code>target</code> ready for remote
   * communication using the same communications runtime as for the passed
   * <code>source</code> parameter. Connection normally happens implicitly
   * when the object is sent or received as an argument on a remote method call.
   * </p>
   * <p>
   * The target object is connected to the same ORB as source by calling the
   * {@link Stub#connect} if it is a stub or by associating its tie with an ORB
   * if it is an implementation object.
   * </p>
   * 
   * @param target the target object that may be either an RMI/IDL stub or an
   * exported RMI/IDL implementation object
   * @param source the source object may also be either an RMI/IDL stub or an
   * exported RMI/IDL implementation object.
   * 
   * @throws RemoteException if the target is already connected to another ORB.
   */
  void connect(Remote target, Remote source)
    throws RemoteException;

  /**
   * Register the passed object with the ORB runtimes, making it remotely
   * accessible. When called on jre with no objects exported, creates a
   * non-daemon thread that prevents jre from terminating until all objects are
   * unexported. Also, such object cannot be collected by garbage collector.
   * This is usually impemented via {@link Util#unexportObject}
   * 
   * @param object the object to export.
   * 
   * @throws RemoteException
   */
  void exportObject(Remote obj)
    throws RemoteException;

  /**
   * Narrows the passed object to conform to the given interface or IDL type.
   * This method may return different instance and cannot be replaced by the
   * direct cast.
   * 
   * @param narrowFrom an object to narrow.
   * @param narrowTo a type to that the object must be narrowed.
   * 
   * @return On success, an object of type narrowTo or null, if narrowFrom =
   * null.
   * 
   * @throws ClassCastException if no narrowing is possible.
   */
  Object narrow(Object narrowFrom, Class narrowTo)
    throws ClassCastException;

  /**
   * Takes a server implementation object and returns a stub object that can be
   * used to access that server object (target). If the target is connected, the
   * returned stub is also connected to the same ORB. If the target is
   * unconnected, the returned stub is unconnected.
   * 
   * @param target a server side object.
   * @return a stub object that can be used to access that server object.
   * 
   * @throws NoSuchObjectException if a stub cannot be located for the given
   * target.
   */
  Remote toStub(Remote obj)
    throws NoSuchObjectException;

  /**
   * Deregister a currently exported server object from the ORB runtimes. The
   * object to becomes available for garbage collection. This is usually
   * impemented via {@link Util#unexportObject}
   * 
   * @param object the object to unexport.
   * 
   * @throws NoSuchObjectException if the passed object is not currently
   * exported.
   */
  void unexportObject(Remote obj)
    throws NoSuchObjectException;
}
