/* PortableRemoteObject.java --
   Copyright (C) 2004, 2004, 2005 Free Software Foundation, Inc.

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


package javax.rmi;

import gnu.javax.rmi.CORBA.DelegateFactory;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.ORB;
import org.omg.CORBA.portable.ObjectImpl;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;

import javax.rmi.CORBA.PortableRemoteObjectDelegate;
import javax.rmi.CORBA.Stub;
import javax.rmi.CORBA.Tie;
import javax.rmi.CORBA.Util;

/**
 * <p>
 * An utility class for RMI/IDL server side object implementations. Server side
 * implementation objects may inherit from this class, but this is not
 * mandatory, as the needed methds are static. Server side implementations may
 * choose to inherit from {@link ObjectImpl} or {@link Servant} instead.
 * </p>
 * <p>
 * The functionality of methods in this class is forwarded to the enclosed
 * PortableRemoteObjectDelegate. This delegate can be altered by setting the
 * system property "javax.rmi.CORBA.PortableRemoteObjectClass" to the name of
 * the alternative class that must implement
 * {@link PortableRemoteObjectDelegate}.
 * </p>
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class PortableRemoteObject
{
  /**
   * A delegate where the functionality is forwarded.
   */
  static PortableRemoteObjectDelegate delegate = (PortableRemoteObjectDelegate) DelegateFactory.getInstance(DelegateFactory.PORTABLE_REMOTE_OBJECT);

  /**
   * The protected constructor calls {@link exportObject} (this).
   * 
   * @throws RemoteException if the exportObject(this) throws one.
   */
  protected PortableRemoteObject()
    throws RemoteException
  {
    exportObject((Remote) this);
  }

  /**
   * <p>
   * Makes the remote object <code>a_target</code> ready for remote
   * communication using the same communications runtime as for the passed
   * <code>a_source</code> parameter. The a_target is connected to the same
   * ORB (and, if applicable, to the same {@link POA}) as the a_source.
   * 
   * @param target the target to connect to ORB, must be an instance of either
   * {@link ObjectImpl} (Stubs and old-style ties) or {@link Tie}.
   * 
   * @param source the object, providing the connection information, must be
   * an instance of either {@link ObjectImpl} (Stubs and old-style ties) or
   * {@link Servant} (the next-generation Ties supporting {@link POA}).
   * 
   * @throws RemoteException if the target is already connected to another ORB.
   */
  public static void connect(Remote target, Remote source)
    throws RemoteException
  {
    delegate.connect(target, source);
  }

  /**
   * <p>
   * Makes a server object ready for remote calls. The subclasses of
   * PortableRemoteObject do not need to call this method, as it is called by
   * the constructor.
   * </p>
   * <p>
   * This method only creates a tie object and caches it for future usage. The
   * created tie does not have a delegate or an ORB associated.
   * </p>
   * 
   * @param object the object to export.
   * 
   * @throws RemoteException if export fails due any reason.
   */
  public static void exportObject(Remote object)
    throws RemoteException
  {
    delegate.exportObject(object);
  }

  /**
   * Narrows the passed object to conform to the given interface or IDL type. In
   * RMI-IIOP, this method replaces the narrow(org.omg.CORBA.Object) method that
   * was present in the CORBA Helpers. This method frequently returns different
   * instance and cannot be replaced by the direct cast. The typical narrowing
   * cases (all supported by GNU Classpath) are:
   * <ul>
   * <li>A CORBA object (for instance, returned by the
   * {@link ORB#string_to_object} or from the naming service) can be narrowed
   * into interface, derived from Remote. The method will try to locate an
   * appropriate {@link Stub} by the name pattern (_*_Stub). If the object being
   * narrowed is connected to an ORB, the returned instance will inherit that
   * connection, representing the same remote (or local) object, but now with
   * the possibility to invoke remote methods. </li>
   * <li>A CORBA object may be directly narrowed into the appropriate
   * {@link Stub} class, if it is and passed as a second parameter. This allows
   * to use non-standard stubs without parameterless constructors.</li>
   * <li>Any two classes, derived from the {@link ObjectImpl} (may be Stub's)
   * can be narrowed one into another (a delegate is transferred). </li>
   * <li>An implementation of Remote can be narrowed into {@link Tie} that can
   * later connected to an ORB, making the methods accessible remotely. The
   * Remote being narrowed normally provides a local implementation, but you can
   * also narrow remote Stub, creating "forwarding Tie".</li>
   * <li>null is narrowed into null regardless of the second parameter.</li>
   * <li>A {@link Tie} can be narrowed into Remote, representing the
   * implementation for this Tie (if one is set).</li>
   * </ul>
   * 
   * @param object the object like CORBA Object, Stub or Remote that must be
   * narrowed to the given interface.
   * 
   * @param narrowToInstaceOf the class of the interface to that the object must
   * be narrowed.
   * 
   * @return On success, an object of type narrowTo or null, if narrowFrom =
   * null.
   * 
   * @throws ClassCastException if no narrowing is possible.
   */
  public static Object narrow(Object object, Class narrowToInstaceOf)
    throws ClassCastException
  {
    return delegate.narrow(object, narrowToInstaceOf);
  }

  /**
   * <p>
   * Takes a server implementation object (name pattern *imp) and returns a stub
   * object that can be used to access that server object (target), name
   * (pattern _*_Stub).
   * 
   * The returned stub is not connected to any ORB and must be explicitly
   * connected using {@link #connect}.
   * </p>
   * <p>
   * The method signature prevents it from returning stubs that does not
   * implement Remote (ClassCastException will be thrown).
   * </p>
   * 
   * @param target a server side object implementation.
   * @return a stub object that can be used to access that server object.
   * 
   * @throws NoSuchObjectException if a stub class cannot be located by supposed
   * name pattern, or an instance of stub fails to be instantiated.
   * 
   * @throws ClassCastException if the stub class can be located, but it does
   * not inherit from Remote.
   * 
   * @throws BAD_PARAM if the name of the passed class does not match the
   * implementation name pattern (does not end by 'Impl').
   */
  public static Remote toStub(Remote targetImpl)
    throws NoSuchObjectException
  {
    return delegate.toStub(targetImpl);
  }

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
  public static void unexportObject(Remote object)
    throws NoSuchObjectException
  {
    delegate.unexportObject(object);
  }
}