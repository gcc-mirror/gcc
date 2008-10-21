/* Activatable.java -- A common ancestor for the activatable objects.
   Copyright (c) 1996, 1997, 1998, 1999, 2004, 2006
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


package java.rmi.activation;

import gnu.java.rmi.server.ActivatableServerRef;
import gnu.java.rmi.server.UnicastServer;
import gnu.java.rmi.server.UnicastServerRef;

import java.lang.reflect.Field;
import java.rmi.MarshalledObject;
import java.rmi.NoSuchObjectException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.ObjID;
import java.rmi.server.RMIClientSocketFactory;
import java.rmi.server.RMIServerSocketFactory;
import java.rmi.server.RemoteObject;
import java.rmi.server.RemoteServer;
import java.rmi.server.UnicastRemoteObject;

/**
 * A common ancestor for the implementations of the activatable objects. Such
 * objects require persistent access over time and can be activated by the
 * system. The derived classes also implements the needed interface of some
 * remote object and usually have the two parameter constructor, the first
 * parameter being the {@link ActivationID} and the second the
 * {@link MarshalledObject}. Activatable is the main class that developers need
 * to use to implement and manage activatable objects. It also contains methods
 * for making activatable remote objects that are not derived from the 
 * Activatable class.
 * 
 * @author Audrius Meskauskas (audriusa@bioinformatics.org) (from stub) 
 */
public abstract class Activatable
    extends RemoteServer
{

  /**
   * Use SVUID for interoperability.
   */
  static final long serialVersionUID = - 3120617863591563455L;
  
  /**
   * The object activation id.
   */
  final ActivationID id;
  
  /**
   * This constructor is used to register export the object on the given port. A
   * subclass of the Activatable class calls this constructor to register and
   * export the object during initial construction. As a side-effect of
   * activatable object construction, the remote object is both "registered"
   * with the activation system and "exported" (on an anonymous port, if port is
   * zero) to the RMI runtime so that it is available to accept incoming calls
   * from clients.
   * 
   * @param codebase the object code base url
   * @param data the data, needed to activate the object.
   * @param restart specifies reactivation mode after crash. If true, the object
   *          is activated when activator is restarted or the activation group
   *          is restarted. If false, the object is only activated on demand.
   *          This flag does has no effect during the normal operation (the
   *          object is normally activated on demand).
   * @param port the port, on which the object will become available. The value
   *          0 means anonymous port.
   * @throws ActivationException if the activation failed
   * @throws RemoteException if the remote call failed.
   */
  protected Activatable(String codebase, MarshalledObject<?> data,
                        boolean restart, int port) throws ActivationException,
      RemoteException
  {
    ActivationDesc descriptor = new ActivationDesc(getClass().getName(),
                                                   codebase, data, restart);
    id = obtainId(descriptor);
    exportObject(this, id, port);
  }

  /**
   * This constructor is used to register export the object on the given port,
   * additionally specifying the socket factories. A subclass of the Activatable
   * class calls this constructor to register and export the object during
   * initial construction.
   * 
   * @param codebase the object code base url
   * @param data the data, needed to activate the object.
   * @param restart specifies reactivation mode after crash. If true, the object
   *          is activated when activator is restarted or the activation group
   *          is restarted. If false, the object is only activated on demand.
   *          This flag does has no effect during the normal operation (the
   *          object is normally activated on demand).
   * @param port the port, on which the object will become available. The value
   *          0 means anonymous port.
   * @param csf the client socket factory
   * @param ssf the server socket factory
   * @throws ActivationException if the activation failed
   * @throws RemoteException if the remote call failed.
   */
  protected Activatable(String codebase, MarshalledObject<?> data,
                        boolean restart, int port, RMIClientSocketFactory csf,
                        RMIServerSocketFactory ssf) throws ActivationException,
      RemoteException
  {
    ActivationDesc descriptor = new ActivationDesc(getClass().getName(),
                                                   codebase, data, restart);
    id = obtainId(descriptor);
    exportObject(this, id, port);
  }

  /**
   * Creates the new instance of activatable with the given activation id and is
   * listening at the given port. A subclass of the Activatable class calls this
   * constructor when the object itself is activated via its special
   * "activation" constructor with the two parameters ({@link ActivationID},
   * {@link MarshalledObject}). As a side effect, the object is exported and is
   * available to accept incomming calls.
   * 
   * @param anId the activation id
   * @param port the port, on which the activatable will be listening
   * @throws RemoteException if the activation failed.
   */
  protected Activatable(ActivationID anId, int port) throws RemoteException
  {
    id = anId;
    try
      {
        exportObject(this, anId, port);
      }
    catch (Exception e)
      {
        e.printStackTrace();
        RemoteException acex = 
          new RemoteException("cannot export Activatable", e);
        throw acex;
      }
  }

  /**
   * Creates the new instance of activatable with the given activation id and is
   * listening at the given port, using the specified client and server sockets
   * factories. A subclass of the Activatable class calls this
   * constructor when the object itself is activated via its special
   * "activation" constructor with the two parameters ({@link ActivationID},
   * {@link MarshalledObject}). As a side effect, the object is exported and is
   * available to accept incomming calls.
   * 
   * @param anId the activation id
   * @param port the port, on which the activatable will be listening
   * @param csf the client socket factory
   * @param ssf the server socket factory
   * 
   * @throws RemoteException if the remote call failed
   */
  protected Activatable(ActivationID anId, int port, RMIClientSocketFactory csf,
                        RMIServerSocketFactory ssf) throws RemoteException
  {
    id = anId;
    try
      {
        exportObject(this, anId, port, csf, ssf);
      }
    catch (Exception e)
      {
        RemoteException acex = new RemoteException();
        acex.initCause(e);
        throw acex;
      }
  }
  
  /**
   * Get the objects activation identifier.
   * 
   * @return the object activation identifier
   */
  protected ActivationID getID()
  {
    return id;
  }
  
  /**
   * Obtain the activation Id from the activation descriptor by registering
   * within the current group.
   */
  static ActivationID obtainId(ActivationDesc descriptor)
      throws RemoteException, UnknownGroupException, ActivationException
  {
    ActivationGroupID id = descriptor.getGroupID();
    ActivationSystem system;

    if (id != null)
      system = id.getSystem();
    else
      system = ActivationGroup.currentGroupID().getSystem();
    return system.registerObject(descriptor);
  }
  
  /**
   * This method registers an activatable object. The object is expected to be
   * on the anonymous port (null client and server socket factories).
   * 
   * @param desc the object description.
   * @return the remote stub for the activatable object (the first call on this
   *         stub will activate the object).
   * @throws UnknownGroupException if the object group identifier is unknown
   * @throws ActivationException if the activation system is not running
   * @throws RemoteException if the remote call fails
   */
  public static Remote register(ActivationDesc desc)
      throws UnknownGroupException, ActivationException, RemoteException
  {
    ActivationID id = obtainId(desc);
    try
      {
        return toStub(
                      id,
                      Thread.currentThread().getContextClassLoader().loadClass(
                        desc.getClassName()));
      }
    catch (ClassNotFoundException e)
      {
        throw new ActivationException("Class not found: "+desc.getClassName());
      }
  }
  
  /**
   * Inactivates and unexports the object. The subsequent calls will activate
   * the object again. The object is not inactivated if it is currently
   * executing calls.
   * 
   * @param id the id of the object being inactivated
   * @return true if the object has been inactivated, false if it has not been
   *         inactivated because of the running or pending calls.
   * @throws UnknownObjectException if the object is unknown.
   * @throws ActivationException if the object group is not active
   * @throws RemoteException if the remote call fails
   */
  public static boolean inactive(ActivationID id)
      throws UnknownObjectException, ActivationException, RemoteException
  {
    if (id.group!=null)
      id.group.inactiveObject(id);
    return UnicastRemoteObject.unexportObject(id.activate(false), false);
  }
  
  /**
   * Unregister the object (the object will no longer be activable with that id)
   * 
   * @param id the object id
   * @throws UnknownObjectException if the id is unknown
   * @throws ActivationException if the activation system is not running
   * @throws RemoteException if the remote call fails.
   */
  public static void unregister(ActivationID id) throws UnknownObjectException,
      ActivationException, RemoteException
  {
    ActivationGroup.currentGroupId.getSystem().unregisterObject(id);
    UnicastServer.unregisterActivatable(id);
  }
  
  /**
   * Register and export the object that activatable object that is not derived
   * from the Activatable super class. It creates and registers the object
   * activation descriptor. There is no need to call this method if the object
   * extends Activable, as its work is done in the constructor
   * {@link #Activatable(String, MarshalledObject, boolean, int)}.
   * 
   * @param obj the object, that is exported, becoming available at the given
   *          port.
   * @param location the object code location (codebase).
   * @param data the data, needed to activate the object
   * @param restart the restart mode
   * @param port the port, where the object will be available
   * 
   * @return the created object activation ID.
   * 
   * @throws ActivationException if the activation group is not active
   * @throws RemoteException if the registration or export fails
   */
  public static ActivationID exportObject(Remote obj, String location,
                                          MarshalledObject<?> data,
                                          boolean restart, int port)
      throws ActivationException, RemoteException
  {
    ActivationDesc descriptor = new ActivationDesc(obj.getClass().getName(),
                                                   location, data, restart);
    ActivationID id = obtainId(descriptor);
    Remote stub = exportObject(obj, id, port);    
    return id;
  }

  /**
   * Register and export the object that activatable object that is not derived
   * from the Activatable super class. It creates and registers the object
   * activation descriptor. There is no need to call this method if the object
   * extends Activable, as its work is done in the constructor
   * {@link #Activatable(String, MarshalledObject, boolean, int, RMIClientSocketFactory, RMIServerSocketFactory)}
   * 
   * @param obj the object, that is exported, becoming available at the given
   *          port.
   * @param location the object code location (codebase).
   * @param data the data, needed to activate the object
   * @param restart the restart mode
   * @param port the port, where the object will be available
   * @param csf the client socket factory
   * @param ssf the server socket factory
   * 
   * @return the created object activation ID.
   * 
   * @throws ActivationException if the activation group is not active
   * @throws RemoteException if the registration or export fails
   */
  public static ActivationID exportObject(Remote obj, String location,
                                          MarshalledObject data,
                                          boolean restart, int port,
                                          RMIClientSocketFactory csf,
                                          RMIServerSocketFactory ssf)
      throws ActivationException, RemoteException
  {
    ActivationDesc descriptor = new ActivationDesc(obj.getClass().getName(),
                                                   location, data, restart);
    ActivationID id = obtainId(descriptor);
    Remote stub = exportObject(obj, id, port, csf, ssf);    
    return id;

  }

  /**
   * During activation, this exportObject method should be invoked explicitly by
   * the activatable object, that does is not derived from the Activatable
   * class. There is no need to call this method if the object extends
   * Activable, as its work is done in the constructor
   * {@link #Activatable(ActivationID, int)}
   * 
   * @param obj the object
   * @param id the known activation id
   * @param port the object port
   *  
   * @return the remote stub of the activatable object
   * 
   * @throws RemoteException if the object export fails
   */
  public static Remote exportObject(Remote obj, ActivationID id, int port)
      throws RemoteException
  {
    Remote stub = export(id, obj, port, null);
    return stub;
  }

  /**
   * During activation, this exportObject method should be invoked explicitly by
   * the activatable object, that does is not derived from the Activatable
   * class. There is no need to call this method if the object extends
   * Activable, as its work is done in the constructor
   * {@link #Activatable(ActivationID, int)}
   * 
   * @param obj the object
   * @param id the known activation id
   * @param port the object port
   * @param csf the client socket factory
   * @param ssf the server socket factory
   *  
   * @return the remote stub of the activatable object
   * 
   * @throws RemoteException if the object export fails
   */
  public static Remote exportObject(Remote obj, ActivationID id, int port,
                                    RMIClientSocketFactory csf,
                                    RMIServerSocketFactory ssf)
      throws RemoteException
  {
    Remote stub = export(id, obj, port, ssf); 
    return stub;

  }

  /**
   * Make the remote object unavailable for incoming calls. This method also
   * unregisters the object, so it cannot be activated again by incomming call
   * (unless registered).
   * 
   * @param obj the object to unexport
   * @param force if true, cancel all pending or running calls to that object
   *          (if false, the object with such calls is not unexported and false
   *          is returned by this method).
   * @return if the object was successfully unexported, false otherwise
   * @throws NoSuchObjectException if such object is not known
   */
  public static boolean unexportObject(Remote obj, boolean force)
      throws NoSuchObjectException
  {
    Object aref = UnicastServer.getExportedRef(obj);
    
    // Unregister it also (otherwise will be activated during the subsequent
    // call.
    if (aref instanceof ActivatableServerRef)
      {
        ActivatableServerRef aar = (ActivatableServerRef) aref;
        UnicastServer.unregisterActivatable(aar.actId);
      }
    return UnicastRemoteObject.unexportObject(obj, force);
  }
  
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
    // addStub(obj, stub); 
    // TODO Need to change the place of the stub repository
    return stub;
  }
  
  /**
   * Create and export the new remote object, making it available at the given
   * port, using sockets, produced by the specified factories.
   * 
   * @param port the port, on that the object should become available. Zero
   *          means anonymous port.
   * @param serverSocketFactory the server socket factory
   */
  private static Remote export(ActivationID id, Remote obj, int port,
			       RMIServerSocketFactory serverSocketFactory)
      throws RemoteException
  {
    ActivatableServerRef sref = null;
    sref = new ActivatableServerRef(makeId(id), id, port, serverSocketFactory);
    return sref.exportObject(obj);
  }  
  
  /**
   * Make the object ID from the activation ID. The same activation ID always
   * produces the identical object id.
   * 
   * @param aid the activation id
   * 
   * @return the object id
   */
  private static ObjID makeId(ActivationID aid)
  {
    ObjID id = new ObjID(0);
    
    // The fields of both ObjID and ActivationID must be package private,
    // so we need to use the reflection to access them anyway.
    // Probably other implementations use some very different approach.
    
    try
      {
        Field idUid =  ObjID.class.getDeclaredField("space");
        Field aidUid = ActivationID.class.getDeclaredField("uid");
        
        aidUid.setAccessible(true);
        idUid.setAccessible(true);
        
        idUid.set(id, aidUid.get(aid));
      }
    catch (Exception e)
      {
        InternalError ierr = new InternalError("Unable to set UID field");
        ierr.initCause(e);
        throw ierr;
      }
    
    return id;
  }  
  
  /**
   * Connect the object to the UnicastServer (export), but not activate it.
   * The object will be activated on the first call.
   */
  static Remote toStub(ActivationID anId, Class stubFor)
  {
    try
      {
        ActivatableServerRef asr = 
          new ActivatableServerRef(makeId(anId), anId, 0, null);
        UnicastServer.exportActivatableObject(asr);
        return asr.exportClass(stubFor);
      }
    catch (RemoteException e)
      {
        InternalError ierr = new InternalError(
          "Failed to obtain activatable stub");
        ierr.initCause(e);
        throw ierr;
      }
  }
}
