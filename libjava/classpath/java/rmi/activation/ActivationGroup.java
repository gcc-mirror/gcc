/* ActivationGroup.java -- the RMI activation group.
   Copyright (c) 1996, 1997, 1998, 1999, 2006 Free Software Foundation, Inc.

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

import gnu.java.rmi.activation.DefaultActivationGroup;
import gnu.java.rmi.activation.DefaultActivationSystem;

import java.lang.reflect.Constructor;
import java.rmi.MarshalledObject;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

/**
 * The entity that receives the request to activate object and activates it.
 * Frequently there is one activation group per virtual machine.
 * 
 * @author Audrius Meskauskas (audriusa@Bioinformatics.org) (from stub)
 */
public abstract class ActivationGroup
    extends UnicastRemoteObject
    implements ActivationInstantiator
{

  /**
   * Use the SVUID for interoperability.
   */
  static final long serialVersionUID = - 7696947875314805420L;
  
  /**
   * The Id of the current group on this VM (null if none).
   */
  static ActivationGroupID currentGroupId = null;  
  
  /**
   * The groups identifier.
   */
  final ActivationGroupID groupId;

  /**
   * The groups activation monitor.
   */
  ActivationMonitor monitor;
  
  /**
   * The groups incarnation number.
   */
  long incarnation;
 
  /**
   * The groups activation system.
   */
  static ActivationSystem system;
  
  /**
   * Used during the group creation (required constructor).
   */
  static final Class[] cConstructorTypes = new Class[]
                                                   {
                                                    ActivationGroupID.class,
                                                    MarshalledObject.class
                                                   };

  /**
   * Create the new activation group with the given group id. 
   * 
   * @param aGroupId the group Id.
   * 
   * @throws RemoteException if the group export fails.
   */
  protected ActivationGroup(ActivationGroupID aGroupId) throws RemoteException
  {
    groupId = aGroupId;
  }
  
  /**
   * The method is called when the object is exported. The group must notify
   * the activation monitor, if this was not already done before.
   *  
   * @param id the object activation id
   * @param obj the remote object implementation
   * 
   * @throws ActivationException if the group is inactive
   * @throws UnknownObjectException if such object is not known
   * @throws RemoteException if the call to monitor fails
   */
  public abstract void activeObject(ActivationID id, Remote obj)
      throws ActivationException, UnknownObjectException, RemoteException;
  
  /**
   * Notifies the monitor about the object being inactivated.
   * 
   * @param id the object being inactivated.
   * @return true always (must be overridden to return other values).
   * @throws ActivationException never
   * @throws UnknownObjectException if the object is not known
   * @throws RemoteException if the remote call to monitor fails
   */
  public boolean inactiveObject(ActivationID id) throws ActivationException,
      UnknownObjectException, RemoteException
  {
    if (monitor != null)
      monitor.inactiveObject(id);
    return true;
  }

  /**
   * Create the new instance of the activation group, using the class name and
   * location information, stored in the passed descriptor. The method expects
   * the group class to have the two parameter constructor, the first parameter
   * being the {@link ActivationGroupID} and the second the
   * {@link MarshalledObject}. The group must be first be registered with the
   * ActivationSystem. Once a group is created, the currentGroupID method
   * returns the identifier for this group until the group becomes inactive.
   * 
   * @param id the activation group id
   * @param desc the group descriptor, providing the information, necessary to
   *          create the group
   * @param incarnation the incarnation number
   * @return the created group instance
   * @throws ActivationException if the activation fails due any reason
   */
  public static ActivationGroup createGroup(ActivationGroupID id,
                                            ActivationGroupDesc desc,
                                            long incarnation)
      throws ActivationException
  {
    // If the activation system is not yet set, set it to the system.
    // passed in the group id.
    if (system == null)
      system = id.system;
    
    ActivationGroup group = null;

    // TODO at the moment all groups are created on the current jre and the
    // group class must be reachable via thread context class loader.
    Class groupClass;

    if (desc.className != null)
      {
        try
          {
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
            groupClass = loader.loadClass(desc.className);
          }
        catch (ClassNotFoundException e)
          {
            ActivationException acex = new ActivationException(
              "Cannot load " + desc.className);
            acex.detail = e;
            throw acex;
          }
      }
    else
      groupClass = DefaultActivationGroup.class;

    try
      {
        Constructor constructor = groupClass.getConstructor(cConstructorTypes);
        group = (ActivationGroup) constructor.newInstance(
          new Object[] { id, desc.data });
      }
    catch (Exception e)
      {
        ActivationException acex = new ActivationException(
          "Cannot instantiate " + desc.className);
        acex.detail = e;
        throw acex;
      }

    currentGroupId = id;
    try
      {
        group.monitor = getSystem().activeGroup(id, group, incarnation);
        return group;
      }
    catch (RemoteException e)
      {
        ActivationException acex = new ActivationException("createGroup");
        acex.detail = e;
        throw acex;
      }
  }

  /**
   * Get the id of current activation group.
   * 
   * @return the id of the current activation group or null if none exists.
   */
  public static ActivationGroupID currentGroupID()
  {
    try
      {
        if (currentGroupId==null)
          {
            // This will also assing the currentGroupId to the current
            // (default) group of the default system.
            setSystem(DefaultActivationSystem.get());
          }
      }
    catch (ActivationException e)
      {
        InternalError ierr = new InternalError("Unable to activate AS");
        ierr.initCause(e);
        throw ierr;
      }
      
    return currentGroupId;
  }

  /**
   * Set the activation system for this virtual machine. The system can only
   * be set if no group is active. 
   * 
   * @param aSystem the system to set
   *  
   * @throws ActivationException if some group is active now.
   */
  public static void setSystem(ActivationSystem aSystem)
      throws ActivationException
  {
    if (currentGroupId!=null)
      throw new ActivationException("Group active");
    else
      {
        try 
          {
            // Register the default transient activation system and group.
            system = aSystem;
            ActivationGroupDesc def = new ActivationGroupDesc(
               DefaultActivationGroup.class.getName(),
              "",
              null,
              null,
              null);
            currentGroupId = system.registerGroup(def);
          }
        catch (Exception ex)
        {
          InternalError ierr = new InternalError("Unable to start default AG");
          ierr.initCause(ex);
          throw ierr;
        }
      }  
  }
  
  /**
   * Get the current activation system. If the system is not set via
   * {@link #setSystem} method, the default system for this virtual machine is
   * returned. The default system is first searched by name
   * "java.rmi.activation.ActivationSystem" on the activation registry port. The
   * default value of the activation registry port is
   * {@link ActivationSystem#SYSTEM_PORT}, but it can be changed by putting the
   * system property java.rmi.activation.port. Both activation system and
   * activation registry are provided by the RMI daemon tool, RMID, if it is
   * running on the local host. If the RMID is not running, the internal
   * transient activation system will be created and returned. This internal
   * system is highly limited in in capabilities and is not intended to be used
   * anywhere apart automated testing.
   * 
   * @return the activation system for this virtual machine
   * @throws ActivationException
   */
  public static ActivationSystem getSystem() throws ActivationException
  {
    if (system == null)
      system = DefaultActivationSystem.get();
    return system;
  }

  /**
   * Makes the call back to the groups {@link ActivationMonitor}.
   * 
   * @param id the id obj the object being activated
   * @param mObject the marshalled object, contains the activated remote object
   *          stub.
   * @throws ActivationException on activation error
   * @throws UnknownObjectException if such object is not registered
   * @throws RemoteException on remote call (to monitor) error
   */
  protected void activeObject(ActivationID id,
			      MarshalledObject<? extends Remote> mObject)
      throws ActivationException, UnknownObjectException, RemoteException
  {
    if (monitor!=null)
      monitor.activeObject(id, mObject);
    
    id.group = this;
  }

  /**
   * Makes the call back to the groups {@link ActivationMonitor} and sets
   * the current group to null.
   */ 
  protected void inactiveGroup() throws UnknownGroupException, RemoteException
  {
    if (monitor!=null)
      monitor.inactiveGroup(groupId, incarnation);
    
    if (currentGroupId!=null && currentGroupId.equals(groupId))
      currentGroupId = null;
  }

}
