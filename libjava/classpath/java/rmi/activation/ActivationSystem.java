/* ActivationSystem.java -- registers groups and objects to be activated.
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

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * <p>
 * The ActivationSystem registers groups and activatable objects to be activated
 * within those groups. The ActivationSystem cooperates with both the Activator,
 * which activates objects registered via the ActivationSystem, and the
 * ActivationMonitor, which obtains information about active and inactive
 * objects and inactive groups.
 * </p>
 * <p>
 * The activation system if frequently a remote object. As a security mean, all
 * methods in this interface throw {@link java.rmi.AccessException} if called
 * from the client that is not reside on the same host as the activation system.
 * </p>
 * @see ActivationGroup#getSystem()
 */
public interface ActivationSystem
    extends Remote
{
  /**
   * The port, used by the activation system. The value is equal to 1098 by
   * default, but it can be changed by putting the system property
   * .
   */
  int SYSTEM_PORT = 1098;

  /**
   * Registers the activation descriptor and creates (and returns) its
   * activation identifier. The map entry (identifier to descriptor) is stored
   * in the stable map and used when the {@link Activator} receives the request
   * to activate the object.
   *
   * @param desc the activation descriptor to register.
   * @return the created activation identifier that is mapped to the passed
   *         descriptor.
   * @throws ActivationException if the registration fails (database update
   *           problems, etc).
   * @throws UnknownGroupException the if group, specified in decriptor, is
   *           unknown.
   * @throws RemoteException if the remote call fails.
   */
  ActivationID registerObject(ActivationDesc desc) throws ActivationException,
      UnknownGroupException, RemoteException;

  /**
   * Removes the stored identifier-description map entry. The object will no
   * longer be activable using the passed activation id
   *
   * @param id the activation id to remove
   * @throws ActivationException if the entry removing operation failed
   *           (database update problems, etc)
   * @throws UnknownObjectException if the passed id is not known to the system
   * @throws RemoteException if the remote call fails
   */
  void unregisterObject(ActivationID id) throws ActivationException,
      UnknownObjectException, RemoteException;

  /**
   * Register the new activation group. For instance, it can be one activation
   * group per virtual machine.
   *
   * @param groupDesc the activation group descriptor.
   * @return the created activation group ID for the activation group
   * @throws ActivationException if the group registration fails
   * @throws RemoteException if the remote call fails
   */
  ActivationGroupID registerGroup(ActivationGroupDesc groupDesc)
      throws ActivationException, RemoteException;

  /**
   * This method is called from the {@link ActivationGroup} to inform the
   * ActivatinSystem that the group is now active and there is the
   * {@link ActivationInstantiator} for that group. This call is made internally
   * from the {@link ActivationGroup#createGroup}.
   *
   * @param id the group id
   * @param group the group activation instantiator
   * @param incarnation the groups incarnatin number.
   * @return the activation monitor that should be informed about the group
   *         state changes
   * @throws UnknownGroupException if this group has not been registered
   * @throws ActivationException if this group is already active
   * @throws RemoteException if the remote call fails
   */
  ActivationMonitor activeGroup(ActivationGroupID id,
                                ActivationInstantiator group, long incarnation)
      throws UnknownGroupException, ActivationException, RemoteException;

  /**
   * Removes the activation group with the given identifier. The group calls
   * back, informing the activator about the shutdown.
   *
   * @param id the group activation id.
   * @throws ActivationException if the database update fails
   * @throws UnknownGroupException if such group is not registered
   * @throws RemoteException if the remote call fails
   */
  void unregisterGroup(ActivationGroupID id) throws ActivationException,
      UnknownGroupException, RemoteException;

  /**
   * Shutdown the activation system and all associated activation groups
   *
   * @throws RemoteException if the remote call fails
   */
  void shutdown() throws RemoteException;

  /**
   * Replace the activation descriptor for the object with the given activation
   * id.
   *
   * @param id the activation id
   * @param desc the new activation descriptor
   * @return the previous activation descriptor for that object.
   * @throws ActivationException if the database update fails
   * @throws UnknownObjectException if the object with such id is not known
   * @throws UnknownGroupException if the activation group (in desc) is not
   *           known.
   * @throws RemoteException if the remote call fails
   */
  ActivationDesc setActivationDesc(ActivationID id, ActivationDesc desc)
      throws ActivationException, UnknownObjectException,
      UnknownGroupException, RemoteException;

  /**
   * Replaces the group descriptor for the group with the given group activation
   * id.
   *
   * @param groupId the group id
   * @param groupDesc the new group descriptor
   * @return the previous group descriptor
   * @throws ActivationException if the database update fails
   * @throws UnknownGroupException if such group is not known
   * @throws RemoteException if the remote call fails
   */
  ActivationGroupDesc setActivationGroupDesc(ActivationGroupID groupId,
                                             ActivationGroupDesc groupDesc)
      throws ActivationException, UnknownGroupException, RemoteException;

  /**
   * Get the activation descriptor for the object with the given activation id.
   *
   * @param id the object activation id
   * @return the activation descriptor for that object
   * @throws ActivationException if the database access fails
   * @throws UnknownObjectException if this object is not known
   * @throws RemoteException if the remote call fails
   */
  ActivationDesc getActivationDesc(ActivationID id) throws ActivationException,
      UnknownObjectException, RemoteException;

  /**
   * Get the group descriptor for the group with the given id.
   *
   * @param groupId the group id
   * @return the group descriptor
   * @throws ActivationException if the database access fails
   * @throws UnknownGroupException if the group with such id is not known
   * @throws RemoteException if the remote call fails
   */
  ActivationGroupDesc getActivationGroupDesc(ActivationGroupID groupId)
      throws ActivationException, UnknownGroupException, RemoteException;
}
