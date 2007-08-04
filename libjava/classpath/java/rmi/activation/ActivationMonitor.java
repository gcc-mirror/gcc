/* ActivationMonitor.java -- the RMI activation/inactivation event listener
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

import java.rmi.MarshalledObject;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * The activation and inactivation event listener. The group obtains this 
 * listener via {@link ActivationSystem#activeGroup} and must notify it
 * when the group objects are activated or inactivated and also when the 
 * whole group becomes inactive.
 * @author root.
 */
public interface ActivationMonitor extends Remote
{
  /**
   * Informs that the object is now active.
   * 
   * @param id the activation id of the object that is now active
   * @throws UnknownObjectException is such object is not known in this group
   * @throws RemoteException if remote call fails
   */
  void activeObject (ActivationID id, MarshalledObject<? extends Remote> obj)
    throws UnknownObjectException, RemoteException;

  /**
   * Informs that the object is not inactive.
   * 
   * @param id the activation id of the object that is now inactive
   * @throws UnknownObjectException is such object is not known in this group
   * @throws RemoteException if remote call fails
   */
  void inactiveObject (ActivationID id)
    throws UnknownObjectException, RemoteException;

  /**
   * Informs that the whole group is now inactive because all group objects are
   * inactive. The group will be recreated upon the later request to activate
   * any object, belonging to the group.
   * 
   * @param groupId the group id
   * @param incarnation the group incarnation number
   * @throws UnknownGroupException if the group id is not known
   * @throws RemoteException if the remote call fails
   */
  void inactiveGroup (ActivationGroupID groupId, long incarnation)
  throws UnknownGroupException, RemoteException;
}
