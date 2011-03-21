/* Activator.java --
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


package java.rmi.activation;

import java.rmi.MarshalledObject;
import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Activates remote object, providing the live reference to the activable remote
 * object. Usually there is only one activator per host.
 *
 * @see ActivationSystem
 * @see ActivationMonitor
 */
public interface Activator
  extends Remote
{
  /**
   * Activate the object, associated with the given activation identifier. The
   * activator looks for the {@link ActivationDesc}riptor for the passed
   * identifier, determines the object activation group and initiates object
   * recreation either via {@link ActivationInstantiator} or via
   * {@link Class#newInstance()}.
   *
   * @param id the identifier of the object to activate.
   * @param force if true, the activator always contacts the group to obtain the
   *          reference. If false, it may return the cached value.
   * @return the activated remote object (its stub).
   * @throws UnknownObjectException if the object with this id is unknown
   * @throws ActivationException if the activation has failed due other reason
   * @throws RemoteException if the remote call has failed.
   */
  MarshalledObject<? extends Remote> activate (ActivationID id, boolean force)
    throws ActivationException, UnknownObjectException, RemoteException;
}
