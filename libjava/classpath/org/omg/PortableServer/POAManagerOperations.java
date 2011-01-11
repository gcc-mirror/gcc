/* POAManagerOperations.java --
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package org.omg.PortableServer;

import org.omg.PortableServer.POAManagerPackage.AdapterInactive;
import org.omg.PortableServer.POAManagerPackage.State;

/**
 * Defines the operations, applicable to the {@link POAManager}.
 * These operations can turn the associated POAs to and from holding,
 * active and discarding states, but the incative state is irreversible.
 * The inactivated POAs can only be recreated after they were destroyed.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface POAManagerOperations
{
  /**
   * Turns the associated POAs into active state, allowing them to receive
   * and process requests.
   *
   * @throws AdapterInactive if the POAs are in the inactive state. If
   * once inactivated, the POA cannot be activated again.
   * This method can only be called to leave the holding or discarding state.
   */
  void activate()
         throws AdapterInactive;

  /**
   * <p>
   * Turns the asociated POAs into inactive state. The POAs in the incative
   * state will reject new requests. A cliet, trying to invoke an
   * object, belonging to the inactivated POA, will receive the remote exception
   * ({@link org.omg.CORBA.OBJ_ADAPTER}, minor code 0x535503ea, incomplete).
   * </p><p>
   * If the POA is once inactivated, it cannot be activated again.
   * The operation is used when the associated POAs are to be shut down.
   * </p>
   * <p>
   * Some independent implementations may set the minor code of the
   * OBJ_ADAPTER to 1, as recommended by OMG (formal/04-03-12).
   * The interoperable systems should expect any of these two values.
   * </p>
   *
   * @param etherealize_objects if true, the servant managers of the
   * associated POAs, having RETAIN and USE_SERVANT_MANAGER policies,
   * will receive a call of {@link ServantActivatorOperations#etherealize}.
   *
   * @param wait_for_completion if true, the method call suspends the current
   * thread till POAs complete the requests they are currently processing. If
   * false, the method returns immediately.
   * <p>
   *
   * @specnote The 0x535503ea is a Sun specific minor exception code 1002,
   * used for interoperability reasons.
   *
   * @throws AdapterInactive if the POAs are already in the inactive state.
   *
   * @see POAOperations#destroy
   */
  void deactivate(boolean etherealize_objects, boolean wait_for_completion)
           throws AdapterInactive;

  /**
   * <p>
   * Turns the associated POAs into discaring state. In this state, the POAs
   * discard the incoming requests. This mode is used in situations when
   * the server is flooded with requests. The client receives remote exception
   * ({@link org.omg.CORBA.TRANSIENT}, minor code 0x535503e9, incomplete).
   * </p><p>
   * Some independent implementations may set the minor code of the
   * TRANSIENT to 1, as recommended by OMG (formal/04-03-12).
   * The interoperable systems should expect any of these two values.
   * </p>
   *
   * @param wait_for_completion if true, the method call suspends the current
   * thread till POAs complete the requests they are currently processing. If
   * false, the method returns immediately.
   *
   * @specnote The 0x535503e9 is a Sun specific minor exception code 1001,
   * used for interoperability reasons.
   *
   * @throws AdapterInactive if the POAs are in the inactive state.
   */
  void discard_requests(boolean wait_for_completion)
                 throws AdapterInactive;

  /**
   * Get the state of the POA manager.
   */
  State get_state();

  /**
   * Turns the associated POAs into holding state. In this state, the POAs
   * queue incoming requests but do not process them.
   *
   * @param wait_for_completion if true, the method call suspends the current
   * thread till POAs complete the requests they are currently processing. If
   * false, the method returns immediately.

   * @throws AdapterInactive if the POAs are in the inactive state.
   */
  void hold_requests(boolean wait_for_completion)
              throws AdapterInactive;
}
