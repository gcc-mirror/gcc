/* gnuPOAManager.java --
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


package gnu.CORBA.Poa;

import org.omg.CORBA.BAD_INV_ORDER;
import org.omg.CORBA.LocalObject;
import org.omg.PortableServer.POAManager;
import org.omg.PortableServer.POAManagerPackage.AdapterInactive;
import org.omg.PortableServer.POAManagerPackage.State;

import java.util.HashSet;
import java.util.Iterator;

/**
 * The implementation of the POA manager. The manager is a controlled
 * switch that can change its states in response to the method calls
 * and throw exceptions if the requested change is invalid. It is possible
 * to check the state this switch. It does not do anything else.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class gnuPOAManager
  extends LocalObject
  implements POAManager
{
  /**
   * The POAs, controlled by this manager. The members must be instances of
   * the gnuAbstractPOA.
   */
  HashSet POAs = new HashSet();

  /**
   * The state of the manager. The newly created manager is always
   * in the holding state.
   */
  State state = State.HOLDING;

  /**
   * Get the state of the POA manager.
   */
  public State get_state()
  {
    return state;
  }

  /**
   * Turns the associated POAs into active state, allowing them to receive
   * and process requests.
   *
   * @throws if the POAs are in the inactive state. If once inactivated,
   * the POA cannot be activated again. This method can only be called
   * to leave the holding or discarding state.
   */
  public void activate()
                throws AdapterInactive
  {
    if (state != State.INACTIVE)
      state = State.ACTIVE;
    else
      throw new AdapterInactive();
  }

  /**
   * Turns the associated POAs into holding state. In this state, the POAs
   * queue incoming requests but do not process them.
   *
   * @param wait_for_completion if true, the method call suspends the current
   * thread till POAs complete the requests they are currently processing. If
   * false, the method returns immediately.

   * @throws AdapterInactive if the POAs are in the inactive state.
   */
  public void hold_requests(boolean wait_for_completion)
                     throws AdapterInactive
  {
    if (state != State.INACTIVE)
      state = State.HOLDING;
    else
      throw new AdapterInactive();
    if (wait_for_completion)
      waitForIdle();
  }

  /**
   *
   * Turns the asociated POAs into inactive state. The POAs in the incative
   * state will reject new requests. If the POA is once inactivated, it
   * cannot be activated again. The operation is used when
   * the associated POAs are to be shut down.
   *
   * @param etherealize_objects if true, the servant managers of the
   * associated POAs, having RETAIN and USE_SERVANT_MANAGER policies,
   * will receive a call of {@link ServantActivatorOperations#etherealize}.
   *
   * @param wait_for_completion if true, the method call suspends the current
   * thread till POAs complete the requests they are currently processing. If
   * false, the method returns immediately.
   *
   * @throws AdapterInactive if the POAs are already in the inactive state.
   *
   * @see POAOperations#destroy
   */
  public void deactivate(boolean etherealize_objects,
                         boolean wait_for_completion
                        )
                  throws AdapterInactive
  {
    if (state == State.INACTIVE)
      throw new AdapterInactive("Repetetive inactivation");
    state = State.INACTIVE;
    if (wait_for_completion)
      waitForIdle();

    Iterator iter = POAs.iterator();
    while (iter.hasNext())
      {
        gnuPOA poa = (gnuPOA) iter.next();

        // If the servant activator is non null, this means it has been
        // set - hence the policies are appropriate.
        if (poa.servant_activator != null)
          poa.etherealizeAll();
      }
  }

  /**
   * Turns the associated POAs into discaring state. In this state, the POAs
   * discard the incoming requests. This mode is used in situations when
   * the server is flooded with requests. The client receives remote exception
   * ({@link org.omg.CORBA.TRANSIENT}, minor code 1).
   *
   * @param wait_for_completion if true, the method call suspends the current
   * thread till POAs complete the requests they are currently processing. If
   * false, the method returns immediately.

   * @throws AdapterInactive if the POAs are in the inactive state.
   */
  public void discard_requests(boolean wait_for_completion)
                        throws AdapterInactive
  {
    if (state != State.INACTIVE)
      state = State.DISCARDING;
    else
      throw new AdapterInactive();
    if (wait_for_completion)
      waitForIdle();
  }

  /**
   * Suspend the current thread while at least one of the associated POA is
   * actively processing some requests. The method assumes that the POAs
   * are not accepting the <i>new</i> requests due manager state.
   *
   * @throws BAD_INV_ORDER if the POAs are in the active state.
   */
  public void waitForIdle()
  {
    if (state == State.ACTIVE)
      throw new BAD_INV_ORDER("The state is active");

    Iterator iter = POAs.iterator();
    while (iter.hasNext())
      {
        gnuPOA poa = (gnuPOA) iter.next();
        poa.waitWhileRunning();
      }
  }

  /**
   * Add the POA that will be controlled by this manager.
   *
   * @param poa the POA.
   */
  public void addPoa(gnuPOA poa)
  {
    POAs.add(poa);
  }

  /**
   * Remove the POA, releasing it from the control of this manager.
   * Called in POA finaliser.
   *
   * @param poa the POA to remove.
   */
  public void removePOA(gnuPOA poa)
  {
    POAs.remove(poa);
  }
}