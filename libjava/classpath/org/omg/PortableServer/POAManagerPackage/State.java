/* State.java --
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


package org.omg.PortableServer.POAManagerPackage;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * Defines the possible states of the POA manager.
 *
 * This enumeration can obtain the following values:
 * <ul>
 * <li>ACTIVE When the manager is in the active state, the associated POAs
 * receive and process requests.</li>
 * <li>DISCARDING When the manager is in the discarding state,
 * the associated POAs discard all incoming requests. The sending clients
 * receive the {@link org.omg.TRANSIENT} system exception, with standard
 * minor code 1. This mode is needed for flow control, when the system is
 * flooded with requests.
 * </li>
 * <li>HOLDING When the manager is in the holding state, the associated POAs
 * queue incoming requests.</li>
 * <li>INACTIVE This state is entered when the associated POAs are to be
 *  shut down.</li>
 * </ul>
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class State
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -2451866258678193271L;

  /**
   * The value field for the current instance.
   */
  private final int _value;

  /**
  * The possible value of this enumeration (HOLDING).
  */
  public static final int _HOLDING = 0;

  /**
   * An instance of State, initialized to HOLDING.
   */
  public static final State HOLDING = new State(_HOLDING);

  /**
  * The possible value of this enumeration (ACTIVE).
  */
  public static final int _ACTIVE = 1;

  /**
   * An instance of State, initialized to ACTIVE.
   */
  public static final State ACTIVE = new State(_ACTIVE);

  /**
  * The possible value of this enumeration (DISCARDING).
  */
  public static final int _DISCARDING = 2;

  /**
   * An instance of State, initialized to DISCARDING.
   */
  public static final State DISCARDING = new State(_DISCARDING);

  /**
  * The possible value of this enumeration (INACTIVE).
  */
  public static final int _INACTIVE = 3;

  /**
   * An instance of State, initialized to INACTIVE.
   */
  public static final State INACTIVE = new State(_INACTIVE);

  /**
   * The private array that maps integer codes to the enumeration
   * values.
   */
  private static final State[] enume =
    new State[] { HOLDING, ACTIVE, DISCARDING, INACTIVE };

  /**
   * The private array of state names.
   */
  private static final String[] state_names =
    new String[] { "HOLDING", "ACTIVE", "DISCARDING", "INACTIVE" };

  /**
   * Normally, no new instances are required, so the constructor is protected.
   */
  protected State(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the State, matching the given integer constant.
   *
   * @param code one of _HOLDING, _ACTIVE, _DISCARDING, _INACTIVE.
   * @return one of HOLDING, ACTIVE, DISCARDING, INACTIVE.
   * @throws BAD_PARAM if the parameter is not one of the valid values.
   */
  public static State from_int(int code)
  {
    try
      {
        return enume [ code ];
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        throw new BAD_OPERATION("Invalid enumeration code " + code);
      }
  }

  /**
   * Returns a short string representation.
   * @return the name of the current enumeration value.
   */
  public String toString()
  {
    return state_names [ _value ];
  }

  /**
   * Returns the integer code of the enumeration value.
   * @return one of HOLDING, ACTIVE, DISCARDING, INACTIVE.
   */
  public int value()
  {
    return _value;
  }
}