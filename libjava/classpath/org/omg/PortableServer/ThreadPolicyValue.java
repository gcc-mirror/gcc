/* ThreadPolicyValue.java --
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

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * Defines the possible values for the POA thread policy.
 *
 * This enumeration can obtain the following values:
 * <ul>
 * <li>ORB_CTRL_MODEL Each object in POA has a separate serving thread
 * and a separate server socket, listening on the objects individual
 * port. Additionally, when the request is accepted, it is also
 * served in a separate thread, so several requests to the same
 * object can be processed in parallel. The servant can always get
 * the Id and POA of the object it is currently serving by
 * invoking {@link Servant#_object_id()} and {@link Servant#_poa}.
 * These two methods use thread to data map and must work correctly
 * even then the servant code is executed in several parallel threads.
 * </li>
 * <li>SINGLE_THREAD_MODEL All objects in POA share the same server
 * socket and are served in the same thread. This model is applicable
 * when the number of objects is greater than the number of threads
 * and (or) ports, supported by the system.</li>
 * </ul>
 * OMG also defines a MAIN_THREAD_MODEL, currently not supported by
 * the java API.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class ThreadPolicyValue
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -8874988828297141512L;

  /**
   * The value field for the current instance.
   */
  private final int _value;

  /**
  * The possible value of this enumeration (ORB_CTRL_MODEL).
  */
  public static final int _ORB_CTRL_MODEL = 0;

  /**
   * An instance of ThreadPolicyValue, initialized to ORB_CTRL_MODEL.
   */
  public static final ThreadPolicyValue ORB_CTRL_MODEL =
    new ThreadPolicyValue(_ORB_CTRL_MODEL);

  /**
  * The possible value of this enumeration (SINGLE_THREAD_MODEL).
  */
  public static final int _SINGLE_THREAD_MODEL = 1;

  /**
   * An instance of ThreadPolicyValue, initialized to SINGLE_THREAD_MODEL.
   */
  public static final ThreadPolicyValue SINGLE_THREAD_MODEL =
    new ThreadPolicyValue(_SINGLE_THREAD_MODEL);

  /**
   * The private array that maps integer codes to the enumeration
   * values.
   */
  private static final ThreadPolicyValue[] enume =
    new ThreadPolicyValue[] { ORB_CTRL_MODEL, SINGLE_THREAD_MODEL };

  /**
   * The private array of state names.
   */
  private static final String[] state_names =
    new String[] { "ORB_CTRL_MODEL", "SINGLE_THREAD_MODEL" };

  /**
   * Normally, no new instances are required, so the constructor is protected.
   */
  protected ThreadPolicyValue(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the ThreadPolicyValue, matching the given integer constant.
   *
   * @param code one of _ORB_CTRL_MODEL, _SINGLE_THREAD_MODEL.
   * @return one of ORB_CTRL_MODEL, SINGLE_THREAD_MODEL.
   * @throws BAD_PARAM if the parameter is not one of the valid values.
   */
  public static ThreadPolicyValue from_int(int code)
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
   * @return one of ORB_CTRL_MODEL, SINGLE_THREAD_MODEL.
   */
  public int value()
  {
    return _value;
  }
}