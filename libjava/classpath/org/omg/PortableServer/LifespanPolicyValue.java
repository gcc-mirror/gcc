/* LifespanPolicyValue.java --
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

import gnu.CORBA.Minor;

import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
* Specifies the object life span policy.
*
* This enumeration can obtain the following values:
* <ul>
* <li>TRANSIENT The objects implemented in the POA never outlive
*  the POA instance in which they are first created.</li>
* <li>PERSISTENT The mentioned objects can outlive the process in
* which they are first created.</li>
* </ul>
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public class LifespanPolicyValue
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (V1.4) for interoperability.
   */
  private static final long serialVersionUID = 6604562925399671611L;

  /**
   * The value field for the current instance.
   */
  private final int _value;

  /**
  * The possible value of this enumeration (TRANSIENT).
  */
  public static final int _TRANSIENT = 0;

  /**
   * An instance of LifespanPolicyValue, initialized to TRANSIENT.
   */
  public static final LifespanPolicyValue TRANSIENT =
    new LifespanPolicyValue(_TRANSIENT);

  /**
  * The possible value of this enumeration (PERSISTENT).
  */
  public static final int _PERSISTENT = 1;

  /**
   * An instance of LifespanPolicyValue, initialized to PERSISTENT.
   */
  public static final LifespanPolicyValue PERSISTENT =
    new LifespanPolicyValue(_PERSISTENT);

  /**
   * The private array that maps integer codes to the enumeration
   * values.
   */
  private static final LifespanPolicyValue[] enume =
    new LifespanPolicyValue[] { TRANSIENT, PERSISTENT };

  /**
   * The private array of state names.
   */
  private static final String[] state_names =
    new String[] { "TRANSIENT", "PERSISTENT" };

  /**
   * Normally, no new instances are required, so the constructor is protected.
   */
  protected LifespanPolicyValue(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the LifespanPolicyValue, matching the given integer constant.
   *
   * @param code one of _TRANSIENT, _PERSISTENT.
   * @return one of TRANSIENT, PERSISTENT.
   * @throws BAD_PARAM if the parameter is not one of the valid values.
   */
  public static LifespanPolicyValue from_int(int code)
  {
    try
      {
        return enume [ code ];
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Invalid policy code " + code);
        bad.minor = Minor.PolicyType;
        throw bad;
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
   * Returns the integer code of the policy value.
   * @return _TRANSIENT or _PERSISTENT.
   */
  public int value()
  {
    return _value;
  }
}