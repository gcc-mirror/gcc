/* ServantRetentionPolicyValue.java --
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
* Specifies the servant retention policy.
*
* This enumeration can obtain the following values:
* <ul>
* <li>RETAIN The POA retains active servants in its Active Object Map.</li>
* <li>NON_RETAIN The servants are not retained.</li>
* </ul>
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public class ServantRetentionPolicyValue
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (V1.4) for interoperability.
   */
  private static final long serialVersionUID = -7476100336036943822L;

  /**
   * The value field for the current instance.
   */
  private final int _value;

  /**
  * The possible value of this enumeration (RETAIN).
  */
  public static final int _RETAIN = 0;

  /**
   * An instance of ServantRetentionPolicyValue, indicating that
   * the POA retains active servants in its Active Object Map.
   */
  public static final ServantRetentionPolicyValue RETAIN =
    new ServantRetentionPolicyValue(_RETAIN);

  /**
  * The possible value of this enumeration (NON_RETAIN).
  */
  public static final int _NON_RETAIN = 1;

  /**
   * An instance of ServantRetentionPolicyValue, indicating that
   * the POA does not use the Active Object Map.
   */
  public static final ServantRetentionPolicyValue NON_RETAIN =
    new ServantRetentionPolicyValue(_NON_RETAIN);

  /**
   * The private array that maps integer codes to the enumeration
   * values.
   */
  private static final ServantRetentionPolicyValue[] enume =
    new ServantRetentionPolicyValue[] { RETAIN, NON_RETAIN };

  /**
   * The private array of state names.
   */
  private static final String[] state_names =
    new String[] { "RETAIN", "NON_RETAIN" };

  /**
   * Normally, no new instances are required, so the constructor is protected.
   */
  protected ServantRetentionPolicyValue(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the ServantRetentionPolicyValue, matching the given integer constant.
   *
   * @param code one of _RETAIN, _NON_RETAIN.
   * @return one of RETAIN, NON_RETAIN.
   * @throws BAD_PARAM if the parameter is not one of the valid values.
   */
  public static ServantRetentionPolicyValue from_int(int code)
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
   * Returns the integer code of the policy value.
   * @return _RETAIN or _NON_RETAIN.
   */
  public int value()
  {
    return _value;
  }
}