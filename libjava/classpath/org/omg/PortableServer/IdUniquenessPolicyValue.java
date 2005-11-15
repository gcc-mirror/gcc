/* IdUniquenessPolicyValue.java --
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
* Specifies the Object Id uniqueness policy.
*
* This enumeration can obtain the following values:
* <ul>
* <li>UNIQUE_ID a servant activated with that POA supports exactly one Object Id.
* </li>
* <li>MULTIPLE_ID a servant activated with that POA supports
* multiple Object Ids.
* </li>
* </ul>
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public class IdUniquenessPolicyValue
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (V1.4) for interoperability.
   */
  private static final long serialVersionUID = 2698951826884611346L;

  /**
   * The value field for the current instance.
   */
  private final int _value;

  /**
  * The possible value of this enumeration (UNIQUE_ID).
  */
  public static final int _UNIQUE_ID = 0;

  /**
   * Indicates that a servant activated with that POA, supports one and only
   * one OBject Id.
   */
  public static final IdUniquenessPolicyValue UNIQUE_ID =
    new IdUniquenessPolicyValue(_UNIQUE_ID);

  /**
  * The possible value of this enumeration (MULTIPLE_ID).
  */
  public static final int _MULTIPLE_ID = 1;

  /**
   * Indicates that a servant activated with that POA is able to support the
   * multiple Object Ids.
   */
  public static final IdUniquenessPolicyValue MULTIPLE_ID =
    new IdUniquenessPolicyValue(_MULTIPLE_ID);

  /**
   * The private array that maps integer codes to the enumeration
   * values.
   */
  private static final IdUniquenessPolicyValue[] enume =
    new IdUniquenessPolicyValue[] { UNIQUE_ID, MULTIPLE_ID };

  /**
   * The private array of state names.
   */
  private static final String[] state_names =
    new String[] { "UNIQUE_ID", "MULTIPLE_ID" };

  /**
   * Normally, no new instances are required, so the constructor is protected.
   */
  protected IdUniquenessPolicyValue(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the IdUniquenessPolicyValue, matching the given integer constant.
   *
   * @param code one of _UNIQUE_ID, _MULTIPLE_ID.
   * @return one of UNIQUE_ID, MULTIPLE_ID.
   * @throws BAD_PARAM if the parameter is not one of the valid values.
   */
  public static IdUniquenessPolicyValue from_int(int code)
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
   * @return _UNIQUE_ID or _MULTIPLE_ID.
   */
  public int value()
  {
    return _value;
  }
}