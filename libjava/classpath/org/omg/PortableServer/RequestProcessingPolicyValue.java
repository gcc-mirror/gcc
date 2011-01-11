/* RequestProcessingPolicyValue.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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
import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
* Specifies the behaviour in the case when the
* requested object is not found in the Active Object Map or that map
* is not in use. The map is not in use when the
* {@link ServantRetentionPolicyValue#NON_RETAIN} policy is active.
* <ul>
* <li>USE_ACTIVE_OBJECT_MAP_ONLY Raise an {@link org.omg.CORBA.OBJECT_NOT_EXIST}
* with the minor code 2. {@link ServantRetentionPolicyValue#RETAIN} policy is
* also required.
* </li>
* <li>USE_DEFAULT_SERVANT Dispatch request to the default servant. If no such
* exists, raise {@link org.omg.CORBA.OBJ_ADAPTER} with minor code 3.
* {@link IdUniquenessPolicyValue#MULTIPLE_ID} is also required.
* </li>
* <li>USE_SERVANT_MANAGER Dispatch request to the servant manager. If no such
* exists, raise {@link org.omg.CORBA.OBJ_ADAPTER} with the minor code 4.</li>
* </ul>
*
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public class RequestProcessingPolicyValue
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (V1.4) for interoperability.
   */
  private static final long serialVersionUID = 7646563512329907695L;

  /**
   * The value field for the current instance.
   */
  private final int _value;

  /**
  * The possible value of this enumeration (USE_ACTIVE_OBJECT_MAP_ONLY).
  */
  public static final int _USE_ACTIVE_OBJECT_MAP_ONLY = 0;

  /**
   * An instance of RequestProcessingPolicyValue, initialized to USE_ACTIVE_OBJECT_MAP_ONLY.
   */
  public static final RequestProcessingPolicyValue USE_ACTIVE_OBJECT_MAP_ONLY =
    new RequestProcessingPolicyValue(_USE_ACTIVE_OBJECT_MAP_ONLY);

  /**
  * The possible value of this enumeration (USE_DEFAULT_SERVANT).
  */
  public static final int _USE_DEFAULT_SERVANT = 1;

  /**
   * An instance of RequestProcessingPolicyValue, initialized to USE_DEFAULT_SERVANT.
   */
  public static final RequestProcessingPolicyValue USE_DEFAULT_SERVANT =
    new RequestProcessingPolicyValue(_USE_DEFAULT_SERVANT);

  /**
  * The possible value of this enumeration (USE_SERVANT_MANAGER).
  */
  public static final int _USE_SERVANT_MANAGER = 2;

  /**
   * An instance of RequestProcessingPolicyValue, initialized to USE_SERVANT_MANAGER.
   */
  public static final RequestProcessingPolicyValue USE_SERVANT_MANAGER =
    new RequestProcessingPolicyValue(_USE_SERVANT_MANAGER);

  /**
   * The private array that maps integer codes to the enumeration
   * values.
   */
  private static final RequestProcessingPolicyValue[] enume =
    new RequestProcessingPolicyValue[]
    {
      USE_ACTIVE_OBJECT_MAP_ONLY, USE_DEFAULT_SERVANT, USE_SERVANT_MANAGER
    };

  /**
   * The private array of state names.
   */
  private static final String[] state_names =
    new String[]
    {
      "USE_ACTIVE_OBJECT_MAP_ONLY", "USE_DEFAULT_SERVANT", "USE_SERVANT_MANAGER"
    };

  /**
   * Normally, no new instances are required, so the constructor is protected.
   */
  protected RequestProcessingPolicyValue(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the RequestProcessingPolicyValue, matching the given integer constant.
   *
   * @param code one of _USE_ACTIVE_OBJECT_MAP_ONLY, _USE_DEFAULT_SERVANT, _USE_SERVANT_MANAGER.
   * @return one of USE_ACTIVE_OBJECT_MAP_ONLY, USE_DEFAULT_SERVANT, USE_SERVANT_MANAGER.
   * @throws BAD_PARAM if the parameter is not one of the valid values.
   */
  public static RequestProcessingPolicyValue from_int(int code)
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
   * @return one of _USE_ACTIVE_OBJECT_MAP_ONLY,
   * _USE_DEFAULT_SERVANT, _USE_SERVANT_MANAGER.
   */
  public int value()
  {
    return _value;
  }
}
