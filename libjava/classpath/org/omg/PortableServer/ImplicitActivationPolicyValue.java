/* ImplicitActivationPolicyValue.java --
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
* Specifies the implicit activation policy.
*
* This enumeration can obtain the following values:
* <ul>
* <li>IMPLICIT_ACTIVATION The POA supports implicit activation of servants.
* ({@link IdAssignmentPolicyValue#SYSTEM_ID} and
* {@link ServantRetentionPolicyValue#RETAIN} policies required).</li>
* <li>NO_IMPLICIT_ACTIVATION The POA does not support implicit activation.</li>
* </ul>
* @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
*/
public class ImplicitActivationPolicyValue
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (V1.4) for interoperability.
   */
  private static final long serialVersionUID = 3826572456602949295L;

  /**
   * The value field for the current instance.
   */
  private final int _value;

  /**
  * The possible value of this enumeration (IMPLICIT_ACTIVATION).
  */
  public static final int _IMPLICIT_ACTIVATION = 0;

  /**
   * An instance of ImplicitActivationPolicyValue, initialized to IMPLICIT_ACTIVATION.
   */
  public static final ImplicitActivationPolicyValue IMPLICIT_ACTIVATION =
    new ImplicitActivationPolicyValue(_IMPLICIT_ACTIVATION);

  /**
  * The possible value of this enumeration (NO_IMPLICIT_ACTIVATION).
  */
  public static final int _NO_IMPLICIT_ACTIVATION = 1;

  /**
   * An instance of ImplicitActivationPolicyValue, initialized to NO_IMPLICIT_ACTIVATION.
   */
  public static final ImplicitActivationPolicyValue NO_IMPLICIT_ACTIVATION =
    new ImplicitActivationPolicyValue(_NO_IMPLICIT_ACTIVATION);

  /**
   * The private array that maps integer codes to the enumeration
   * values.
   */
  private static final ImplicitActivationPolicyValue[] enume =
    new ImplicitActivationPolicyValue[]
    {
      IMPLICIT_ACTIVATION, NO_IMPLICIT_ACTIVATION
    };

  /**
   * The private array of state names.
   */
  private static final String[] state_names =
    new String[] { "IMPLICIT_ACTIVATION", "NO_IMPLICIT_ACTIVATION" };

  /**
   * Normally, no new instances are required, so the constructor is protected.
   */
  protected ImplicitActivationPolicyValue(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the ImplicitActivationPolicyValue, matching the given integer constant.
   *
   * @param code one of _IMPLICIT_ACTIVATION, _NO_IMPLICIT_ACTIVATION.
   * @return one of IMPLICIT_ACTIVATION, NO_IMPLICIT_ACTIVATION.
   * @throws BAD_PARAM if the parameter is not one of the valid values.
   */
  public static ImplicitActivationPolicyValue from_int(int code)
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
   * @return _IMPLICIT_ACTIVATION or _NO_IMPLICIT_ACTIVATION.
   */
  public int value()
  {
    return _value;
  }
}