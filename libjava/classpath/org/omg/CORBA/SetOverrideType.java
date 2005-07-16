/* SetOverrideType.java --
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


package org.omg.CORBA;

import java.io.Serializable;

import org.omg.CORBA.portable.IDLEntity;

/**
 * Defines the instruction, how the newly specified policies can be
 * taken into consideration. The policies can be either
 * added to the current policies or replace them.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class SetOverrideType
  implements Serializable, IDLEntity
{
  /**
   * Use v 1.4 serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = -2761857189425106972L;

  /**
   * Add the new policies to the existing policies.
   */
  public static final int _ADD_OVERRIDE = 1;

  /**
   * Replace the new existing policies by the new policies.
   */
  public static final int _SET_OVERRIDE = 0;

  /**
   * Add the new policies to the existing policies.
   * An instance of SetOverrideType, initialized to _ADD_OVERRIDE.
   */
  public static final SetOverrideType ADD_OVERRIDE =
    new SetOverrideType(_ADD_OVERRIDE);

  /**
   * Replace the new existing policies by the new policies.
   * An instance of SetOverrideType, initialized to _SET_OVERRIDE.
   */
  public static final SetOverrideType SET_OVERRIDE =
    new SetOverrideType(_SET_OVERRIDE);

  private final int _value;

  /**
   * No other instances can be created.
   */
  protected SetOverrideType(int a_value)
  {
    _value = a_value;
  }

  /**
   * Returns the SetOverrideType, matching the given integer constant
   * @param kind one of _ADD_OVERRIDE or _SET_OVERRIDE.
   *
   * @return one of ADD_OVERRIDE or SET_OVERRIDE.
   *
   * @throws BAD_PARAM if the parameter is not one of these two values.
   */
  public static SetOverrideType from_int(int kind)
  {
    switch (kind)
      {
        case _ADD_OVERRIDE :
          return ADD_OVERRIDE;

        case _SET_OVERRIDE :
          return SET_OVERRIDE;

        default :
          throw new BAD_PARAM("invalid add/override type " + kind);
      }
  }

  /**
   * Returns a short string representation.
   *
   * @return either "add" or "replace".
   */
  public String toString()
  {
    return (_value == _ADD_OVERRIDE) ? "add" : "replace";
  }

  /**
   * Returns the value, representing stored instruction.
   *
   * @return one of ADD_OVERRIDE or SET_OVERRIDE
   */
  public int value()
  {
    return _value;
  }
}
