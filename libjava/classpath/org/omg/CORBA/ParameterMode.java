/* ParameterMode.java --
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

import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * Defines the parameter modes (the ways in that a method parameter
 * is used during invocation).
 *
 * In CORBA, a method parameter can pass the value (PARAM_IN),
 * be used as a placeholder to return the value (PARAM_OUT) or
 * both pass the data and be used as a placeholder to return the
 * changed value (PARAM_INOUT).
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class ParameterMode
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 1521598391932998229L;

  /**
   * This value means that the parameter is an IN parameter.
   */
  public static final int _PARAM_IN = 0;

  /**
   * This value means that the parameter is an OUT parameter.
   */
  public static final int _PARAM_OUT = 1;

  /**
   * This value means that the parameter is an INOUT parameter.
   */
  public static final int _PARAM_INOUT = 2;

  /**
   * This value means that the parameter is an IN parameter.
   */
  public static ParameterMode PARAM_IN = new ParameterMode(_PARAM_IN);

  /**
   * This value means that the parameter is an OUT parameter.
   */
  public static ParameterMode PARAM_OUT = new ParameterMode(_PARAM_OUT);

  /**
   * This value means that the parameter is an INOUT parameter.
   */
  public static ParameterMode PARAM_INOUT = new ParameterMode(_PARAM_INOUT);

  /**
   * The value of this parameter mode instance.
   */
  private final int value;

  /**
   * The conversion table.
   */
  private static final ParameterMode[] table =
    new ParameterMode[] { PARAM_IN, PARAM_OUT, PARAM_INOUT };

  /**
   * Create an instance of the parameter mode with the given value.
   */
  protected ParameterMode(int a_value)
  {
    value = a_value;
  }

  /**
   * Return the integer value code for the given parameter mode.
   *
   * @return 0 for PARAM_IN, 1 for PARAM_OUT, 3 for PARAM_INOUT.
   */
  public int value()
  {
    return value;
  }

  /**
   * Get a parameter mode instance for the integer parameter mode code.
   *
   * @param p_mode a parameter mode (0..2).
   *
   * @return a corresponding parameter mode instance.
   *
   * @throws BAD_PARAM for the invalid parameter mode code.
   */
  public static ParameterMode from_int(int p_mode)
  {
    try
      {
        return table [ p_mode ];
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        throw new BAD_PARAM("Invalid parameter mode: " + p_mode);
      }
  }
}