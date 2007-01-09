/* BindingType.java --
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


package org.omg.CosNaming;

import org.omg.CORBA.BAD_PARAM;

/**
 * Specifies the binding type (how the binding has been created).
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class BindingType
  implements org.omg.CORBA.portable.IDLEntity
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 3735105633408228513L;

  /**
   * This constant means that the binding has been created
   * with operations  bind_context,  rebind_context or
   * bind_new_context.
   */
  public static final int _ncontext = 1;

  /**
   * This constant means that the binding has been created by the
   * means, different from the listed in {@link #_ncontext} description.
   */
  public static final int _nobject = 0;

  /**
   * This constant means that the binding has been created
   * with operations  bind_context,  rebind_context or
   * bind_new_context.
   */
  public static final BindingType ncontext = new BindingType(_ncontext);

  /**
   * This constant means that the binding has been created by the
   * means, different from the listed in {@link #_ncontext} description.
   */
  public static final BindingType nobject = new BindingType(_nobject);

  /**
   * The binding type, defined by this instance.
   */
  private final int type;

  /**
   * Create the new binding type definition.
   *
   * @param value the binding type, normally either _nobject or
   * _ncontext.
   */
  protected BindingType(int value)
  {
    type = value;
  }

  /**
   * Get the binding type instance, matching its integer code.
   *
   * @param value the binding type code.
   * @return the matching binding type instance.
   *
   * @throws BAD_PARAM if there is no matching binding type for
   * the passed value.
   */
  public static BindingType from_int(int value)
  {
    switch (value)
      {
        case _nobject :
          return nobject;

        case _ncontext :
          return ncontext;

        default :
          throw new BAD_PARAM("Unsupported binding type code " + value);
      }
  }

  /**
   * Return the integer code for this binding.
   */
  public int value()
  {
    return type;
  }
}