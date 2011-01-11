/* NotFoundReason.java --
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


package org.omg.CosNaming.NamingContextPackage;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.portable.IDLEntity;

/**
 * Represents the reason (explanation), why the binding cannot be found.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class NotFoundReason
  implements IDLEntity
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -5689237060527596081L;

  /**
   * The code for reason, when the node is missing.
   */
  public static final int _missing_node = 0;

  /**
   * The code for reason, when the binding type is object when
   * it should be a context.
   */
  public static final int _not_context = 1;

  /**
   * The code for reason, when the binding type is context when
   * it should be an object.
   */
  public static final int _not_object = 2;

  /**
   * The reason, when the node is missing.
   */
  public static final NotFoundReason missing_node =
    new NotFoundReason(_missing_node);

  /**
   * The reason, when the binding type is object when it should be
   * a context.
   */
  public static final NotFoundReason not_context =
    new NotFoundReason(_not_context);

  /**
   * The reason, when the binding type is context when it should be
   * and object.
   */
  public static final NotFoundReason not_object =
    new NotFoundReason(_not_object);

  /**
   * The reason code for this instance.
   */
  private int value;

  protected NotFoundReason(int a_value)
  {
    value = a_value;
  }

  /**
   * Get the reason object from the reason code.
   *
   * @throws BAD_PARAM for unsupported code.
   */
  public static NotFoundReason from_int(int value)
  {
    switch (value)
      {
        case _missing_node :
          return missing_node;

        case _not_context :
          return not_context;

        case _not_object :
          return not_object;

        default :
          throw new BAD_PARAM("Unsupported not found reason: " + value);
      }
  }

  /**
   * Get the reason code for this reason of not finding.
   */
  public int value()
  {
    return value;
  }
}
