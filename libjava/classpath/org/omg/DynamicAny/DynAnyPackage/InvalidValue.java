/* InvalidValue.java --
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


package org.omg.DynamicAny.DynAnyPackage;

import org.omg.CORBA.UserException;
import org.omg.CORBA.portable.IDLEntity;

import java.io.Serializable;

/**
 * Raised when the operation on DynAny cannot be performed because one of
 * the parameters, while having the correct type, has the invalid value.
 * For instance, it is raised in response to the set the array content,
 * when number of the provided elements mismatch the size of array.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public final class InvalidValue
  extends UserException
  implements IDLEntity, Serializable
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = 4928947584617628504L;

  /**
   * Create InvalidValue with no explaining
   * message.
   */
  public InvalidValue()
  {
  }

  /**
   * Create the InvalidValue with explaining
   * message.
   *
   * @param why a string, explaining, why this exception has been thrown.
   */
  public InvalidValue(String why)
  {
    super(why);
  }
}