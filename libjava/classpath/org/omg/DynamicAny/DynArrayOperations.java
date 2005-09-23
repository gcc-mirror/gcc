/* DynArrayOperations.java --
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


package org.omg.DynamicAny;

import org.omg.CORBA.Any;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

/**
 * Defines operations, applicable for {@link DynArray}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynArrayOperations
  extends DynAnyOperations
{
  /**
   * Returns the array.
   *
   * @return the array of elements as an array of DynAny's.
   */
  DynAny[] get_elements_as_dyn_any();

  /**
   * Returns the array.
   *
   * @return the array of elements as an array of Any's.
   */
  Any[] get_elements();

  /**
   * Sets the array.
   *
   * @param value the array of elements an DynAny's.
   *
   * @throws TypeMismatch if the members of the passed array does not match
   * array component type.
   *
   * @throws InvalidValue if the number of elements in the passed array is not
   * the same as the size of this DynArray.
   */
  void set_elements_as_dyn_any(DynAny[] value)
    throws TypeMismatch, InvalidValue;

  /**
   * Sets the array.
   *
   * @param value the array of elements as Any's.
   *
   * @throws TypeMismatch if the members of the passed array does not match
   * array component type.
   *
   * @throws InvalidValue if the number of elements in the passed array is not
   * the same as the size of this DynArray.
   */
  void set_elements(Any[] value)
    throws TypeMismatch, InvalidValue;
}
