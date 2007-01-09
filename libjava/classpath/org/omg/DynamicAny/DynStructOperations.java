/* DynStructOperations.java --
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


package org.omg.DynamicAny;

import org.omg.CORBA.TCKind;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

/**
 * Defines the operations, applicable to the DynStructure.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynStructOperations
  extends DynAnyOperations
{
  /**
   * Get the kind of the structure field at the current position.
   *
   * @return the kind of field.
   *
   * @throws TypeMismatch for an empty structure (normally exception).
   * @throws InvalidValue if the current position does not indicate a memeber.
   */
  TCKind current_member_kind()
    throws TypeMismatch, InvalidValue;

  /**
   * Get the name of the structure field at the current position.
   *
   * @return the name of the field.
   *
   * @throws TypeMismatch for an empty structure (normally exception).
   * @throws InvalidValue if the current position does not indicate a memeber.
   */
  String current_member_name()
    throws TypeMismatch, InvalidValue;

  /**
   * Return array, describing describing the name and the value of each member
   * in the structure.
   *
   * @return an array of NameDynAnyPair's, each defining a single field in this
   * structure.
   */
  NameDynAnyPair[] get_members_as_dyn_any();

  /**
   * Return array, describing describing the name and the value of each member
   * in the structure.
   *
   * @return an array of NameValuePair's, each defining a single field in this
   * structure.
   */
  NameValuePair[] get_members();

  /**
   * Set the structure contend from the array, where each member defines the
   * name and value of the structure field. If the passed array is not empty,
   * the current position is set to the first member.
   *
   * The members of array must follow in the same order as the structure fields,
   * how they are defined in the typecode. The name-based value assignment is
   * not supported.
   *
   * @specnote The name-based value assignment is not supported by Sun's jdk
   * 1.4.
   *
   * @param value an array of NameDynValuePair's, each defining a single field in the
   * structure.
   *
   * @throws TypeMismatch if the member of the passed array has a different type
   * than the corresponding structure field.
   *
   * @throws InvalidValue if the size of the passed array is not the same as the
   * number of fields in this structure.
   */
  void set_members_as_dyn_any(NameDynAnyPair[] value)
    throws TypeMismatch, InvalidValue;

  /**
   * Set the structure contend from the array, where each member defines the
   * name and value of the structure field. If the passed array is not empty,
   * the current position is set to the first member.
   *
   * The members of array must follow in the same order as the structure fields,
   * how they are defined in the typecode. The name-based value assignment is
   * not supported.
   *
   * @specnote The name-based value assignment is not supported by Sun's jdk
   * 1.4.
   *
   * @param value an array of NameValuePair's, each defining a single field in the
   * structure.
   *
   * @throws TypeMismatch if the member of the passed array has a different type
   * than the corresponding structure field.
   *
   * @throws InvalidValue if the size of the passed array is not the same as the
   * number of fields in this structure.
   */
  void set_members(NameValuePair[] value)
    throws TypeMismatch, InvalidValue;

}
