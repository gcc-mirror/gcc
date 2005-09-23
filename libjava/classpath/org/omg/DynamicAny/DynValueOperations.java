/* DynValueOperations.java --
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

import org.omg.CORBA.TCKind;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

/**
 * Defines operations, applicable to DynValue. From the view point of DynAny,
 * the Value is very much like structure. However, differently from the
 * structure, the value type can also have private members. The private members
 * of DynValue are also accessible via this interface, but this possibility
 * should only be used in applications like in debuggers or inter-orb bridges.
 * Unlike structure, the value can also be equal to <code>null</code>.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynValueOperations
  extends DynAnyOperations, DynValueCommonOperations
{
  /**
   * Get the kind of the current member.
   *
   * @return the kind of member at the current position.
   *
   * @throws TypeMismatch if this DynValue is holding <code>null</code>.
   * @thorws InvalidValue if the current position does not indicate the member.
   */
  TCKind current_member_kind()
    throws TypeMismatch, InvalidValue;

  /**
   * Get the name of the current member.
   *
   * @return the name of the current member as defined by the typecode. May be
   * an empty string.
   *
   * @throws TypeMismatch if this DynValue is holding <code>null</code>.
   * @thorws InvalidValue if the current position does not indicate the member.
   */
  String current_member_name()
    throws TypeMismatch, InvalidValue;

  /**
   * Get all members as an array of the named DynAny's. The returned names are
   * set as they are defined by typecode.
   *
   * @return the array, representing the members of this instance of value.
   *
   * @throws InvalidValue if this DynValue is holding <code>null</code>.
   */
  NameDynAnyPair[] get_members_as_dyn_any()
    throws InvalidValue;

  /**
   * Get all members as an array of the named Any's. The returned names are set
   * as they are defined by typecode.
   *
   * @return the array, representing the members of this instance of value.
   *
   * @throws InvalidValue if this DynValue is holding <code>null</code>.
   */
  NameValuePair[] get_members()
    throws InvalidValue;

  /**
   * Set all members from the array of the named Any's.
   *
   * @param value the array, where the data for fields of the structure must
   * occur exactly in the same order, as defined by typecode.
   *
   * @throws TypeMismatch if the type or name of the array member does not match
   * the name and type of the corresponding field in the DynValue data
   * structure. The empty string is assumed matching any name.
   *
   * @throws InvalidValue if the size of the array does not match the number of
   * fields.
   */
  void set_members_as_dyn_any(NameDynAnyPair[] value)
    throws TypeMismatch, InvalidValue;

  /**
   * Set all members from the array of the named Any's.
   *
   * @param value the array, where the data for fields of the structure must
   * occur exactly in the same order, as defined by typecode.
   *
   * @throws TypeMismatch if the type or name of the array member does not match
   * the name and type of the corresponding field in the DynValue data
   * structure. The empty string is assumed matching any name.
   *
   * @throws InvalidValue if the size of the array does not match the number of
   * fields.
   */
  void set_members(NameValuePair[] value)
    throws TypeMismatch, InvalidValue;
}
