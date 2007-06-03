/* DynValue.java --
   Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.

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

import org.omg.CORBA.DynAnyPackage.InvalidSeq;

/**
 * Represents the {@link DynAny}, holding a value type.
 *
 * A value type is something between CORBA structure and CORBA object.
 * Like CORBA object, it can have methods, supporting some IDL-defined
 * interface. However, like structures, they are always local and passed by
 * value, not by IOR reference.
 *
 * The value types can have both public and private members. They support
 * inheritance. Value types can also be abstract.
 *
 * @deprecated by {@link org.omg.DynamicAny.DynValue}
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public interface DynValue
  extends DynAny, org.omg.CORBA.Object
{
  /**
   * Get the kind of the member, pointed by the internal pointer.
   *
   * @return the kind of the member.
   */
  TCKind current_member_kind();

  /**
   * Get the name of the member, pointed by the internal pointer.
   *
   * @return the name of the member.
   */
  String current_member_name();

  /**
   * Get all members of the enclosed value type object.
   * @return members, as an array of the name - value pairs.
   */
  NameValuePair[] get_members();

  /**
   * Set all members for the enclosed value type object.
   *
   * @param value an array of members to set.
   *
   * @throws InvalidSeq if the passed sequence is not valid.
   */
  void set_members(NameValuePair[] value)
            throws InvalidSeq;
}
