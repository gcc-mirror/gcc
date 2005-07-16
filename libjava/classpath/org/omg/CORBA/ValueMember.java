/* ValueMember.java --
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
 * The class, defining properties of the value member.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class ValueMember
  implements Serializable, IDLEntity
{
  /**
   * Use serialVersionUID (v1.4) for interoperability.
   */
  private static final long serialVersionUID = -2507594168537449114L;

  /**
   * The typedef that represents the IDL type of the value member.
   */
  public IDLType type_def;

  /**
   * The repository ID of the value for that this member is defined
   */
  public String defined_in;

  /**
   * The repository ID of this value member itself.
   */
  public String id;

  /** The name of the value member. */
  public String name;

  /**
   * The version of the value in which this member is defined.
   */
  public String version;

  /** The type of of this value member. */
  public TypeCode type;

  /**
   * The type of access (public, private) of this value member.
   * This field can be equal to either {@link PUBLIC_MEMBER#value} or
   * {@link PRIVATE_MEMBER#value}.
   */
  public short access;

  /**
   * Create the value member with all fields initialised to default values
   * (0 and <code>null</code>).
   */
  public ValueMember()
  {
  }

  /**
   * Create the value member, specifying the field values.
   *
   * @param a_name name.
   * @param an_id id .
   * @param is_defined_in id of the value where the member is defined.
   * @param a_version version.
   * @param a_type tye.
   * @param a_type_def {@link IDLType} (typeded).
   * @param an_access accessibility scope. Can be equal to either
   * {@link PUBLIC_MEMBER#value} or {@link PRIVATE_MEMBER#value}.
   */
  public ValueMember(String a_name, String an_id, String is_defined_in,
                     String a_version, TypeCode a_type, IDLType a_type_def,
                     short an_access
                    )
  {
    name = a_name;
    id = an_id;
    defined_in = is_defined_in;
    version = a_version;
    type = a_type;
    type_def = a_type_def;
    access = an_access;
  }
}
