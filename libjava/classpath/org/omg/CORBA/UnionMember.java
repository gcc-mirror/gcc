/* UnionMember.java --
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
 * The component, describing the member of CORBA IDL <code>union</code>.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class UnionMember
  implements IDLEntity, Serializable
{
  /**
   * Use 1.4 version serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 5506049694216071974L;

  /**
   * The label of the union member.
   */
  public Any label;

  /**
   * The IDL type of the union member.
   */
  public IDLType type_def;

  /**
   * The name of the union member.
   */
  public String name;

  /**
   * The typecode of the union member.
   */
  public TypeCode type;

  /**
   * Creates a union member with all fields
   * left with the default value <code>null</code>.
   */
  public UnionMember()
  {
  }

  /**
   * Creates a union member.
   *
   * @param a_name member name.
   * @param a_type member type code.
   * @param a_type_def member IDL type definition.
   */
  public UnionMember(String a_name, Any a_label, TypeCode a_type,
                     IDLType a_type_def
                    )
  {
    name = a_name;
    label = a_label;
    type = a_type;
    type_def = a_type_def;
  }
}
