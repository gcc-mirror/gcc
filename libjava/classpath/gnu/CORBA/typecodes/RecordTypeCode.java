/* RecordTypeCode.java --
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


package gnu.CORBA.typecodes;

import gnu.CORBA.CorbaList;

import org.omg.CORBA.Any;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.TypeCodePackage.Bounds;
import org.omg.CORBA.UnionMember;
import org.omg.CORBA.ValueMember;

/**
 * The type code that also has the member property getters
 * supported.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class RecordTypeCode
  extends GeneralTypeCode
{
  /** 
   * Use serialVersionUID for interoperability. 
   */
  private static final long serialVersionUID = 1;
  
  /**
   * The individual field of the record.
   */
  public static class Field
  {
    /**
     * The record label.
     */
    public Any label;

    /**
     * The record name.
     */
    public String name;

    /**
     * The record type.
     */
    public TypeCode type;

    /**
     * The record visibility.
     */
    public int visibility = UNSET;
  }

  /**
   * The members of this data structure.
   */
  protected CorbaList members = new CorbaList();
  private TypeCode discriminator_type;
  private int default_index = UNSET;

  /**
   * Creates the type code of the given kind.
   */
  public RecordTypeCode(TCKind a_kind)
  {
    super(a_kind);
  }

  /**
   * Set the default index.
   */
  public void setDefaultIndex(int a_default_index)
  {
    this.default_index = a_default_index;
  }

  /**
   * Set the discriminator type.
   */
  public void setDiscriminator_type(TypeCode a_discriminator_type)
  {
    this.discriminator_type = a_discriminator_type;
  }

  public Field getField(int p)
  {
    return (Field) members.get(p);
  }

  public void add(Field field)
  {
    members.add(field);
  }

  /**
   * Adds a new field, taking values from the passed
   * {@link StructMember}.
   */
  public void add(StructMember m)
  {
    Field f = field();
    f.name = m.name;
    f.type = m.type;
  }

  /**
   * Adds a new field, taking values from the passed
   * {@link ValueMember}.
   */
  public void add(ValueMember m)
  {
    Field f = field();
    f.name = m.name;
    f.type = m.type;
    f.visibility = m.access;
    
  }

  /**
   * Adds a new field, taking values from the passed
   * {@link UnionMember}.
   */
  public void add(UnionMember m)
  {
    Field f = field();
    f.name = m.name;
    f.type = m.type;
    f.label = m.label;
  }

  public int default_index()
                    throws BadKind
  {
    if (default_index != UNSET)
      return default_index;
    throw new BadKind();
  }

  public TypeCode discriminator_type()
                              throws BadKind
  {
    if (discriminator_type != null)
      return discriminator_type;
    throw new BadKind();
  }

  /**
   * Creates, adds and returns new field.
   */
  public Field field()
  {
    Field f = new Field();
    members.add(f);
    return f;
  }

  /** {@inheritDoc} */
  public int member_count()
  {
    return members.size();
  }

  /** {@inheritDoc} */
  public Any member_label(int index)
                   throws BadKind, Bounds
  {
    Field f = getField(index);
    if (f.label != null)
      {
        return f.label;
      }
    else
      throw new BadKind();
  }

  /** {@inheritDoc} */
  public String member_name(int index)
                     throws BadKind
  {
    Field f = getField(index);
    if (f.name != null)
      {
        return f.name;
      }
    else
      throw new BadKind();
  }

  /** {@inheritDoc} */
  public TypeCode member_type(int index)
                       throws BadKind, Bounds
  {
    Field f = getField(index);
    if (f.type != null)
      {
        return f.type;
      }
    else
      throw new BadKind();
  }

  /** {@inheritDoc} */
  public short member_visibility(int index)
                          throws BadKind, Bounds
  {
    Field f = getField(index);
    if (f.visibility != UNSET)
      {
        return (short) f.visibility;
      }
    else
      throw new BadKind();
  }
}
