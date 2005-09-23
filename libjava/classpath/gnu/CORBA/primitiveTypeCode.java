/* primitiveTypeCode.java --
    Copyright (C) 2005 Free Software Foundation, Inc.

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

package gnu.CORBA;

import java.io.Serializable;

import org.omg.CORBA.Any;
import org.omg.CORBA.IDLEntity;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.TypeCodePackage.Bounds;

/**
 * An information about a primitive CORBA data type
 * (boolean, char, wchar, octet and also signed or unsigned short, long,
 * long long, float and double).
 * This class only implements the methods {@link #kind() }
 * and {@link equal() } that are valid for
 * all TypeCode kinds. Other methods are implemented in derived
 * subclasses.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class primitiveTypeCode
  extends TypeCode
  implements IDLEntity, Serializable
{
  /**
   * The kind of this TypeCode.
   */
  protected final TCKind kind;

  public primitiveTypeCode(TCKind a_kind)
  {
    kind = a_kind;
  }

  public TypeCode concrete_base_type()
                              throws BadKind
  {
    throw new BadKind();
  }

  public TypeCode content_type()
                        throws BadKind
  {
    throw new BadKind();
  }

  public int default_index()
                    throws BadKind
  {
    throw new BadKind();
  }

  public TypeCode discriminator_type()
                              throws BadKind
  {
    throw new BadKind();
  }

  /**
   * Test two types for equality. The default implementation
   * returs true of the types of the same kind.
   * @param other the other type to compere with
   * @return true if the types are interchangeable.
   */
  public boolean equal(TypeCode other)
  {
    return kind() == other.kind();
  }

  public boolean equivalent(TypeCode parm1)
  {
    throw new NO_IMPLEMENT();
  }

  public short fixed_digits()
                     throws BadKind
  {
    throw new BadKind("fixed_digits");
  }

  public short fixed_scale()
                    throws BadKind
  {
    throw new BadKind("fixed_scale");
  }

  public TypeCode get_compact_typecode()
  {
    throw new NO_IMPLEMENT();
  }

  public String id()
            throws BadKind
  {
    throw new BadKind("id");
  }

  /**
   * Return the kind of this type code object.
   * @return one of the <code>TCKind.t_..</code> fields.
   */
  public TCKind kind()
  {
    return kind;
  }

  public int length()
             throws BadKind
  {
    throw new BadKind("length");
  }

  public int member_count()
                   throws BadKind
  {
    throw new BadKind("member_count");
  }

  public Any member_label(int index)
                   throws BadKind, Bounds
  {
    throw new BadKind("member_label");
  }

  public String member_name(int index)
                     throws BadKind, Bounds
  {
    throw new BadKind("member_name");
  }

  public TypeCode member_type(int index)
                       throws BadKind, Bounds
  {
    throw new BadKind("member_type");
  }

  public short member_visibility(int index)
                          throws BadKind, Bounds
  {
    throw new BadKind("member_visibility");
  }

  public String name()
              throws BadKind
  {
    throw new BadKind("name");
  }

  public short type_modifier()
                      throws BadKind
  {
    throw new BadKind("type_modifier");
  }
}
