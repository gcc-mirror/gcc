/* GeneralTypeCode.java --
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

import gnu.CORBA.CDR.BufferedCdrOutput;

import java.util.Arrays;
import java.util.BitSet;

import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;

/**
 * A typecode for types, requiring to provide various additional
 * properties but still not requiring to store the
 * members of the structure. The property can be retrieved
 * by the corresponding method if it has been previously assigned.
 * Otherwise, a {@link BadKind} is thrown.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class GeneralTypeCode
  extends PrimitiveTypeCode
{
  /**
   * Use serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = 1;


  /**
   * Indicates that the field value has not been previously set.
   */
  protected static int UNSET = Integer.MIN_VALUE;

  /**
   * The kinds for that the length() must return 0 even if it
   * has not been previously set.
   */
  private static final BitSet lengthAllowed = new BitSet();

  static
  {
    lengthAllowed.set(TCKind._tk_array);
    lengthAllowed.set(TCKind._tk_sequence);
    lengthAllowed.set(TCKind._tk_string);
    lengthAllowed.set(TCKind._tk_wstring);
  }

  private String id;
  private String name;
  private TypeCode concrete_base_type;
  private TypeCode content_type;
  private int len;
  private int type_modifier = UNSET;

  /**
   * Create a new instance, setting kind to the given kind.
   * @param a_kind the kind of the typecode being created.
   */
  public GeneralTypeCode(TCKind a_kind)
  {
    super(a_kind);
    if (!lengthAllowed.get(a_kind.value()))
      len = UNSET;
  }

  /**
   * Set this property.
   */
  public void setConcreteBase_type(TypeCode a_concrete_base_type)
  {
    this.concrete_base_type = a_concrete_base_type;
  }

  /**
   * Set the component content type.
   */
  public void setContentType(TypeCode a_content_type)
  {
    this.content_type = a_content_type;
  }

  /**
   * Set this property.
   */
  public void setId(String an_id)
  {
    this.id = an_id;
  }

  /**
   * Set the length property.
   * @param l
   */
  public void setLength(int l)
  {
    len = l;
  }

  /**
   * Set this property.
   */
  public void setName(String a_name)
  {
    this.name = a_name;
  }

  /**
   * Set the type modifier.
   */
  public void setTypeModifier(int a_type_modifier)
  {
    this.type_modifier = a_type_modifier;
  }

  /** {@inheritDoc} */
  public TypeCode concrete_base_type()
                              throws BadKind
  {
    if (concrete_base_type != null)
      return concrete_base_type;
    throw new BadKind("concrete_base_type");
  }

  /**
   * Returns the content type that must be explicitly set
   * for this class.
   *
   * @throws BadKind if the content type has not been set.
   */
  public TypeCode content_type()
                        throws BadKind
  {
    if (content_type != null)
      return content_type;
    throw new BadKind("content_type");
  }

  /**
   * Returns true if both typecodes, if written into CDR
   * stream, would result the same stream content.
   */
  public boolean equal(TypeCode other)
  {
    if (this == other)
      return true;
    if (kind() != other.kind())
      return false;

    BufferedCdrOutput a = new BufferedCdrOutput(16);
    BufferedCdrOutput b = new BufferedCdrOutput(16);

    a.write_TypeCode(this);
    b.write_TypeCode(other);

    return Arrays.equals(a.buffer.toByteArray(), b.buffer.toByteArray());
  }

  /**
   * Delegates functionality to {@link #equal}.
   */
  public boolean equivalent(TypeCode other)
  {
    return equal(other);
  }

  /** {@inheritDoc} */
  public String id()
            throws BadKind
  {
    if (id != null)
      return id;
    throw new BadKind("id");
  }

  /**
   * Get the length. For sequences, arrays, strings and wstrings
   * this method returns 0 rather than throwing a BadKind even
   * if {@link setLength(int)} has not been previously called.
   *
   * @return the length of string, array or sequence.
   *
   * @throws BadKind if the method cannot be invoked for the
   * given kind of typecode.
   */
  public int length()
             throws BadKind
  {
    if (len != UNSET)
      return len;
    throw new BadKind("length");
  }

  /** {@inheritDoc} */
  public String name()
              throws BadKind
  {
    if (name != null)
      return name;
    throw new BadKind("name");
  }

  /** {@inheritDoc} */
  public short type_modifier()
                      throws BadKind
  {
    if (type_modifier != UNSET)
      return (short) type_modifier;
    throw new BadKind("type_modifier");
  }
}
