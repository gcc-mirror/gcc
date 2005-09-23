/* primitiveArrayTypeCode.java --
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

import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;

/**
 * A TypeCode for arrays.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class primitiveArrayTypeCode
  extends primitiveTypeCode
{
  /**
   * The array components.
   */
  TypeCode of;

  /**
   * The length of the array, must be updated when setting
   * a new value.
   */
  private int length;

  /**
   * Create a primitive array type code, defining the sequence
   * {@link TCKind.tk_sequence)} with
   * the given member type.
   *
   * @param array_of the sequence member type.
   */
  public primitiveArrayTypeCode(TCKind array_of)
  {
    super(TCKind.tk_sequence);
    of = new primitiveTypeCode(array_of);
  }

  /**
   * Create a primitive array type code, defining the array, sequence
   * or other type  with the given member type.
   *
   * @param this_type the type of this type (normally either
   * sequence of array).
   * @param array_of the sequence member type.
   */
  public primitiveArrayTypeCode(TCKind this_type, TypeCode array_of)
  {
    super(this_type);
    of = array_of;
  }

  /**
   * Return the array component type.
   * @return the array component type
   * @throws org.omg.CORBA.TypeCodePackage.BadKind
   */
  public TypeCode content_type()
                        throws org.omg.CORBA.TypeCodePackage.BadKind
  {
    return of;
  }

  /**
   * Return true if the other TypeCode defines the array, having elements
   * of the same type. The sizes of arrays are not taken into
   * consideration.
   *
   * @param other the other TypeCode
   * @return true if <code>other</code> is an array with the same
   * component type.
   */
  public boolean equal(TypeCode other)
  {
    try
      {
        return kind() == other.kind() &&
               content_type() == other.content_type();
      }
    catch (BadKind ex)
      {
        // Should not be thrown.
        return false;
      }
  }

  /**
   * Returns the agreed Id in the form of
   * <code>IDL:omg.org/CORBA/ {type name} Seq:1.0</code>.
   *
   * @return the Id of this TypeCode.
   *
   * @throws org.omg.CORBA.TypeCodePackage.BadKind if the content type
   * is not one of the constants, defined in {@link TCKind}.
   * This package class should not be used as TypeCode for the arrays,
   * holding the user defined components.
   */
  public String id()
            throws org.omg.CORBA.TypeCodePackage.BadKind
  {
    switch (content_type().kind().value())
      {
        case TCKind._tk_null :
          return "IDL:omg.org/CORBA/NullSeq:1.0";

        case TCKind._tk_void :
          return "IDL:omg.org/CORBA/VoidSeq:1.0";

        case TCKind._tk_short :
          return "IDL:omg.org/CORBA/ShortSeq:1.0";

        case TCKind._tk_long :
          return "IDL:omg.org/CORBA/LongSeq:1.0";

        case TCKind._tk_ushort :
          return "IDL:omg.org/CORBA/UShortSeq:1.0";

        case TCKind._tk_ulong :
          return "IDL:omg.org/CORBA/ULongSeq:1.0";

        case TCKind._tk_float :
          return "IDL:omg.org/CORBA/FloatSeq:1.0";

        case TCKind._tk_double :
          return "IDL:omg.org/CORBA/DoubleSeq:1.0";

        case TCKind._tk_boolean :
          return "IDL:omg.org/CORBA/BooleanSeq:1.0";

        case TCKind._tk_char :
          return "IDL:omg.org/CORBA/CharSeq:1.0";

        case TCKind._tk_octet :
          return "IDL:omg.org/CORBA/OctetSeq:1.0";

        case TCKind._tk_any :
          return "IDL:omg.org/CORBA/AnySeq:1.0";

        case TCKind._tk_TypeCode :
          return "IDL:omg.org/CORBA/TypeCodeSeq:1.0";

        case TCKind._tk_Principal :
          return "IDL:omg.org/CORBA/PrincipalSeq:1.0";

        case TCKind._tk_objref :
          return "IDL:omg.org/CORBA/ObjrefSeq:1.0";

        case TCKind._tk_struct :
          return "IDL:omg.org/CORBA/StructSeq:1.0";

        case TCKind._tk_union :
          return "IDL:omg.org/CORBA/UnionSeq:1.0";

        case TCKind._tk_enum :
          return "IDL:omg.org/CORBA/EnumSeq:1.0";

        case TCKind._tk_string :
          return "IDL:omg.org/CORBA/StringSeq:1.0";

        case TCKind._tk_sequence :
          return "IDL:omg.org/CORBA/SequenceSeq:1.0";

        case TCKind._tk_array :
          return "IDL:omg.org/CORBA/ArraySeq:1.0";

        case TCKind._tk_alias :
          return "IDL:omg.org/CORBA/AliasSeq:1.0";

        case TCKind._tk_except :
          return "IDL:omg.org/CORBA/ExceptSeq:1.0";

        case TCKind._tk_longlong :
          return "IDL:omg.org/CORBA/LongLongSeq:1.0";

        case TCKind._tk_ulonglong :
          return "IDL:omg.org/CORBA/ULongLongSeq:1.0";

        case TCKind._tk_longdouble :
          return "IDL:omg.org/CORBA/LongDoubleSeq:1.0";

        case TCKind._tk_wchar :
          return "IDL:omg.org/CORBA/WCharSeq:1.0";

        case TCKind._tk_wstring :
          return "IDL:omg.org/CORBA/WStringSeq:1.0";

        case TCKind._tk_fixed :
          return "IDL:omg.org/CORBA/FixedSeq:1.0";

        case TCKind._tk_value :
          return "IDL:omg.org/CORBA/ValueSeq:1.0";

        case TCKind._tk_value_box :
          return "IDL:omg.org/CORBA/Value_boxSeq:1.0";

        case TCKind._tk_native :
          return "IDL:omg.org/CORBA/NativeSeq:1.0";

        case TCKind._tk_abstract_interface :
          return "IDL:omg.org/CORBA/Abstract_interfaceSeq:1.0";

        default :
          throw new BadKind();
      }
  }

  /**
   * Return the array length.
   * @return the length of the array.
   * @throws org.omg.CORBA.TypeCodePackage.BadKind
   */
  public int length()
             throws org.omg.CORBA.TypeCodePackage.BadKind
  {
    return length;
  }

  /**
   * Sets the array length to the supplied value.
   *
   * @param l the new length.
   */
  public void setLength(int l)
  {
    this.length = l;
  }

}
