/* primitiveTypes.java --
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
 * A conveniency method for naming the built-in types.
 * This is used in error reporting that is part of the user interface.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class typeNamer
{
  /**
   * Names of the primitve types.
   */
  protected static final String[] tk =
    new String[]
    {
      "null", "void", "short", "long", "ushort", "ulong", "float", "double",
      "boolean", "char", "octet", "any", "TypeCode", "Principal", "objref",
      "struct", "union", "enum", "string", "sequence", "array", "alias",
      "exception", "longlong", "ulonglong", "longdouble", "wchar", "wstring",
      "fixed", "value", "value_box", "native", "abstract_interface"
    };

  /**
   * Primitve TypeCodes.
   */
  protected static final TypeCode[] primitveCodes =
    new TypeCode[]
    {
      new primitiveTypeCode(TCKind.tk_null),
      new primitiveTypeCode(TCKind.tk_void),
      new primitiveTypeCode(TCKind.tk_short),
      new primitiveTypeCode(TCKind.tk_long),
      new primitiveTypeCode(TCKind.tk_ushort),
      new primitiveTypeCode(TCKind.tk_ulong),
      new primitiveTypeCode(TCKind.tk_float),
      new primitiveTypeCode(TCKind.tk_double),
      new primitiveTypeCode(TCKind.tk_boolean),
      new primitiveTypeCode(TCKind.tk_char),
      new primitiveTypeCode(TCKind.tk_octet),
      new primitiveTypeCode(TCKind.tk_any),
      new primitiveTypeCode(TCKind.tk_TypeCode),
      new primitiveTypeCode(TCKind.tk_Principal),
      new primitiveTypeCode(TCKind.tk_objref),
      new primitiveTypeCode(TCKind.tk_struct),
      new primitiveTypeCode(TCKind.tk_union),
      new primitiveTypeCode(TCKind.tk_enum),
      new primitiveTypeCode(TCKind.tk_string),
      new primitiveTypeCode(TCKind.tk_sequence),
      new primitiveTypeCode(TCKind.tk_array),
      new primitiveTypeCode(TCKind.tk_alias),
      new primitiveTypeCode(TCKind.tk_except),
      new primitiveTypeCode(TCKind.tk_longlong),
      new primitiveTypeCode(TCKind.tk_ulonglong),
      new primitiveTypeCode(TCKind.tk_longdouble),
      new primitiveTypeCode(TCKind.tk_wchar),
      new primitiveTypeCode(TCKind.tk_wstring),
      new primitiveTypeCode(TCKind.tk_fixed),
      new primitiveTypeCode(TCKind.tk_value),
      new primitiveTypeCode(TCKind.tk_value_box),
      new primitiveTypeCode(TCKind.tk_native),
      new primitiveTypeCode(TCKind.tk_abstract_interface)
    };

  /**
   * Get the primitive type code.
   *
   * @return the primitve type code, corresponding the passed value.
   *
   * @throws BadKind if this is not a primitive type code.
   */
  public static TypeCode getPrimitveTC(TCKind tc)
                                throws BadKind
  {
    try
      {
        return primitveCodes [ tc.value() ];
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        throw new BadKind(tc.value() + " is not a primitve type.");
      }
  }

  /**
   * Get the string name of the passed primitive type.
   *
   * @param kind the kind of the primitive type the must be defined
   * in {@link omg.org.CORBA.TCKind}.
   *
   * @return the short string name, used in error reporting, etc.
   */
  public static String nameIt(int kind)
  {
    try
      {
        return tk [ kind ];
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        return "type of kind '" + kind + "'";
      }
  }

  /**
   * Get the string name of the passed primitive type.
   *
   * @param kind the kind of the primitive type the must be defined
   * in {@link omg.org.CORBA.TCKind}.
   *
   * @return the short string name, used in error reporting, etc.
   */
  public static String nameIt(TypeCode type)
  {
    try
      {
        if (type.kind().value() == TCKind._tk_array)
          return "array of " + nameIt(type.content_type());
        else if (type.kind().value() == TCKind._tk_sequence)
          return "sequence of " + nameIt(type.content_type());
        else
          return nameIt(type.kind().value());
      }
    catch (Exception ex)
      {
        return "type of kind '" + type.kind().value() + "'";
      }
  }
}
