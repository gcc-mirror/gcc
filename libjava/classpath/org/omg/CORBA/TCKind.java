/* TCKind.java -- java to IDL mapping constants.
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


/**
 * The basic constants, used in java to IDL mapping.
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class TCKind
{
  /**
   * The integer constant, indicating IDL data type
   * <code>null</code>.
   */
  public static final int _tk_null = 0;

  /**
   * The integer constant, indicating IDL data type
   * <code>void</code>.
   */
  public static final int _tk_void = 1;

  /**
   * The integer constant, indicating IDL data type
   * <code>short</code>.
   */
  public static final int _tk_short = 2;

  /**
   * The integer constant, indicating IDL data type
   * <code>long</code>.
   */
  public static final int _tk_long = 3;

  /**
   * The integer constant, indicating IDL data type
   * <code>ushort</code>.
   */
  public static final int _tk_ushort = 4;

  /**
   * The integer constant, indicating IDL data type
   * <code>ulong</code>.
   */
  public static final int _tk_ulong = 5;

  /**
   * The integer constant, indicating IDL data type
   * <code>float</code>.
   */
  public static final int _tk_float = 6;

  /**
   * The integer constant, indicating IDL data type
   * <code>double</code>.
   */
  public static final int _tk_double = 7;

  /**
   * The integer constant, indicating IDL data type
   * <code>boolean</code>.
   */
  public static final int _tk_boolean = 8;

  /**
   * The integer constant, indicating IDL data type
   * <code>char</code>.
   */
  public static final int _tk_char = 9;

  /**
   * The integer constant, indicating IDL data type
   * <code>octet</code>.
   */
  public static final int _tk_octet = 10;

  /**
   * The integer constant, indicating IDL data type
   * <code>any</code>.
   */
  public static final int _tk_any = 11;

  /**
   * The integer constant, indicating IDL data type
   * <code>TypeCode</code>.
   */
  public static final int _tk_TypeCode = 12;

  /**
   * The integer constant, indicating IDL data type
   * <code>Principal</code>.
   */
  public static final int _tk_Principal = 13;

  /**
   * The integer constant, indicating IDL data type
   * <code>objref</code>.
   */
  public static final int _tk_objref = 14;

  /**
   * The integer constant, indicating IDL data type
   * <code>struct</code>.
   */
  public static final int _tk_struct = 15;

  /**
   * The integer constant, indicating IDL data type
   * <code>union</code>.
   */
  public static final int _tk_union = 16;

  /**
   * The integer constant, indicating IDL data type
   * <code>enum</code>.
   */
  public static final int _tk_enum = 17;

  /**
   * The integer constant, indicating IDL data type
   * <code>string</code>.
   */
  public static final int _tk_string = 18;

  /**
   * The integer constant, indicating IDL data type
   * <code>sequence</code>.
   */
  public static final int _tk_sequence = 19;

  /**
   * The integer constant, indicating IDL data type
   * <code>array</code>.
   */
  public static final int _tk_array = 20;

  /**
   * The integer constant, indicating IDL data type
   * <code>alias</code>.
   */
  public static final int _tk_alias = 21;

  /**
   * The integer constant, indicating IDL data type
   * <code>except</code>.
   */
  public static final int _tk_except = 22;

  /**
   * The integer constant, indicating IDL data type
   * <code>longlong</code>.
   */
  public static final int _tk_longlong = 23;

  /**
   * The integer constant, indicating IDL data type
   * <code>ulonglong</code>.
   */
  public static final int _tk_ulonglong = 24;

  /**
   * The integer constant, indicating IDL data type
   * <code>longdouble</code>.
   */
  public static final int _tk_longdouble = 25;

  /**
   * The integer constant, indicating IDL data type
   * <code>wchar</code>.
   */
  public static final int _tk_wchar = 26;

  /**
   * The integer constant, indicating IDL data type
   * <code>wstring</code>.
   */
  public static final int _tk_wstring = 27;

  /**
   * The integer constant, indicating IDL data type
   * <code>fixed</code>.
   */
  public static final int _tk_fixed = 28;

  /**
   * The integer constant, indicating IDL data type
   * <code>value</code>.
   */
  public static final int _tk_value = 29;

  /**
   * The integer constant, indicating IDL data type
   * <code>value_box</code>.
   */
  public static final int _tk_value_box = 30;

  /**
   * The integer constant, indicating IDL data type
   * <code>native</code>.
   */
  public static final int _tk_native = 31;

  /**
   * The integer constant, indicating IDL data type
   * <code>abstract_interface</code>.
   */
  public static final int _tk_abstract_interface = 32;

  /**
   * The TCKind constant, indicating IDL data type
   * <code>null</code>
   */
  public static final TCKind tk_null = new TCKind(_tk_null);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>void</code>
   */
  public static final TCKind tk_void = new TCKind(_tk_void);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>short</code>
   */
  public static final TCKind tk_short = new TCKind(_tk_short);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>long</code>
   */
  public static final TCKind tk_long = new TCKind(_tk_long);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>ushort</code>
   */
  public static final TCKind tk_ushort = new TCKind(_tk_ushort);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>ulong</code>
   */
  public static final TCKind tk_ulong = new TCKind(_tk_ulong);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>float</code>
   */
  public static final TCKind tk_float = new TCKind(_tk_float);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>double</code>
   */
  public static final TCKind tk_double = new TCKind(_tk_double);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>boolean</code>
   */
  public static final TCKind tk_boolean = new TCKind(_tk_boolean);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>char</code>
   */
  public static final TCKind tk_char = new TCKind(_tk_char);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>octet</code>
   */
  public static final TCKind tk_octet = new TCKind(_tk_octet);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>any</code>
   */
  public static final TCKind tk_any = new TCKind(_tk_any);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>TypeCode</code>
   */
  public static final TCKind tk_TypeCode = new TCKind(_tk_TypeCode);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>Principal</code>
   */
  public static final TCKind tk_Principal = new TCKind(_tk_Principal);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>objref</code>
   */
  public static final TCKind tk_objref = new TCKind(_tk_objref);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>struct</code>
   */
  public static final TCKind tk_struct = new TCKind(_tk_struct);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>union</code>
   */
  public static final TCKind tk_union = new TCKind(_tk_union);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>enum</code>
   */
  public static final TCKind tk_enum = new TCKind(_tk_enum);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>string</code>
   */
  public static final TCKind tk_string = new TCKind(_tk_string);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>sequence</code>
   */
  public static final TCKind tk_sequence = new TCKind(_tk_sequence);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>array</code>
   */
  public static final TCKind tk_array = new TCKind(_tk_array);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>alias</code>
   */
  public static final TCKind tk_alias = new TCKind(_tk_alias);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>except</code>
   */
  public static final TCKind tk_except = new TCKind(_tk_except);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>longlong</code>
   */
  public static final TCKind tk_longlong = new TCKind(_tk_longlong);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>ulonglong</code>
   */
  public static final TCKind tk_ulonglong = new TCKind(_tk_ulonglong);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>longdouble</code>
   */
  public static final TCKind tk_longdouble = new TCKind(_tk_longdouble);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>wchar</code>
   */
  public static final TCKind tk_wchar = new TCKind(_tk_wchar);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>wstring</code>
   */
  public static final TCKind tk_wstring = new TCKind(_tk_wstring);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>fixed</code>
   */
  public static final TCKind tk_fixed = new TCKind(_tk_fixed);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>value</code>
   */
  public static final TCKind tk_value = new TCKind(_tk_value);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>value_box</code>
   */
  public static final TCKind tk_value_box = new TCKind(_tk_value_box);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>native</code>
   */
  public static final TCKind tk_native = new TCKind(_tk_native);

  /**
   * The TCKind constant, indicating IDL data type
   * <code>abstract_interface</code>
   */
  public static final TCKind tk_abstract_interface =
    new TCKind(_tk_abstract_interface);

  /**
   * The kind value for this instance.
   */
  private final int _value;

  /**
   * The array of all tk_... instances, sorted by the kind number.
   * 
   * As long as the kind numbers make the continuos sequence, starting from 0,
   * the members can be found just by direct indexing. 
   */
  private static final TCKind[] tk =
    new TCKind[]
    {
      tk_null, tk_void, tk_short, tk_long, tk_ushort, tk_ulong, tk_float,
      tk_double, tk_boolean, tk_char, tk_octet, tk_any, tk_TypeCode,
      tk_Principal, tk_objref, tk_struct, tk_union, tk_enum, tk_string,
      tk_sequence, tk_array, tk_alias, tk_except, tk_longlong, tk_ulonglong,
      tk_longdouble, tk_wchar, tk_wstring, tk_fixed, tk_value, tk_value_box,
      tk_native, tk_abstract_interface
    };

  /**
   * Creates a new TCKind instance with the given integer constant value.
   * @deprecated it is recommended to use {@link #from_int(int)} that
   * reuses existing TCKind object instead of allocating the new instance.
   * @param kind one of the <code>_tk_..</code> constants.
   */
  protected TCKind(int kind)
  {
    _value = kind;
  }

  /**
   * Returns the integer value, corresponding this instance of TCKind.
   * @return the <code>_tk_..</code> value, matching this instance.
   */
  public int value()
  {
    return _value;
  }

  /**
   * Return a TCKind object, matching the given integer code.
   * @param _tk_nnn the TCKind code, one of <code>_tk_..</code> constants.
   * @return the matching instance of TCKind, on of tk_.. constants.
   * @throws BAD_PARAM if the given integer constant is not one of the
   * valid <code>_tk_..</code> constants.
   */
  public static TCKind from_int(int _tk_nnn)
  {
    try {
      return tk[_tk_nnn];
    }
    catch (ArrayIndexOutOfBoundsException aex) {
      throw new BAD_PARAM("Invalid _tk_ code "+_tk_nnn);
    }
  }
  
}
