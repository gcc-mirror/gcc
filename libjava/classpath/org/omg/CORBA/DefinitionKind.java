/* DefinitionKind.java --
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
 * This class indicates the kind of the definition, stored in the interface
 * repository.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public class DefinitionKind
  implements IDLEntity, Serializable
{
  /**
   * Use v1.4 serialVersionUID for interoperability.
   */
  private static final long serialVersionUID = -8601167576704143376L;

  /**
   * Indicates that the current definition defines has no definition kind.
   */
  public static final int _dk_none = 0;

  /**
   * This is a "wildcard '*'", used in cases where any definition kind
   * is appropriate .
   */
  public static final int _dk_all = 1;

  /**
   * Indicates that the current definition defines an attribute.
   */
  public static final int _dk_Attribute = 2;

  /**
   * Indicates that the current definition defines a constant.
   */
  public static final int _dk_Constant = 3;

  /**
   * Indicates that the current definition defines an exception.
   */
  public static final int _dk_Exception = 4;

  /**
   * Indicates that the current definition defines an interface.
   * The interface definition can contain constants, types,
   * exceptions, operations, and attributes.
   */
  public static final int _dk_Interface = 5;

  /**
   * Indicates that the current definition defines a Module.
   * The Module can contain constants, typedefs, exceptions and also
   * interface, component, home, value or event type definitions.
   * The module can also enclose other (nested) modules.
   */
  public static final int _dk_Module = 6;

  /**
   * Indicates that the current definition defines an operation, including
   * the lists of parameters and exceptions raised by this operation.
   */
  public static final int _dk_Operation = 7;

  /**
   * Indicates that the current definition defines a named type that is not
   * an interface nor a value definition. Generally, it also cannot be
   * a definition of component, home and event, but these three kinds are
   * not listed in this class.
   */
  public static final int _dk_Typedef = 8;

  /**
   * Indicates that the current definition defines an alias.
   */
  public static final int _dk_Alias = 9;

  /**
   * Indicates that the current definition defines a structure.
   */
  public static final int _dk_Struct = 10;

  /**
   * Indicates that the current definition defines a union.
   */
  public static final int _dk_Union = 11;

  /**
   * Indicates that the current definition defines an enumeration.
   */
  public static final int _dk_Enum = 12;

  /**
   * Indicates that the current definition defines a primitive type.
   */
  public static final int _dk_Primitive = 13;

  /**
   * Indicates that the current definition defines a string.
   */
  public static final int _dk_String = 14;

  /**
   * Indicates that the current definition defines a sequence.
   */
  public static final int _dk_Sequence = 15;

  /**
   * Indicates that the current definition defines an array.
   */
  public static final int _dk_Array = 16;

  /**
   * Indicates that the current definition defines an another interface
   * repository.
   */
  public static final int _dk_Repository = 17;

  /**
   * Indicates that the current definition defines a wide (usually 16-bit
   * per character) string.
   */
  public static final int _dk_Wstring = 18;

  /**
   * Indicates that the current definition defines a CORBA <code>fixed</code>.
   */
  public static final int _dk_Fixed = 19;

  /**
   * Indicates that the current definition defines a value.
   */
  public static final int _dk_Value = 20;

  /**
   * Indicates that the current definition defines a value box.
   */
  public static final int _dk_ValueBox = 21;

  /**
   * Indicates that the current definition defines value member.
   */
  public static final int _dk_ValueMember = 22;

  /**
   * Indicates that the current definition defines a Native.
   */
  public static final int _dk_Native = 23;

  /**
   * Indicates that the current definition defines an abstract interface.
   */
  public static final int _dk_AbstractInterface = 24;

  /**
   * Indicates that the current definition defines has no definition kind.
   */
  public static final DefinitionKind dk_none = new DefinitionKind(_dk_none);

  /**
   * This is a "wildcard '*'", used in cases where any definition kind
   * is appropriate .
   */
  public static final DefinitionKind dk_all = new DefinitionKind(_dk_all);

  /**
   * Indicates that the current definition defines an attribute.
   */
  public static final DefinitionKind dk_Attribute = new DefinitionKind(_dk_Attribute);

  /**
   * Indicates that the current definition defines a constant.
   */
  public static final DefinitionKind dk_Constant = new DefinitionKind(_dk_Constant);

  /**
   * Indicates that the current definition defines an exception.
   */
  public static final DefinitionKind dk_Exception = new DefinitionKind(_dk_Exception);

  /**
   * Indicates that the current definition defines an interface.
   * The interface definition can contain constants, types,
   * exceptions, operations, and attributes.
   */
  public static final DefinitionKind dk_Interface = new DefinitionKind(_dk_Interface);

  /**
   * Indicates that the current definition defines a Module.
   * The Module can contain constants, typedefs, exceptions and also
   * interface, component, home, value or event type definitions.
   * The module can also enclose other (nested) modules.
   */
  public static final DefinitionKind dk_Module = new DefinitionKind(_dk_Module);

  /**
   * Indicates that the current definition defines an operation, including
   * the lists of parameters and exceptions raised by this operation.
   */
  public static final DefinitionKind dk_Operation = new DefinitionKind(_dk_Operation);

  /**
   * Indicates that the current definition defines a named type that is not
   * an interface nor a value definition. Generally, it also cannot be
   * a definition of component, home and event, but these three kinds are
   * not listed in this class.
   */
  public static final DefinitionKind dk_Typedef = new DefinitionKind(_dk_Typedef);

  /**
   * Indicates that the current definition defines an alias.
   */
  public static final DefinitionKind dk_Alias = new DefinitionKind(_dk_Alias);

  /**
   * Indicates that the current definition defines a structure.
   */
  public static final DefinitionKind dk_Struct = new DefinitionKind(_dk_Struct);

  /**
   * Indicates that the current definition defines a union.
   */
  public static final DefinitionKind dk_Union = new DefinitionKind(_dk_Union);

  /**
   * Indicates that the current definition defines an enumeration.
   */
  public static final DefinitionKind dk_Enum = new DefinitionKind(_dk_Enum);

  /**
   * Indicates that the current definition defines a primitive type.
   */
  public static final DefinitionKind dk_Primitive = new DefinitionKind(_dk_Primitive);

  /**
   * Indicates that the current definition defines a string.
   */
  public static final DefinitionKind dk_String = new DefinitionKind(_dk_String);

  /**
   * Indicates that the current definition defines a sequence.
   */
  public static final DefinitionKind dk_Sequence = new DefinitionKind(_dk_Sequence);

  /**
   * Indicates that the current definition defines an array.
   */
  public static final DefinitionKind dk_Array = new DefinitionKind(_dk_Array);

  /**
   * Indicates that the current definition defines an another interface
   * repository.
   */
  public static final DefinitionKind dk_Repository =
    new DefinitionKind(_dk_Repository);

  /**
   * Indicates that the current definition defines a wide (usually 16-bit
   * per character) string.
   */
  public static final DefinitionKind dk_Wstring = new DefinitionKind(_dk_Wstring);

  /**
   * Indicates that the current definition defines a CORBA <code>fixed</code>.
   */
  public static final DefinitionKind dk_Fixed = new DefinitionKind(_dk_Fixed);

  /**
   * Indicates that the current definition defines a value.
   */
  public static final DefinitionKind dk_Value = new DefinitionKind(_dk_Value);

  /**
   * Indicates that the current definition defines a value box.
   */
  public static final DefinitionKind dk_ValueBox = new DefinitionKind(_dk_ValueBox);

  /**
   * Indicates that the current definition defines value member.
   */
  public static final DefinitionKind dk_ValueMember =
    new DefinitionKind(_dk_ValueMember);

  /**
   * Indicates that the current definition defines a Native.
   */
  public static final DefinitionKind dk_Native = new DefinitionKind(_dk_Native);

  /**
   * Indicates that the current definition defines .
   */
  public static final DefinitionKind dk_AbstractInterface =
    new DefinitionKind(_dk_AbstractInterface);

  /**
   * The defintion code of the current instance of the definition kind.
   */
  private final int kind;

  /**
   * The table of the definition kinds
   */
  private static DefinitionKind[] table;

  /**
   * Creates a definition kind with the given integer definition kind code.
   *
   * @param a_kind a definition kind code, one of the _dk_.. constants,
   * defined in this class.
   */
  protected DefinitionKind(int a_kind)
  {
    kind = a_kind;
  }

  /**
   * Get the definition code of the current instance of the definition kind.
   *
   * @return one of the _dk_... constants, defined in this class.
   */
  public int value()
  {
    return kind;
  }

  /**
   * Get the definition kind, corresponding the given integer code.
   *
   * @param a_kind the definition kind code, one of the _dk_... constants,
   * defined in this class.
   *
   * @return the corresponding definition kind, one of the dk_... constants,
   * defined in this class.
   *
   * @throws BAD_PARAM if the given integer does not match any definition
   * kind.
   */
  public static DefinitionKind from_int(int a_kind)
  {
    if (table == null)
      fill_table();
    try
      {
        return table [ a_kind ];
      }
    catch (ArrayIndexOutOfBoundsException ex)
      {
        throw new BAD_PARAM("No def. kind " + a_kind);
      }
  }

  /**
   * Fill the conversion table on demand.
   */
  private static void fill_table()
  {
    table = new DefinitionKind[ 25 ];
    table [ _dk_none ] = dk_none;
    table [ _dk_all ] = dk_all;
    table [ _dk_Attribute ] = dk_Attribute;
    table [ _dk_Constant ] = dk_Constant;
    table [ _dk_Exception ] = dk_Exception;
    table [ _dk_Interface ] = dk_Interface;
    table [ _dk_Module ] = dk_Module;
    table [ _dk_Operation ] = dk_Operation;
    table [ _dk_Typedef ] = dk_Typedef;
    table [ _dk_Alias ] = dk_Alias;
    table [ _dk_Struct ] = dk_Struct;
    table [ _dk_Union ] = dk_Union;
    table [ _dk_Enum ] = dk_Enum;
    table [ _dk_Primitive ] = dk_Primitive;
    table [ _dk_String ] = dk_String;
    table [ _dk_Sequence ] = dk_Sequence;
    table [ _dk_Array ] = dk_Array;
    table [ _dk_Repository ] = dk_Repository;
    table [ _dk_Wstring ] = dk_Wstring;
    table [ _dk_Fixed ] = dk_Fixed;
    table [ _dk_Value ] = dk_Value;
    table [ _dk_ValueBox ] = dk_ValueBox;
    table [ _dk_ValueMember ] = dk_ValueMember;
    table [ _dk_Native ] = dk_Native;
    table [ _dk_AbstractInterface ] = dk_AbstractInterface;
  }
}