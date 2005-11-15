/* DefinitionKindHelper.java --
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

import gnu.CORBA.DefinitionKindHolder;
import gnu.CORBA.OrbRestricted;
import gnu.CORBA.gnuAny;
import gnu.CORBA.typecodes.PrimitiveTypeCode;

import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper operations for the definition kind.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class DefinitionKindHelper
{
  /**
   * The cached typecode value, computed only once.
   */
  private static TypeCode typeCode;

  /**
   * Insert the definition kind into the given Any.
   */
  public static void insert(Any a, DefinitionKind that)
  {
    a.insert_Streamable(new DefinitionKindHolder(that));
  }

  /**
   * Extract the definition kind from the given Any.
   */
  public static DefinitionKind extract(Any a)
  {
    return ((DefinitionKindHolder) a.extract_Streamable()).value;
  }

  /**
   * Get the definition kind typecode (enumeration, named "DefinitionKind").
   * The member names are listed as defined by java 1.4 specification.
   * The names, defined in OMG specification only (like dk_Home or
   * dk_Event) are not listed.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      {
        String[] members =
          new String[]
          {
            "dk_none", "dk_all", "dk_Attribute", "dk_Constant", "dk_Exception",
            "dk_Interface", "dk_Module", "dk_Operation", "dk_Typedef",
            "dk_Alias", "dk_Struct", "dk_Union", "dk_Enum", "dk_Primitive",
            "dk_String", "dk_Sequence", "dk_Array", "dk_Repository",
            "dk_Wstring", "dk_Fixed", "dk_Value", "dk_ValueBox",
            "dk_ValueMember", "dk_Native"
          };

        typeCode =
          OrbRestricted.Singleton.create_enum_tc(id(), "DefinitionKind",
                                                  members
                                                 );
      }
    return typeCode;
  }

  /**
   * Get the definition kind repository id.
   *
   * @return "IDL:omg.org/CORBA/DefinitionKind:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/DefinitionKind:1.0";
  }

  /**
   * Read the definitin kind (as int) from the CDR intput stream.
   *
   * @param istream a stream to read from.
   */
  public static DefinitionKind read(InputStream istream)
  {
    return DefinitionKind.from_int(istream.read_long());
  }

  /**
   * Write the definition kind (as int) to the CDR output stream.
   *
   * @param ostream a stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream ostream, DefinitionKind value)
  {
    ostream.write_long(value.value());
  }
}