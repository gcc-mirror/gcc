/* SetOverrideTypeHelper.java --
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

import gnu.CORBA.SetOverrideTypeHolder;
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper operations for the definition kind.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class SetOverrideTypeHelper
{
  /**
   * The cached typecode value, computed only once.
   */
  private static TypeCode typeCode;

  /**
   * Insert the definition kind into the given Any.
   */
  public static void insert(Any any, SetOverrideType that)
  {
    any.insert_Streamable(new SetOverrideTypeHolder(that));
  }

  /**
   * Extract the definition kind from the given Any.
   */
  public static SetOverrideType extract(Any any)
  {
    return ((SetOverrideTypeHolder) any.extract_Streamable()).value;
  }

  /**
   * Get the definition kind typecode (enumeration, named "SetOverrideType").
   * The typecode states that the enumeration can obtain one of
   * the following values: SET_OVERRIDE ,ADD_OVERRIDE .
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      {
        String[] members =
          new String[]
          {
            "SET_OVERRIDE" ,"ADD_OVERRIDE" 
          };

        typeCode =
          OrbRestricted.Singleton.create_enum_tc(id(), "SetOverrideType",
                                                  members
                                                 );
      }
    return typeCode;
  }

  /**
   * Get the definition kind repository id.
   *
   * @return "IDL:omg.org/CORBA/SetOverrideType:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/CORBA/SetOverrideType:1.0";
  }

  /**
   * Read the enumeration value (as int) from the CDR intput stream.
   *
   * @param istream a stream to read from.*
   */
  public static SetOverrideType read(InputStream istream)
  {
    return SetOverrideType.from_int(istream.read_long());
  }

  /**
   * Write the enumeration value (as int) to the CDR output stream.
   *
   * @param ostream a stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream ostream, SetOverrideType value)
  {
    ostream.write_long(value.value());
  }
}
