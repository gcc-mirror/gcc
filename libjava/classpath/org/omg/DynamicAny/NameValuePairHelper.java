/* NameValuePairHelper.java --
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


package org.omg.DynamicAny;

import gnu.CORBA.Minor;
import gnu.CORBA.NameValuePairHolder;
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper operations for the structure {@link NameValuePair}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class NameValuePairHelper
{
  /**
   * Create the NameValuePair typecode (structure,
   * named "NameValuePair").
   * The typecode states that the structure contains the
   * following fields: id, value.
   */
  public static TypeCode type()
  {
    ORB orb = OrbRestricted.Singleton;
    StructMember[] members = new StructMember[ 2 ];

    TypeCode field;

    field =
      orb.create_alias_tc("IDL:omg.org/DynamicAny/FieldName:1.0",
                          "FieldName",
                          orb.get_primitive_tc(TCKind.tk_string)
      );
    members [ 0 ] = new StructMember("id", field, null);

    field = orb.get_primitive_tc(TCKind.tk_any);
    members [ 1 ] = new StructMember("value", field, null);
    return orb.create_struct_tc(id(), "NameValuePair", members);
  }

  /**
   * Insert the NameValuePair into the given Any.
   * This method uses the NameValuePairHolder.
   *
   * @param any the Any to insert into.
   * @param that the NameValuePair to insert.
   */
  public static void insert(Any any, NameValuePair that)
  {
    any.insert_Streamable(new NameValuePairHolder(that));
  }

  /**
   * Extract the NameValuePair from given Any.
   * This method uses the NameValuePairHolder.
   *
   * @throws BAD_OPERATION if the passed Any does not contain NameValuePair.
   */
  public static NameValuePair extract(Any any)
  {
    try
      {
        return ((NameValuePairHolder) any.extract_Streamable()).value;
      }
    catch (ClassCastException cex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("NameValuePair expected");
        bad.initCause(cex);
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Get the NameValuePair repository id.
   *
   * @return "IDL:omg.org/DynamicAny/NameValuePair:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/DynamicAny/NameValuePair:1.0";
  }

  /**
   * Read the structure from the CDR intput stream.
   *
   * @param input a org.omg.CORBA.portable stream to read from.
   */
  public static NameValuePair read(InputStream input)
  {
    NameValuePair value = new NameValuePair();
    value.id = input.read_string();
    value.value = input.read_any();
    return value;
  }

  /**
   * Write the structure to the CDR output stream.
   *
   * @param output a org.omg.CORBA.portable stream stream to write into.
   * @param value a value to write.
   */
  public static void write(OutputStream output, NameValuePair value)
  {
    output.write_string(value.id);
    output.write_any(value.value);
  }
}
