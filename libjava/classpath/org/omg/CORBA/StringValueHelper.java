/* StringValueHelper.java --
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

import gnu.CORBA.Minor;
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.portable.BoxedValueHelper;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

import java.io.Serializable;

/**
 * Provides helper operations for the String value type, treating a
 * String as a CORBA value type rather than as a primitive type. The OMG
 * specification states this may be convenient in some specific
 * cases. The typecode is different, but the reading/writing format in
 * this implementation is the same as for the ordinary string. This is
 * that Sun's IDL compiler (v1.4) would generate.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class StringValueHelper
  implements BoxedValueHelper
{
  /**
   * The String value helper repository Id.
   */
  private static final String id = "IDL:omg.org/CORBA/StringValue:1.0";

  /**
   * The String typecode.
   */
  private static final TypeCode tString =
    OrbRestricted.Singleton.create_string_tc(0);

  /**
   * Returns the String Value repository Id.
   * @return "IDL:omg.org/CORBA/StringValue:1.0", always.
   */
  public String get_id()
  {
    return id;
  }

  /**
   * Returns the String Value repository Id.
   * @return "IDL:omg.org/CORBA/StringValue:1.0", always.
   */
  public static String id()
  {
    return id;
  }

  /**
   * Read the string value from the input stream.
   *
   * @param istream a stream to read from.
   *
   * @return a string (delegates to read_string()).
   */
  public Serializable read_value(InputStream istream)
  {
    return istream.read_string();
  }

  /**
   * Write the given string value into the output stream.
   *
   * @param ostream a stream to write into.
   * @param a_string a string to write.
   */
  public void write_value(OutputStream ostream, Serializable a_string)
  {
    try
      {
        ostream.write_string((String) a_string);
      }
    catch (ClassCastException ex)
      {
        MARSHAL m = new MARSHAL("String expected");
        m.minor = Minor.ClassCast;
        throw m;
      }
  }

  /**
   * Extract the string from the given Any. The operation
   * requires Any to hold a String value and not a String.
   *
   * @param an_any an Any to extract from.
   *
   * @return the extracted string.
   */
  public static String extract(Any an_any)
  {
    if (an_any.type().equal(type()))
      {
        an_any.type(tString);
        return an_any.extract_string();
      }
    else
      {
        BAD_OPERATION bad = new BAD_OPERATION("String value type expected");
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Insert the string into the given Any. After the operation,
   * the Any will have a String Value typecode and not a
   * String typecode.
   *
   * @param an_any an Any to insert into.
   *
   * @param that a string to insert.
   */
  public static void insert(Any an_any, String that)
  {
    an_any.insert_string(that);
    an_any.type(type());
  }

  /**
   * Reads a string as a value type.
   *
   * @param in a stream to read value from.
   */
  public static String read(InputStream in)
  {
    return in.read_string();
  }

  /**
   * Create and return the value box typecode, named "StringValue", with the
   * content typecode being unbounded string.
   */
  public static TypeCode type()
  {
    ORB orb = OrbRestricted.Singleton;
    return orb.create_value_box_tc(id(), "StringValue", tString);
  }

  /**
   * Writes a string as a value type.
   *
   * @param out a stream to write value into.
   *
   * @param a_string a string to write.
   */
  public static void write(OutputStream out, String a_string)
  {
    out.write_string(a_string);
  }
}
