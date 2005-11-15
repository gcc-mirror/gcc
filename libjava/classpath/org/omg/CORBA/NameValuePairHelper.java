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


package org.omg.CORBA;

import gnu.CORBA.DynAn.NameValuePairHolder;
import gnu.CORBA.typecodes.AliasTypeCode;
import gnu.CORBA.typecodes.PrimitiveTypeCode;
import gnu.CORBA.typecodes.StringTypeCode;
import gnu.CORBA.Minor;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for {@link NameValuePair}.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class NameValuePairHelper
{
  /**
   * The {@link NameValuePair} repository id,
   * 'IDL:omg.org/CORBA/NameValuePair:1.0'.
   */
  private static String _id = "IDL:omg.org/CORBA/NameValuePair:1.0";

  /**
   * The cached type code value.
   */
  private static TypeCode typeCode;

  /**
   * Extract the NameValuePair from the given {@link Any}.
   */
  public static NameValuePair extract(Any a)
  {
    try
      {
        return ((NameValuePairHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("NameValuePair expected");
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Return the NameValuePair repository id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the NameValuePair into the given {@link Any}.
   */
  public static void insert(Any a, NameValuePair that)
  {
    a.insert_Streamable(new NameValuePairHolder(that));
  }

  /**
   * Read the NameValuePair from the given CDR stream. First reads the
   * name (id) as string, then the value as {@link Any}.
   */
  public static NameValuePair read(InputStream istream)
  {
    NameValuePair p = new NameValuePair();
    p.id = istream.read_string();
    p.value = istream.read_any();
    return p;
  }

  /**
   * Create the type code for the name value pair.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      {
        StructMember[] members = new StructMember[ 2 ];

        TypeCode t_id =
          new AliasTypeCode(new StringTypeCode(TCKind.tk_string), "", "id");

        members [ 0 ] = new StructMember("id", t_id, null);

        members [ 1 ] =
          new StructMember("value", new PrimitiveTypeCode(TCKind.tk_any), null);

        typeCode = ORB.init().create_struct_tc(id(), "NameValuePair", members);
      }
    return typeCode;
  }

  /**
   * Write the exception to the CDR output stream. First writes the
   * name (id), then the value as {@link Any}.
   */
  public static void write(OutputStream ostream, NameValuePair value)
  {
    ostream.write_string(value.id);
    ostream.write_any(value.value);
  }
}
