/* FieldNameHelper.java --
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

import gnu.CORBA.Restricted_ORB;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * A helper for the FieldName.  The fields {@link NameValuePair#id},
 * {@link NameDynAnyPair#id} and return values of methods
 * <code>current_member_name()</code>, <code>member_name()</code> in several
 * interfaces officially have the "FieldName" type. This type is directly
 * mapped into java String and needs no helper. The helper
 * is included only as a part of the formal standard.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class FieldNameHelper
{
  /**
   * Insert the FieldName into Any (uses {@link Any.insert_string}).
   *
   * @param a the Any to insert into.
   * @param that the string to insert.
   */
  public static void insert(Any a, String that)
  {
    a.insert_string(that);
  }

  /**
   * Extract the FieldName from Any ((uses {@link Any.extract_string}).
   *
   * @param a the Any to extract from.
   */
  public static String extract(Any a)
  {
    return a.extract_string();
  }

  /**
   * Return an alias typecode.
   */
  public static TypeCode type()
  {
    ORB orb = Restricted_ORB.Singleton;
    return orb.create_alias_tc(id(), "FieldName", orb.create_string_tc(0));
  }

  /**
   * Return the FieldName repository id.
   * @return "IDL:omg.org/DynamicAny/FieldName:1.0", always.
   */
  public static String id()
  {
    return "IDL:omg.org/DynamicAny/FieldName:1.0";
  }

  /**
   * Calls {@link InputStream#read_string()}.
   *
   * @param instream the stream to read from.
   */
  public static String read(InputStream istream)
  {
    return istream.read_string();
  }

  /**
   * Calls {@link OutputStream#write_string()}.
   *
   * @param ostream the stream to write into.
   * @param value the string (FieldName) value to write.
   */
  public static void write(OutputStream ostream, String value)
  {
    ostream.write_string(value);
  }
}