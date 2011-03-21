/* NameHelper.java --
   Copyright (C) 2005, 2006 Free Software Foundation, Inc.

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


package org.omg.CosNaming;

import gnu.CORBA.Minor;
import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the name that is defined as an array
 * of the name components. There is no java class, directly matching
 * the 'Name' IDL type.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class NameHelper
{
  /**
   * The repository id of the Name.
   */
  private static String _id = "IDL:omg.org/CosNaming/Name:1.0";

  /**
   * Extract the Name the given {@link Any}.
   */
  public static NameComponent[] extract(Any a)
  {
    try
      {
        return ((NameHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Name expected");
        bad.initCause(ex);
        bad.minor = Minor.Any;
        throw bad;
      }
  }

  /**
   * Get the Name repository Id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Extract the Name from the given {@link Any}.
   */
  public static void insert(Any a, NameComponent[] that)
  {
    a.insert_Streamable(new NameHolder(that));
  }

  /**
   * Read the Name from the given CDR input stream.
   */
  public static NameComponent[] read(InputStream istream)
  {
    NameComponent[] value = null;
    int l = istream.read_long();
    value = new NameComponent[ l ];
    for (int i = 0; i < value.length; ++i)
      value [ i ] = NameComponentHelper.read(istream);
    return value;
  }

  /**
   * Get the type code of the Name.
   */
  public static TypeCode type()
  {
    TypeCode typeCode;
    typeCode = NameComponentHelper.type();
    typeCode = OrbRestricted.Singleton.create_sequence_tc(0, typeCode);
    typeCode =
      OrbRestricted.Singleton.create_alias_tc(NameHelper.id(), "Name", typeCode);
    return typeCode;
  }

  /**
   * Write the Name into the CDR output stream.
   */
  public static void write(OutputStream ostream, NameComponent[] value)
  {
    ostream.write_long(value.length);
    for (int i = 0; i < value.length; ++i)
      NameComponentHelper.write(ostream, value [ i ]);
  }
}
