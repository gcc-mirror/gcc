/* BindingListHelper.java --
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


package org.omg.CosNaming;

import org.omg.CORBA.Any;
import org.omg.CORBA.BAD_OPERATION;
import org.omg.CORBA.ORB;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the binding list.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class BindingListHelper
{
  /**
   * The binding list repository id.
   */
  private static String _id = "IDL:omg.org/CosNaming/BindingList:1.0";

  /**
   * The cached binding list type code.
   */
  private static TypeCode typeCode;

  /**
   * Extract the array of bindings from the given {@link Any}.
   */
  public static Binding[] extract(Any a)
  {
    try
      {
        return ((BindingListHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        BAD_OPERATION bad = new BAD_OPERATION("Binding list expected");
        bad.initCause(ex);
        throw bad;
      }
  }

  /**
   * Get the binding list repository id.
   */
  public static String id()
  {
    return _id;
  }

  /**
   * Insert the array of bindings into the given {@link Any}.
   */
  public static void insert(Any a, Binding[] bindings)
  {
    a.insert_Streamable(new BindingListHolder(bindings));
  }

  /**
   * Read the array of bindings from the given CDR input stream.
   */
  public static Binding[] read(InputStream istream)
  {
    Binding[] value = new Binding[ istream.read_long() ];

    for (int i = 0; i < value.length; i++)
      value [ i ] = BindingHelper.read(istream);
    return value;
  }

  /**
   * Return the binding array type code.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      {
        ORB orb = ORB.init();
        typeCode = BindingHelper.type();
        typeCode = orb.create_sequence_tc(0, typeCode);
        typeCode = orb.create_alias_tc(id(), "BindingList", typeCode);
      }
    return typeCode;
  }

  /**
   * Write the array of bindings into the given CDR output stream.
   */
  public static void write(OutputStream ostream, Binding[] value)
  {
    ostream.write_long(value.length);
    for (int i = 0; i < value.length; i++)
      org.omg.CosNaming.BindingHelper.write(ostream, value [ i ]);
  }
}
