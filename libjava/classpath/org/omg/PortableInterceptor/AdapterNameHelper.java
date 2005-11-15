/* AdapterNameHelper.java --
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


package org.omg.PortableInterceptor;

import gnu.CORBA.OrbRestricted;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.StringSeqHelper;
import org.omg.CORBA.StringSeqHolder;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * Provides static helper methods for working with the adapter name helper.
 * The adapter name helper is an array of strings, so {@link StringSeqHelper}
 * could be used for io operations. The separate helper is provided anyway.
 * 
 * @since 1.5
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public abstract class AdapterNameHelper
{
  
  /**
   * The cached typecode, computed once.
   */
  static TypeCode typecode;
  
  /**
   * Extract the adapter name (<code>String[]</code>) from the given {@link Any}.
   *
   * @param a an Any to extract the array from.
   *
   * @return the extracted array.
   */
  public static String[] extract(Any a)
  {
    StringSeqHolder h = (StringSeqHolder) a.extract_Streamable();
    return h.value;
  }

  /**
   * Returns the agreed Id.
   * 
   * @return "IDL:omg.org/PortableInterceptor/AdapterName:1.0", always. 
   */
  public static String id()
  {
    return "IDL:omg.org/PortableInterceptor/AdapterName:1.0";
  }

  /**
   * Insert into the given adapter name (<code>String[]</code>) into the
   * given {@link Any}. 
   *
   * @param into the target Any.
   * @param that the array to insert.
   */
  public static void insert(Any into, String[] that)
  {
    StringSeqHolder holder = new StringSeqHolder(that);
    into.insert_Streamable(holder);
    into.type(type());
  }

  /**
   * Reads the <code>String[]</code> from the CORBA input stream.
   *
   * @param input the CORBA stream to read from.
   * @return the value from the stream.
   */
  public static String[] read(InputStream input)
  {
    return StringSeqHelper.read(input);
  }

  /**
   * Creates and returns a new instance of the TypeCode,
   * corresponding the adapter name.
   * 
   * @return the alias of the string sequence, named "AdapterName".
   */
  public static TypeCode type()
  {
    if (typecode == null)
      {
        ORB orb = OrbRestricted.Singleton;
        
        TypeCode component = orb.create_string_tc(0);
        typecode = orb.create_alias_tc(id(), "AdapterName", component);
      }
    return typecode;
  }

  /**
   * Writes the <code>String[]</code> into the given stream.
   *
   * @param output the CORBA output stream to write.
   * @param value the value that must be written.
   */
  public static void write(OutputStream output, String[] value)
  {
    StringSeqHelper.write(output, value);
  }
}
