/* ObjectHelper.java --
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

import gnu.CORBA.primitiveTypeCode;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;

/**
 * The helper operations for the binding list.
 *
 * @author Audrius Meskauskas, Lithuania (AudriusA@Bioinformatics.org)
 */
public abstract class ObjectHelper
{
  static TypeCode typeCode;

  /**
   * Extract the array of object from the given {@link Any}.
   */
  public static org.omg.CORBA.Object extract(Any a)
  {
    try
      {
        return ((ObjectHolder) a.extract_Streamable()).value;
      }
    catch (ClassCastException ex)
      {
        throw new BAD_OPERATION("CORBA object expected");
      }
  }

  /**
   * Get the object repository id.
   * @return the empty string.
   */
  public static String id()
  {
    return "";
  }

  /**
   * Insert the object into the given {@link Any}.
   */
  public static void insert(Any a, org.omg.CORBA.Object object)
  {
    a.insert_Streamable(new ObjectHolder(object));
  }

  /**
   * Read the object from the given CDR input stream.
   */
  public static org.omg.CORBA.Object read(InputStream istream)
  {
    return istream.read_Object();
  }

  /**
   * Return the object type code.
   */
  public static TypeCode type()
  {
    if (typeCode == null)
      typeCode = ORB.init().get_primitive_tc(TCKind.tk_objref);
    return typeCode;
  }

  /**
   * Write the object into the given CDR output stream.
   */
  public static void write(OutputStream ostream, org.omg.CORBA.Object value)
  {
    ostream.write_Object(value);
  }
}
